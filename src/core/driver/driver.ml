(* Copyright (c) 2017 Uber Technologies, Inc. *)
(* *)
(* Permission is hereby granted, free of charge, to any person obtaining a copy *)
(* of this software and associated documentation files (the "Software"), to deal *)
(* in the Software without restriction, including without limitation the rights *)
(* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell *)
(* copies of the Software, and to permit persons to whom the Software is *)
(* furnished to do so, subject to the following conditions: *)
(* *)
(* The above copyright notice and this permission notice shall be included in *)
(* all copies or substantial portions of the Software. *)
(* *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE *)
(* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, *)
(* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN *)
(* THE SOFTWARE. *)

open Config
open Fs
open Pipeline
open Utils

module Absyn = Neal.Absyn
module Ctx = Neal.Ctx
module Provider = Neal.Provider
module Reporter = Neal.Reporter
module Rule = Neal.Rule

module RuleSet = Neal.Rule.Set

exception Done of string * Stats.stat

let cwd = Sys.getcwd()

exception Found of (module Provider.PROVIDER)

let select_provider config file ext providers =
  let match_extension ext (module P: Provider.PROVIDER) =
    List.exists ((=) ext) P.extensions
  in

  let match_name name (module P: Provider.PROVIDER) =
    String.equal name P.name
  in

  let find_maps file maps =
    let aux (regex, provider) =
      try
        ignore (Str.search_forward regex file 0);
        raise (Found (List.find (match_name provider) providers))
      with Not_found -> ()
    in
    List.iter aux maps
  in

  let match_map { path; maps } =
    let pathLen = String.length path in
    let fileLen = String.length file in
    if pathLen > fileLen
    then ()
    else
      let sub = String.sub file 0 (String.length path) in
      if String.equal sub path then
        find_maps file maps
  in

  try
    (* Try looking into the configuration's provider maps first *)
    List.iter match_map config.provider_maps;

    (* Fallback to extension matching if nothing is found *)
    (match ext with
      | Some ext -> List.filter (match_extension ext) providers
      | None -> []
    )|> function
    | [] -> raise (Done (file, Stats.No_available_provider))
    | p :: _ -> p
  with Found p -> p

let select_rules provider rules =
  let select (Rule.Ruleset rules) =
    try
      let (_, rules, _) = List.find (fun (n, _, _) -> n = provider) rules
      in rules
    with Not_found -> []
  in
  List.map select rules
  |> List.concat
  |> RuleSet.of_list

let scope_matches { includes; ignores } file =
  let match_ file regexp =
    try
      let _ = Str.search_forward regexp file 0 in
      true
    with Not_found ->
      false
  in
  List.exists (match_ file) includes &&
  not (List.exists (match_ file) ignores)

exception Break of Neal.Rule.ruleset list
let collect_rules file rules = function
  | (`Global, rs) -> rs @ rules
  | (`Scoped scope, rs) when scope_matches scope file -> rs @ rules
  | _ -> rules

let visitor (module P : Provider.PROVIDER) reporters file source rules absyn =
  let rec visitor (nested_rules, vars) depth prop (kind, props, loc) =
    debug ~depth:depth ~sep:"| " ("looking at node " ^ kind);
    let d = depth + 1 in
    let aux rule (rules, conditions, vars) =
      let ctx = {
        Ctx.provider = (module P);
        Ctx.reporters;
        Ctx.kind;
        Ctx.prop;
        Ctx.props;
        Ctx.depth;
        Ctx.file;
        Ctx.source;
        Ctx.loc;
        Ctx.rule;
        Ctx.vars;
      } in
      let (rules', conditions', vars') = Matcher.matcher ctx in
      (Rule.Set.union rules rules', (ctx, conditions') :: conditions, vars' @ vars)
    in
    let (nested_rules', conditions, vars') = RuleSet.fold aux nested_rules (RuleSet.empty, [], vars) in
    let rec callback = fun visitor (prop, node) ->
      match node with
      | Absyn.List nodes ->
          List.iter (fun node -> callback visitor (prop, node)) nodes
      | Absyn.Node(kind, loc, props) ->
          visitor prop (kind, props, Some loc)
      | _ -> ()
    in
    List.iter (callback (visitor (nested_rules', vars') d)) props;

    List.iter (fun (ctx, conditions) ->
      Evaluator.eval_conditions { ctx with Ctx.vars=vars' } conditions
    ) conditions
  in
  visitor (rules, []) (~-1) "" ("$root", [("$root",absyn)], None)

let parse (module P : Provider.PROVIDER) file source =
  try
    let absyn = P.parse file source in
    Stats.add_stat file Stats.Parsing_ok;
    absyn
  with Failure message ->
    Printf.fprintf stderr "%s\n" message;
    raise (Done (file, Stats.Parsing_fail))

let check' config file ext read_source =
  let (module P: Provider.PROVIDER) = select_provider config file ext config.providers in
  debug ("Selected provider `" ^ P.name ^ "` for file " ^ file);

  let rules' =
    begin
      try List.fold_left (collect_rules file) [] config.rules
      with Break r -> r
    end |> select_rules P.name
  in

  if RuleSet.is_empty rules' && config.print_ast = `No then
    raise (Done (file, Stats.No_rules_found));

  begin (* debug *)
    debug "Rules found:";
    RuleSet.iter (fun (Rule.Rule (name, _, _)) -> debug ("-> " ^ name)) rules';
  end; (* debug *)

  let source = read_source () in
  let absyn = parse (module P) file source in

  if config.print_ast <> `No then begin
    Prabsyn.print config.print_ast absyn;
    raise (Done (file, Stats.Parsing_ok))
  end;

  let file' = relative_to cwd file in
  visitor (module P) config.reporters file' source rules' absyn

let check config file ext read_source =
  try
    check' config file ext read_source
  with Done (file, stat) ->
    Stats.add_stat file stat

let update_progress =
  if Utils.env "NO_PROGRESS" || not (Unix.isatty Unix.stderr) then
    fun current total file -> ()
  else
    let last_progess_update = ref 0.0 in
    let esc = char_of_int 27 in
    let eraseLine = Printf.sprintf "%c[K" esc in
    let disableLineWrap = Printf.sprintf "%c[?7l" esc in
    let enableLineWrap = Printf.sprintf "%c[?7h" esc in
    fun current total file ->
      let now = Sys.time() in
      let current' = current + 1 in
      let file' = relative_to cwd file in
      if now -. (!last_progess_update) > 0.1 || current' = total then begin
        last_progess_update := now;
        Printf.fprintf stderr "%s%s[%d of %d]: Analysing %s\r%s" eraseLine disableLineWrap current' total file' enableLineWrap;
        flush stderr;
        if current' = total then
          (Printf.fprintf stderr "\n"; flush stderr)
      end

let process config =
  let total = List.length config.files in
  let check' current file =
      update_progress current total file;
      check config file (extname file) (fun () -> read file)
  in
  List.iteri check' config.files
