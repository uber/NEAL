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

module StringSet = Set.Make(String)

type scope_config = {
  includes: Str.regexp list;
  ignores: Str.regexp list;
}

type scope = [ `Global | `Scoped of scope_config ]

type provider = (module Neal.Provider.PROVIDER)
type reporter = (module Neal.Reporter.REPORTER)

type config = {
  print_ast: [ `No | `Json | `Pretty ];
  strict: bool;
  strict_parse: bool;
  rules_paths: string list;
  provider_paths: string list;
  reporter_paths: string list;
  files: string list;
  providers: (module Neal.Provider.PROVIDER) list;
  reporters: (module Neal.Reporter.REPORTER) list;
  rules: (scope * Neal.Rule.ruleset list) list;
  provider_maps: provider_map list;
}

and provider_map = {
  path: string;
  maps: (Str.regexp * provider_name) list;
}

and provider_name = string

let print_list p outx l =
  List.map p l
  |> String.concat "; "
  |> Printf.fprintf outx "[ %s ]"

let id x = x

let print_ast_to_string = function
  | `No -> "`No"
  | `Json -> "`Json"
  | `Pretty -> "`Pretty"

let print_config (config : config) : unit =
  Printf.printf "Config {\n";
  Printf.printf "\tprint_ast = %s;\n" (print_ast_to_string config.print_ast);
  Printf.printf "\tstrict = %B;\n" config.strict;
  Printf.printf "\tstrict_parse = %B;\n" config.strict_parse;
  Printf.printf "\trules_paths = %a;\n" (print_list id) config.rules_paths;
  Printf.printf "\tprovider_paths = %a;\n" (print_list id) config.provider_paths;
  Printf.printf "\treporter_paths = %a;\n" (print_list id) config.reporter_paths;
  Printf.printf "\tfiles = %a;\n" (print_list id) config.files;
  Printf.printf "}\n"

let default_config = {
  print_ast = `No;
  strict = false;
  strict_parse = false;
  rules_paths = [];
  provider_paths = [];
  reporter_paths = [];
  files = [];
  providers = [];
  reporters = [];
  rules = [];
  provider_maps = [];
}

(**
 * Configuration Steps
 *
 * Rule Scanning
 * Provider Scanning
 * Reporter Scanning
 * Configuration Scanning
 * File Scanning
 *)
type configuration_step = config -> config

(**
 * Actually analyse the files
 *)
type process_step = config -> unit

(**
 * Filter Steps
 *
 * NEAL
 * SwiftLint
 *)
type filter_step = Neal.Reporter.violation list -> Neal.Reporter.violation list

module type PIPELINE = sig
  val configuration_steps : configuration_step list
  val process_step : process_step
  val filter_steps : filter_step list
end

module Make(M : PIPELINE) = struct
  let apply_config_step config step =
    step config

  let apply_filter violations filter =
    filter violations

  let report_violation violation =
    let (severity, ctx, fix) = violation in
    let commit_reporter (module R : Neal.Reporter.REPORTER) =
      let stat = match severity with
        | Neal.Reporter.Error -> Stats.Reported_error
        | Neal.Reporter.Warning -> Stats.Reported_warning
        | Neal.Reporter.Other -> Stats.Reported_other
      in
      Stats.add_stat ctx.Neal.Ctx.file stat;
      R.report severity ctx fix
    in
    List.iter commit_reporter ctx.Neal.Ctx.reporters

  let run initial_config =
    let config = List.fold_left apply_config_step initial_config M.configuration_steps in
    M.process_step config;
    let violations = Neal.Reporter.get_violations () in
    let violations' = List.fold_left apply_filter violations M.filter_steps in

    List.iter report_violation violations';
    if Utils.env "GC_STATS" then Gc.print_stat stderr;
    Stats.finish config.strict config.strict_parse
end
