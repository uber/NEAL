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

open Lexing
open Neal.Rule

let loaded_rule = ref None

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let _parser = ref Rule_parser.prog
let set_parser parser_ = _parser := parser_

let parse_with_error lexbuf =
  try !_parser Rule_lexer.read lexbuf with
  | Neal.Rule.SyntaxError msg ->
    Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    exit (-1)
  | Rule_parser.Error ->
    Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let _cache_enabled = ref true
let disable_cache() = _cache_enabled := false
let enable_cache() = _cache_enabled := true

let _rule_cache = Hashtbl.create 7
let fetch_from_cache filename =
  if !_cache_enabled then
    Hashtbl.find _rule_cache filename
  else raise Not_found

let cache filename ruleset =
  if !_cache_enabled then Hashtbl.add _rule_cache filename ruleset;
  ruleset

let _resolve_imports = ref true
let disable_imports() = _resolve_imports := false
let enable_imports() = _resolve_imports := true

let merge_assoc list key value tests =
  let rec aux = function
    | [] -> [(key, value, tests)]
    | (key', value', tests)::rest when key=key' ->
        (key, value@value', tests)::rest
    | x::xs -> x::(aux xs)
  in aux list

let merge filter (Ruleset imported) (Ruleset ruleset) =
  let rec aux ruleset (provider, rules, tests) =
    let filtered = match filter with
    | `All -> rules
    | `Rules rs ->
        let aux (Rule (name, _, _)) =
          List.mem name rs
        in List.filter aux rules
    in
    merge_assoc ruleset provider filtered tests
  in Ruleset (List.fold_left aux ruleset imported)

let rec resolve_imports filename (imports, ruleset) =
  if !_resolve_imports then
    let dirname = Filename.dirname filename in
    List.fold_left (resolve_import dirname) ruleset imports
  else
    ruleset

and resolve_import dirname ruleset { rules; file } =
  let path = Fs.join_path dirname file in
  let imported = load path in
  merge rules imported ruleset

and load filename =
  try fetch_from_cache filename
  with Not_found ->
    let inx = open_in filename in
    let lexbuf = Lexing.from_channel inx in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    let absyn = parse_with_error lexbuf in
    close_in inx;
    check_cycle filename;
    visiting filename;
    let ruleset = resolve_imports filename absyn in
    done_visiting filename;
    cache filename ruleset

and _visiting_list = ref []
and visiting filename =
  _visiting_list := filename :: !_visiting_list

and done_visiting filename =
  _visiting_list := List.filter (fun f -> f = filename) !_visiting_list

and check_cycle filename =
  if List.mem filename !_visiting_list then begin
    visiting filename;
    Printf.fprintf stderr "Error: Cycle found in rule imports:\n\t %s\n"
      @@ String.concat " -> " (List.rev !_visiting_list);
    flush stderr;
    exit 1
  end
