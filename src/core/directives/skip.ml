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
open Neal.Ctx open Skip_absyn

let neal_marker = Str.regexp "NEAL:[^\n]+"

let print_position line index outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    line (pos.pos_cnum - pos.pos_bol + 1 + index)

let parse line index lexbuf =
  try
    Some (Skip_parser.parse Skip_lexer.read lexbuf)
  with _ ->
    Printf.fprintf stderr "%a: syntax error on NEAL comment\n" (print_position line index) lexbuf;
    None

let parse directives file line index matched =
  let lexbuf = Lexing.from_string matched in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file };
  match parse line index lexbuf with
    | Some directive -> (line, directive) :: directives
    | None -> directives

let compute_directives { file; source } =
  let lines = String.split_on_char '\n' source in
  let rec aux line matches = function
    | [] -> matches
    | l::ls ->
        let matches' =
          try
            let index = Str.search_forward neal_marker l 0 in
            let matched = (Str.matched_string l) in
            parse matches file line index matched
        with Not_found -> matches
        in aux (line + 1) matches' ls
  in aux 1 [] lines
