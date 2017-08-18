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

open Neal.Ctx
open Skip_absyn

let regex = Str.regexp "swiftlint:disable\\(:next\\)? +.*?custom_rules"

let parse directives line str =
  let _ = Str.search_forward regex str 0 in
  let scope =
    try Str.matched_group 1 str; Local Next
    with Not_found -> Global
  in (line, Skip (AllRules, scope, "")) :: directives

let compute_directives { source } =
  let lines = String.split_on_char '\n' source in
  let rec aux line matches = function
    | [] -> matches
    | l::ls ->
        let matches' =
          try parse matches line l
          with Not_found -> matches
        in aux (line + 1) matches' ls
  in aux 1 [] lines
