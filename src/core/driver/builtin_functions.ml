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

open Neal

exception Type_error

let str = function
  | `String s -> s
  | _ -> ""

let bool = function
  | `Bool b -> b
  | _ -> false

let regex = function
  | `RegExp r -> r
  | _ -> raise Type_error

let list = function
  | `Node (Absyn.List l) -> l
  | _ -> []

let mk_report report_type ctx args =
  let message = str (List.nth args 0) in
  Reporter.report report_type ctx (Reporter.Suggestion message)

let fail ctx args = mk_report Reporter.Error ctx args
let warn ctx args = mk_report Reporter.Warning ctx args

let not_ ctx args =
  `Bool (not (bool (List.nth args 0)))

let match_ ctx args =
  let target = str (List.nth args 0) in
  let regex = regex (List.nth args 1) in
  try
    let _ = Str.search_forward regex target 0 in
    `Bool true
  with Not_found ->
    `Bool false

let count ctx args =
  let lst = list (List.nth args 0) in
  `Int (List.length lst)

let exported_actions = [
  ("fail", fail);
  ("warn", warn);
]

let exported_macros = [
  ("not", not_);
  ("match", match_);
  ("count", count);
]
