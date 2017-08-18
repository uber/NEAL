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

type direction = This | Next | Prev
type scope = Global | Local of direction
type rules = AllRules | SomeRules of string list
type excuse = string
type skip = Skip of rules * scope * excuse
type line_number = int
type 'a directive = (line_number * 'a)

let print_rules out = function
  | AllRules -> Printf.fprintf out "all rules"
  | SomeRules rules ->
      let rec aux = function
        | a :: [] -> Printf.fprintf out "%s" a
        | a :: b :: [] -> Printf.fprintf out "%s and %s" a b
        | a :: b :: more ->
            Printf.fprintf out "%s, " a;
            aux (b::more)
        | _ -> assert false
      in aux rules

let print_direction out = function
  | This -> Printf.fprintf out "this"
  | Next -> Printf.fprintf out "the next"
  | Prev -> Printf.fprintf out "the previous"

let print_scope out = function
  | Global -> Printf.fprintf out "this file"
  | Local d -> Printf.fprintf out "%a line" print_direction d

let print_skip (Skip (rules, scope, excuse)) =
  Printf.printf "NEAL: skip %a on %a because %s\n" print_rules rules print_scope scope excuse
