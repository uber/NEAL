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

open Neal.Absyn
open Printf

let to_absyn = function
  | Off o -> Int o
  | Pos (line, column) -> String (sprintf "(%d, %d)" line column)

let print ast =
  let rec indent = function
    | 0 -> ()
    | n -> printf "  "; indent (n-1)
  in
  let rec aux d = function
    | Null -> printf "null"
    | Bool false -> printf "false"
    | Bool true -> printf "true"
    | String s -> printf "\"%s\"" s
    | Int i -> printf "%d" i
    | List l ->
        printf "[\n";
        let first = ref true in
        List.iter (fun x ->
          (if !first then
            first := false
          else
            printf ",\n");
          indent (d+1);
          aux (d+1) x
        ) l;
        if not !first then printf "\n";
        indent d;
        printf "]"
    | Node (name, loc, props) ->
        printf "%s {\n" name;
        List.iter (fun (pname,pval) ->
          indent (d+1);
          printf "%s = " pname;
          aux (d+1) pval;
          printf "\n"
        ) (("location", to_absyn loc)::props);
        indent d;
        printf "}"
  in aux 0 ast; printf "\n"
