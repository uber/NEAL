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

let to_string = function
  | `String s -> s
  | _ -> raise Type_error

let to_int = function
  | `Int i -> i
  | _ -> raise Type_error

let get_kind props =
  List.assoc "_type" props
  |> to_string
  |> String.capitalize_ascii

let get_loc props =
  try
    Absyn.Pos (List.assoc "lineno" props |> to_int, List.assoc "col_offset" props |> to_int)
  with Not_found ->
    Absyn.Pos (0, 0)

let rec get_props props =
  List.fold_left (fun ps (k,v) ->
    match k with
    | "lineno" | "col_offset" | "_type" | "ctx" -> ps
    | k -> (String.capitalize_ascii k, absyn_of_json v) :: ps
  ) [] props

and absyn_of_json = function
  | `Assoc props -> Absyn.Node (get_kind props, get_loc props, get_props props)
  | `Int i -> Absyn.Int i
  | `String s -> Absyn.String s
  | `Bool b -> Absyn.Bool b
  | `Null -> Absyn.Null | `List l -> Absyn.List (List.map absyn_of_json l)
  | _ -> raise Type_error

let exec_name = Neal.Utils.relative_path "providers/helpers/dump_python_ast.py"

let parse _ source =
  let stdout, stdin = Unix.open_process exec_name in
  output_string stdin source;
  close_out stdin;
  let json = Yojson.Safe.from_channel stdout in
  close_in stdout;
  let _ = Unix.close_process (stdout, stdin) in
  absyn_of_json json

let () = Provider.register(module struct
  let name = "Python"
  let extensions = [".py"]
  let parse = parse
  let exported_macros = []
  let exported_actions = []
end)
