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

let take n list =
  let rec aux n l = function
    | [] -> List.rev l
    | _ when n = 0 -> List.rev l
    | x::xs -> aux (n-1) (x::l) xs
  in aux n [] list

let print_source'' line lineno pre_ctx pos_ctx error_off =
  let start = lineno - List.length pre_ctx in
  let limit = lineno + List.length pos_ctx in
  let width = String.length (string_of_int limit) in

  let print_marker mark =
    print_string " ";
    for i = String.length mark to width do
      print_char ' ';
    done;
    print_string mark;
    print_string " | "
  in

  let print_line lineno line =
    print_marker (string_of_int lineno);
    print_endline line
  in

  let print_indicator() =
    print_marker "~";
    for i = 2 to error_off do
      print_char ' ';
    done;
    print_endline "^";
  in

  print_newline();
  List.iteri (fun i line -> print_line (start + i) line) (List.rev pre_ctx);
  print_line lineno line;
  print_indicator();
  List.iteri (fun i line -> print_line (lineno + 1 + i) line) pos_ctx;
  print_newline()

let print_source_off source offset =
  let ctx_length = 2 in
  let lines = Str.split (Str.regexp "\n") source in
  let rec aux ctx off lineno lines =
    match lines with
    | [] -> assert false
    | line::lines' ->
        let off' = String.length line + off + 1 in
        if off' >= offset then
          print_source'' line lineno ctx (take ctx_length lines') (offset - off)
        else
          let ctx' = take ctx_length (line :: ctx) in
          aux ctx' off' (lineno + 1) lines'
  in aux [] 0 1 lines

let print_source_pos source (err_line, err_column) =
  let ctx_length = 2 in
  let lines = Str.split (Str.regexp "\n") source in
  let rec aux ctx lineno lines =
    match lines with
    | [] -> assert false
    | line::lines' ->
        if lineno = err_line then
          print_source'' line lineno ctx (take ctx_length lines') err_column
        else
          let ctx' = take ctx_length (line :: ctx) in
          aux ctx' (lineno + 1) lines'
  in aux [] 1 lines

let print_source ctx =
  match ctx.Ctx.loc with
  | None -> ()
  | Some l ->
      match l with
      | Absyn.Off off -> print_source_off ctx.Ctx.source off
      | Absyn.Pos (l,c) -> print_source_pos ctx.Ctx.source (l,c)
