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

let directives_cache = Hashtbl.create 64

let actually_compute_directives ctx =
  Skip.compute_directives ctx @ Swiftlint.compute_directives ctx

let compute_directives ({ file } as ctx) =
  try
    Hashtbl.find directives_cache file
  with Not_found ->
    let directives = actually_compute_directives ctx in
    Hashtbl.add directives_cache file directives;
    directives

let is_ignored ctx line directives =
  let Neal.Rule.Rule(rule_name, _, _) = ctx.rule in
  let direction line = function
    | This -> line
    | Prev -> line - 1
    | Next -> line + 1
  in
  let aux = fun d -> match line, d with
    | _, (_, Skip (AllRules, Global, _)) -> true
    | Some line, (l, Skip (AllRules, Local d, _)) when direction l d = line -> true
    | _, (_, Skip (SomeRules rules, Global, _)) when List.mem rule_name rules -> true
    | Some line, (l, Skip (SomeRules rules, Local d, _)) when List.mem rule_name rules && direction l d = line -> true
    | _, (_, s) -> false
  in List.exists aux directives

let get_line source = function
  | None -> None
  | Some (Neal.Absyn.Pos (line, _)) -> Some (line)
  | Some (Neal.Absyn.Off target_off) ->
    let rec aux off line = function
      | [] -> failwith "offset beyond end of file"
      | l::lines ->
          let off' = String.length l + off + 1 in
          if off' > target_off then
            Some line
          else aux off' (line + 1) lines
    in
    let lines = String.split_on_char '\n' source in
    aux 0 1 lines

let should_ignore (_, ctx, _) =
  let directives = compute_directives ctx in
  let line = get_line ctx.source ctx.loc in
  is_ignored ctx line directives

let filter violations =
  let violations' = List.filter (fun v -> not (should_ignore v)) violations in
  Hashtbl.reset directives_cache;
  violations'
