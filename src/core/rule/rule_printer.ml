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

open Printf
open Neal.Rule

let rec print_literal = function
  | `Int i ->
      printf "(`Int %d)" i
  | `String str ->
      printf "(`String \"%s\")" str
  | `Node str ->
      printf "(`Node)"
  | `Id id ->
      printf "(`Id \"%s\")" id
  | `Bool b ->
      printf "(`Bool %s)" (if b then "true" else "false")
  | `RegExp r ->
      printf "(`RegExp ?)"
  | `PropAccess (p, props) ->
      List.map (fun s -> "\"" ^ s ^ "\"") props
      |> String.concat ","
      |> printf "(`PropAccess %s[%s])" p
  | `List l ->
    printf "(`List [";
    List.iter print_literal l;
    printf "])"

and print_value = function
  | Literal literal ->
      printf "(Rule.Literal";
      print_literal literal;
      printf ")"
  | Exp exp ->
      printf "(Rule.Exp";
      print_exp exp;
      printf ")"

and print_exp = function
  | OpExp(v1,op,v2) ->
      printf "(Rule.OpExp (";
      print_value v1;
      printf ", ";
      printf "\"%s\", " op;
      print_value v2;
      printf "))";
  | CallExp call ->
      printf "(Rule.CallExp ";
      print_call call;
      printf ")";

and print_call (callee,args) =
  printf "(\"%s\", [" callee;
  List.iter (fun v -> print_value v; printf "; ") args;
  printf "])"

and print_stmt = function
  | Call call ->
      printf "(Rule.Call";
      print_call call;
      printf ")"
  | Matcher matcher ->
      printf "(Rule.Matcher";
      print_matcher matcher;
      printf ")"

and print_matcher {node;constr;body} =
  printf "{\n";
  printf "Rule.node=\"%s\";\n" node;
  printf "Rule.constr=";
  begin match constr with
  | None -> printf "None;\n"
  | Some exp ->
      printf "Some (";
      print_exp exp;
      printf ");\n";
  end;
  printf "Rule.body=[\n";
  List.iter (fun st -> print_stmt st; printf ";\n") body;
  printf "];\n";
  printf "}\n"
let print_rule (Rule(name, matcher)) =
  printf "Rule.Rule(\"%s\",\n" name;
  print_matcher matcher;
  printf ");\n"

let print_providers_rules (provider, rules) =
  printf "(\"%s\", [\n" provider;
  List.iter print_rule rules;
  printf "]);\n"

let print (Ruleset(provider_rules)) =
  printf "let () = \n";
  printf "Rule_loader.loaded_rule := Some(Rule.Ruleset [\n";
  List.iter print_providers_rules provider_rules;
  printf "])\n"
