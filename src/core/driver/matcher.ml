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

open Utils
open Neal

let is_matcher = function
  | Rule.Matcher _ -> true
  | _ -> false

let get_condition acc = function
  | Rule.Condition cond -> cond :: acc
  | _ -> acc

let get_var acc = function
  | Rule.Var var -> var :: acc
  | _ -> acc

let mk_rule name path rules = function
  | Rule.Matcher matcher ->
      Rule.Set.add (Rule.Rule (name, path, matcher)) rules
  | _ -> failwith "internal error"

let matcher ctx =
  let Rule.Rule (name, path, matcher) as rule = ctx.Ctx.rule in

  if matcher.Rule.node = ctx.Ctx.kind
  && Evaluator.eval_predicate ctx matcher.Rule.predicate
  then begin
      debug ~depth:ctx.Ctx.depth ~sep:"* " ("Rule `" ^ name ^ "` matched");

      Evaluator.eval_actions ctx matcher.Rule.body;

      let rules = List.filter is_matcher matcher.Rule.body
        |> List.fold_left (mk_rule name path) Rule.Set.empty
      in
      let conditions = List.fold_left get_condition [] matcher.Rule.body in
      let vars = List.fold_left get_var [] matcher.Rule.body
        |> List.map (fun (k, v) -> (k, ref v))
      in
      (rules, conditions, vars)
  end
  else (Rule.Set.empty, [], [])
