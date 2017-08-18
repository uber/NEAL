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
module B = Builtin_functions
module P = Neal.Provider

(*NOTE: this does not belong here, when the AST type was an existential type
 * it used to live in the provider, which is a more appropriate location
 * for it. I'm not sure what the best solution is here yet, but I might go back
 * to existential types.
 *)
let literal_of_node = function
  | Absyn.Int int -> Some (`Int int)
  | Absyn.String str -> Some (`String str)

  (* Swift identifiers *)
  | Absyn.Node ("Identifier",_,("Value", Absyn.String id)::_) -> Some (`String id)

  (* Python identifiers *)
  | Absyn.Node ("Name",_,("Id", Absyn.String id)::_) -> Some (`String id)

  | Absyn.Bool b -> Some (`Bool b)
  | n -> Some (`Node n)

let rec eq v1 v2 = match v1, v2 with
| `List l, v2
| v2, `List l ->
    List.exists (fun v1 -> eq v1 v2) l

| `Node n, v2
| v2, `Node n ->
    begin match literal_of_node n with
    | None -> false
    | Some (`Node n) -> `Node n = v2
    | Some n -> eq n v2
    end

| v1, v2 -> v1 = v2

let resolve_var ctx var =
  try
    let bool = List.assoc var ctx.Ctx.vars in
    `Bool !bool
  with Not_found -> `Bool false

let rec resolve_id ctx = function
  | `Id id -> begin
    let (module P: Provider.PROVIDER) = ctx.Ctx.provider in
    try
      let node = List.assoc id ctx.Ctx.props in
      match literal_of_node node with
      | Some value -> value
      | None -> `Bool false
    with Not_found -> `Bool false
  end
  | value -> value


let eval_lit ctx = function
  | `Id _ as id -> resolve_id ctx id
  | `ConditionalVar var -> resolve_var ctx var
  | `PropAccess (prop, accesses) ->
      let aux nodes access =
        let rec aux acc node =
          match node with
          | `Node (Absyn.Node (n, _, props)) ->
              begin try `Node (List.assoc access props) :: acc
              with Not_found -> acc
              end
          | `Node (Absyn.List l) ->
              List.concat @@ List.map (fun n -> aux acc (`Node n)) l
          | _ -> acc
        in
        List.fold_left aux [] nodes
      in
      `List (List.fold_left aux [resolve_id ctx (`Id prop)] accesses)
  | lit -> lit

let rec eval_value ctx = function
  | Rule.Literal lit -> eval_lit ctx lit
  | Rule.Exp exp -> eval_exp ctx exp

and eval_call
: 'a.  Ctx.ctx -> 'a P.exports -> 'a P.exports -> string -> Rule.exp list -> 'a
= fun ctx a1 a2 callee args ->

  let args' = List.map (eval_exp ctx) args in
  let call_from fns = (List.assoc callee fns) ctx args' in

  try call_from a1
  with Not_found ->
    try call_from a2
    with Not_found ->
      failwith (Printf.sprintf "Unknown function: %s\n" callee)

and eval_exp ctx = function
  | Rule.Value v -> eval_value ctx v
  | Rule.Binop (lhs, op, rhs) -> begin
      let lhs' = eval_exp ctx lhs in
      let rhs' = eval_exp ctx rhs in
      match op with
      | "==" -> `Bool (eq lhs' rhs')
      | "!=" -> `Bool (not @@ eq lhs' rhs')
      | "||" -> `Bool (B.bool lhs' || B.bool rhs')
      | "&&" -> `Bool (B.bool lhs' && B.bool rhs')
      | _ -> failwith (Printf.sprintf "unsupported operator: %s" op)
  end
  | Rule.Call (callee, args) ->
      let (module P: Provider.PROVIDER) = ctx.Ctx.provider in
      eval_call ctx P.exported_macros B.exported_macros callee args

let eval_actions ctx stmts =
  let eval = function
    | Rule.Action (fn, args) ->
        let (module P: Provider.PROVIDER) = ctx.Ctx.provider in
        eval_call ctx P.exported_actions B.exported_actions fn args
    | Rule.Assignment (var, value) -> begin
        try
          (List.assoc var ctx.Ctx.vars) := value
        with Not_found ->
          failwith ("Unknown conditional variable: " ^ fst var)
    end
    | _ -> ()
  in List.iter eval stmts

let eval_predicate ctx = function
  | None -> true
  | Some exp ->
      match eval_exp ctx exp with
      | `Bool true -> true
      | _ -> false

let eval_conditions ctx conds =
  let aux (cond, stmts) =
    if (eval_predicate ctx (Some cond)) then
      eval_actions ctx stmts
  in
  List.iter aux conds
