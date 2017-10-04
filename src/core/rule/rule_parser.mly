// Copyright (c) 2017 Uber Technologies, Inc.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

%{

module Rule = Neal.Rule

let count = ref 0
let uid() = incr count; !count

(* Returns one ruleset of the form *)
(* [(provider, rules, [])] *)
let flip rules =
  let update assoc key value =
    let rec aux = function
      | [] -> [(key,[value],[])]
      | (key', values, tests)::rest when key=key' ->
          (key,value::values, tests)::rest
      | x::xs -> x::(aux xs)
    in aux assoc
  in
  let rec aux acc = function
    | [] -> acc
    | (rule_name, file, provider_matchers, _)::xs ->
        let rec aux2 acc = function
          | [] -> acc
          | (provider,matcher)::xs ->
              let acc' = update acc provider (Rule.Rule(rule_name, file, matcher)) in
              aux2 acc' xs
        in
        let acc' = aux2 acc provider_matchers in
        aux acc' xs
  in Rule.Ruleset(aux [] rules)

(* returns one ruleset of the form *)
(* [(rule_name, matchers, tests)] *)
let mk_ruleset rules =
  let mk_rule filename (provider, matcher) =
    Rule.Rule (provider, filename, matcher)
  in

  let rule (name, filename, matchers, tests) =
    let rules = List.map (mk_rule filename) matchers in
    (name, rules, tests)
  in

  Rule.Ruleset(List.map rule rules)

%}

%token RULE
%token WHERE
%token TESTS
%token EXPECT_OK
%token EXPECT_WARN
%token EXPECT_FAIL
%token FROM_FILE
%token IMPORT
%token FROM
%token VAR
%token CONDITION

%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token COLON
%token COLONCOLON
%token COMMA
%token DOT
%token EOF
%token STAR
%token ASGN
%token GT
%token GTGT

%token AND
%token OR

%token <string> IDENTIFIER
%token <string> STRING
%token <string> OP
%token <bool> BOOL
%token <int> INT
%token <Str.regexp> REGEXP

%start prog
%start prog_non_flip
%type <Neal.Rule.file> prog
%type <Neal.Rule.file> prog_non_flip

%left AND
%left OR
%left OP

%%

prog: import* rule* EOF { ($1, flip $2) }

prog_non_flip: import* rule* EOF { ($1, mk_ruleset $2) }

import: IMPORT import_list FROM STRING {
  { Rule.rules = $2; Rule.file = $4 }
}

import_list:
  | STAR { `All }
  | LBRACE separated_nonempty_list(COMMA, IDENTIFIER) RBRACE { `Rules $2 }

rule: RULE IDENTIFIER LBRACE provider_matcher+ loption(tests) RBRACE {
  let file = $startpos.Lexing.pos_fname in
  ($2, file, $4, $5)
}

provider_matcher: IDENTIFIER COLONCOLON matcher {
  ($1, ($3 []))
}

scoped_matcher:
  | IDENTIFIER GT matcher {
    fun ctx ->
      { ($3 ctx) with Rule.scope = Rule.Child $1  }
  }
  | IDENTIFIER GTGT matcher {
    fun ctx ->
      { ($3 ctx) with Rule.scope = Rule.Descendant $1  }
  }
  | matcher { $1 }

matcher: IDENTIFIER where? stmts {
  fun ctx ->
    let predicate = match $2 with
    | None -> None
    | Some f -> Some (f ctx)
    in {
      Rule.scope = Rule.Unscoped;
      Rule.node=$1;
      Rule.predicate;
      Rule.body=($3 ctx);
      Rule._uid = uid()
    }
}

stmts: delimited(LBRACE, stmt+, RBRACE) {
  fun ctx ->
    let aux (stmts, ctx) fn =
      let stmt, ctx' = fn ctx in
      (stmt::stmts, ctx')
    in
    List.fold_left aux ([], ctx) $1
    |> fst
    |> List.rev
}

where: WHERE exp { $2 }

stmt:
  | VAR IDENTIFIER ASGN BOOL {
    fun ctx ->
      let var = ($2, uid()) in
      Rule.Var (var, $4), ($2, var) :: ctx
  }
  | IDENTIFIER ASGN BOOL {
    fun ctx ->
      try Rule.Assignment (List.assoc $1 ctx, $3), ctx
      with Not_found ->
        raise (Neal.Rule.SyntaxError ("Unknown conditional variable: " ^ $1))
  }
  | call { fun ctx -> Rule.Action($1 ctx), ctx }
  | scoped_matcher { fun ctx -> Rule.Matcher($1 ctx), ctx }
  | condition { fun ctx -> ($1 ctx), ctx }

call: IDENTIFIER LPAREN separated_list(COMMA, exp) RPAREN {
  fun ctx -> ($1, List.map (fun f -> f ctx) $3)
}

condition: CONDITION delimited(LPAREN, exp, RPAREN) stmts {
  fun ctx -> Condition ($2 ctx, $3 ctx)
}

exp:
  | exp op exp { fun ctx -> Rule.Binop($1 ctx, $2, $3 ctx) }
  | value { fun ctx -> Value ($1 ctx) }
  | call { fun ctx -> Rule.Call($1 ctx) }

%inline op:
  | AND { "&&" }
  | OR { "||" }
  | OP { $1 }

value:
  | literal { fun ctx -> Rule.Literal($1 ctx) }
  | LPAREN exp RPAREN { fun ctx -> Rule.Exp($2 ctx) }

literal:
  | INT { fun _ -> `Int($1) }
  | STRING { fun _ -> `String($1) }
  | IDENTIFIER {
    fun ctx ->
      try `ConditionalVar (List.assoc $1 ctx)
      with Not_found -> `Id($1)
  }
  | BOOL { fun _ -> `Bool($1) }
  | REGEXP { fun _ -> `RegExp($1) }
  | IDENTIFIER DOT separated_nonempty_list(DOT, IDENTIFIER) { fun _ -> `PropAccess($1, $3) }

tests: TESTS LBRACE nonempty_list(test) RBRACE { $3 }

test: IDENTIFIER COLON expectation { let (a,b,c) = $3 in $1, a, b, c }

expectation:
  | EXPECT_OK LPAREN test_input RPAREN { `Ok, $3, None }
  | EXPECT_WARN LPAREN test_input expect_message? RPAREN { `Warn, $3, $4 }
  | EXPECT_FAIL LPAREN test_input expect_message? RPAREN { `Fail, $3, $4 }

test_input:
  | STRING { `String $1 }
  | FROM_FILE LPAREN STRING RPAREN { `File $3 }

expect_message: COMMA STRING { $2 }
