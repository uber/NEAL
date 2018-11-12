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
  open Skip_absyn
%}

%token AND
%token COMMA
%token ALL
%token RULES
%token ON
%token THIS
%token NEXT
%token PREV
%token LINE
%token FILE
%token SKIP
%token RUNONLY
%token COLON
%token NEAL
%token THE
%token EOF

%token <string> ID
%token <string> BECAUSE

%start <Skip_absyn.action> parse

%%

%inline rule: ID { $1 }

%inline rule_and: rule AND rule { $1 :: $3 :: [] }

%inline rule_comma: rule COMMA rule_and { $1 :: $3}

%inline rules:
  | rule_comma { $1 }
  | rule_and { $1 }
  | rule { [$1] }

pattern:
  | ALL RULES { AllRules }
  | rules { SomeRules $1 }

%inline line_context_:
  | THIS { This }
  | THE? NEXT { Next }
  | THE? PREV { Prev }

line_context: line_context_ LINE { Local $1 }

global_context: THIS FILE { Global }

context: line_context | global_context { $1 }

parse:
  | NEAL COLON SKIP pattern ON context BECAUSE EOF { Skip ($4, $6, $7) }
  | NEAL COLON RUNONLY pattern ON context BECAUSE EOF { RunOnly ($4, $6, $7) }
