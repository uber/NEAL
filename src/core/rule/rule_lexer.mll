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

{
open Rule_parser
}

rule read = parse
| "rule" { RULE }
| "where" { WHERE }
| "tests" { TESTS }
| "expect_ok" { EXPECT_OK }
| "expect_warn" { EXPECT_WARN }
| "expect_fail" { EXPECT_FAIL }
| "from_file" { FROM_FILE }
| "import" { IMPORT }
| "from" { FROM }
| "var" { VAR }
| "condition" { CONDITION }

| "{" { LBRACE }
| "}" { RBRACE }
| "(" { LPAREN }
| ")" { RPAREN }
| "::" { COLONCOLON }
| ":" { COLON }
| "," { COMMA }
| "." { DOT }
| "*" { STAR }
| ":=" { ASGN }

| ">>" { GTGT }
| ">" { GT }

| "true" { BOOL(true) }
| "false" { BOOL(false) }

| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* { IDENTIFIER(Lexing.lexeme lexbuf) }
| '"'  { read_string (Buffer.create 17) lexbuf }
| "&&" { AND }
| "||" { OR }
| ['!' '=' '<' '>' '|' '&']+ { OP(Lexing.lexeme lexbuf) }
| ['0' - '9']+ { INT(int_of_string (Lexing.lexeme lexbuf)) }

| "//" [^'\n']* { read lexbuf }
| "/*" { multiline_comment lexbuf }

| eof { EOF }

| '\n' { Lexing.new_line lexbuf; read lexbuf }
| ' ' { read lexbuf }
| '/' { regexp (Buffer.create 32) lexbuf }

| _ { raise (Neal.Rule.SyntaxError("Unknown token: '" ^ Lexing.lexeme lexbuf ^ "'")) }

and regexp buf = parse
| "/i" { REGEXP(Str.regexp_case_fold (Buffer.contents buf)) }
| "\\n" {
  Buffer.add_char buf '\n';
  regexp buf lexbuf
}
| "\\r" {
  Buffer.add_char buf '\r';
  regexp buf lexbuf
}
| "\\t" {
  Buffer.add_char buf '\t';
  regexp buf lexbuf
}
| "\\\\" {
  Buffer.add_char buf '\\';
  regexp buf lexbuf
}
| "\\/" {
  Buffer.add_char buf '/';
  regexp buf lexbuf
}
| "\\(" {
  Buffer.add_char buf '(';
  regexp buf lexbuf
}
| "(" {
  Buffer.add_string buf "\\(";
  regexp buf lexbuf
}
| "\\)" {
  Buffer.add_char buf ')';
  regexp buf lexbuf
}
| ")" {
  Buffer.add_string buf "\\)";
  regexp buf lexbuf
}
| "\\|" {
  Buffer.add_char buf '|';
  regexp buf lexbuf
}
| "|" {
  Buffer.add_string buf "\\|";
  regexp buf lexbuf
}
| '/' {
  REGEXP(Str.regexp (Buffer.contents buf))
}
| _ {
  Buffer.add_char buf (Lexing.lexeme_char lexbuf 0);
  regexp buf lexbuf
}

and multiline_comment = parse
| "*/" { read lexbuf }
| '\n' { Lexing.new_line lexbuf; multiline_comment lexbuf }
| _ { multiline_comment lexbuf }

and read_string buf =
  parse
  | '"'      { STRING (Buffer.contents buf) }
  | '\\' '"' { Buffer.add_char buf '"'; read_string buf lexbuf }
  | [^ '"' '\\' '\n']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | '\n'
  | eof  { raise (Neal.Rule.SyntaxError ("String is not terminated")) }
  | _    { raise (Neal.Rule.SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
