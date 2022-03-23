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

open Angstrom
open Combinators
open Neal.Absyn

let rec grammar = ()

(*|GRAMMAR OF AN IDENTIFIER |*)

(*| implicit-parameter-name -> "$" decimal-digits |*)
and implicitParameterName () =
  mkNode "ImplicitParameter"
  <* wchar '$'
  <:> mkPropE "Offset" decimalDigits

(*| identifier-head -> Upper- or lowercase letter A through Z |*)
(*| identifier-head -> "_" |*)
(*| identifier-head -> U+00A8, U+00AA, U+00AD, U+00AF, U+00B2–U+00B5, or U+00B7–U+00BA |*)
(*| identifier-head -> U+00BC–U+00BE, U+00C0–U+00D6, U+00D8–U+00F6, or U+00F8–U+00FF |*)
(*| identifier-head -> U+0100–U+02FF, U+0370–U+167F, U+1681–U+180D, or U+180F–U+1DBF |*)
(*| identifier-head -> U+1E00–U+1FFF |*)
(*| identifier-head -> U+200B–U+200D, U+202A–U+202E, U+203F–U+2040, U+2054, or U+2060–U+206F |*)
(*| identifier-head -> U+2070–U+20CF, U+2100–U+218F, U+2460–U+24FF, or U+2776–U+2793 |*)
(*| identifier-head -> U+2C00–U+2DFF or U+2E80–U+2FFF |*)
(*| identifier-head -> U+3004–U+3007, U+3021–U+302F, U+3031–U+303F, or U+3040–U+D7FF |*)
(*| identifier-head -> U+F900–U+FD3D, U+FD40–U+FDCF, U+FDF0–U+FE1F, or U+FE30–U+FE44 |*)
(*| identifier-head -> U+FE47–U+FFFD |*)
(*| identifier-head -> U+10000–U+1FFFD, U+20000–U+2FFFD, U+30000–U+3FFFD, or U+40000–U+4FFFD |*)
(*| identifier-head -> U+50000–U+5FFFD, U+60000–U+6FFFD, U+70000–U+7FFFD, or U+80000–U+8FFFD |*)
(*| identifier-head -> U+90000–U+9FFFD, U+A0000–U+AFFFD, U+B0000–U+BFFFD, or U+C0000–U+CFFFD |*)
(*| identifier-head -> U+D0000–U+DFFFD or U+E0000–U+EFFFD |*)
and identifierHead () =
  satisfy (function
    | 'a'..'z'
    | 'A'..'Z'
    | '_'
    | '\xA8'
    | '\xAA'
    | '\xAD'
    | '\xAF'
    | '\xB2'..'\xB5'
    | '\xB7'..'\xBA' ->
    (*TODO: unicode*)
        true
    | _ -> false
  )


(*| identifier-character -> Digit 0 through 9 |*)
(*| identifier-character -> U+0300–U+036F, U+1DC0–U+1DFF, U+20D0–U+20FF, or U+FE20–U+FE2F |*)
(*| identifier-character -> identifier-head |*)
and identifierCharacter () =
  identifierHead ()
  <|> satisfy (function
    | '0'..'9' ->
    (*TODO: unicode*)
        true
    | _ -> false
  )

(*| identifier-characters -> identifier-character identifier-characters ??? |*)
and identifierCharacters () =
  many1 identifierCharacter

(*| identifier -> identifier-head identifier-characters ??? |*)
(*| identifier -> backtick-identifier (not a separate production in the original grammar) |*)
(*| identifier -> implicit-parameter-name |*)

and nonReservedIdentifier pos keywords =
  (
    (List.cons <$> identifierHead () <*> (option [] (identifierCharacters ())))
  ) >>= fun chars ->
  match string_of_chars chars with
  | String l when List.mem l keywords ->
      fail "Keyword can't be used as identifier"
  | str ->
      return (NodeHolder (pos, Node ("Identifier", Off pos, [("Value", str)])))

(*| backtick-identifier -> "`" identifier-head identifier-characters ??? "`" |*)
and backtickIdentifier pos =
  (char '`' *> (List.cons <$> identifierHead () <*> (option [] (identifierCharacters()))) <* char '`') >>= fun str ->
    return (NodeHolder (pos, Node ("Identifier", Off pos, [("Value", string_of_chars str)])))

and identifier' keywords =
  pos >>= fun p ->
    anyspace *> (
      nonReservedIdentifier p keywords
      <|> implicitParameterName ()
      <|> backtickIdentifier p
    )

and paramKeywords = ["inout"; "var"; "let"]

and paramName () = identifier' paramKeywords

and keywords = [
 "associatedtype";
 "class";
 "deinit";
 "enum";
 "extension";
 "fileprivate";
 "func";
 "import";
 "init";
 "inout";
 "internal";
 "let";
 "open";
 "operator";
 "private";
 "protocol";
 "public";
 "static";
 "struct";
 "subscript";
 "typealias";
 "var";
 " break";
 "case";
 "continue";
 "default";
 "defer";
 "do";
 "else";
 "fallthrough";
 "for";
 "guard";
 "if";
 "in";
 "repeat";
 "return";
 "switch";
 "where";
 "while";
 "as";
 "catch";
 "false";
 "is";
 "nil";
 "rethrows";
 "super";
 "self";
 "throw";
 "throws";
 "true";
 "try";
 "await";
]

and identifier () = identifier' keywords

(*| identifier-list -> identifier | identifier "," identifier-list |*)
and identifierList () =
  commaSep identifier

(*| GRAMMAR OF A LITERAL |*)

(*| literal -> numeric-literal | string-literal | boolean-literal | nil-literal |*)
and literal () =
  numericLiteral ()
  <!> stringLiteral
  <!> booleanLiteral
  <!> nilLiteral

(*| numeric-literal -> "- ???" integer-literal | "- ???" floating-point-literal |*)
and numericLiteral () =
  mkOptPropEmpty (mkBoolProp "Negative" (wchar '-')) >>= fun p ->
    (
      floatingPointLiteral ()
      <!> integerLiteral
    ) <:> (return p)

(*| boolean-literal -> "true" | "false" |*)
and booleanLiteral () =
  mkNode "BooleanLiteral"
  <:> mkProp "Value" (
    (wstring "true" *> pos >>| fun pos -> NodeHolder (pos, Bool true))
    <|>
    (wstring "false" *> pos >>| fun pos -> NodeHolder (pos, Bool false))
  )

(*| nil-literal -> "nil" |*)
and nilLiteral () =
  mkNode "NilLiteral"
  <* wstring "nil"


(*| GRAMMAR OF AN INTEGER LITERAL |*)

(*| integer-literal -> binary-literal |*)
(*| integer-literal -> octal-literal |*)
(*| integer-literal -> decimal-literal |*)
(*| integer-literal -> hexadecimal-literal |*)
and integerLiteral () =
  binaryLiteral ()
  <!> octalLiteral
  <!> hexadecimalLiteral
  <!> decimalLiteral

(*| binary-literal -> "0b" binary-digit binary-literal-characters ??? |*)
and binaryLiteral () =
  mkNode "NumericLiteral"
  <:> mkProp "Type" (mkString "Binary")
  <* anyspace
  <* string "0b"
  <:> mkProp "StringValue" (
    pos >>= fun p ->
    binaryDigit() >>= fun d ->
    option [] (binaryLiteralCharacters ()) >>= fun ds ->
      return (NodeHolder (p, string_of_chars ('0'::'b'::d::ds)))
  )

(*| binary-digit -> Digit 0 or 1 |*)
and binaryDigit () =
  satisfy (function
    | '0'..'1' -> true
    | _ -> false
  )
(*| binary-literal-character -> binary-digit | "_" |*)
and binaryLiteralCharacter () =
  binaryDigit ()
  <|> char '_'

(*| binary-literal-characters -> binary-literal-character binary-literal-characters ??? |*)
and binaryLiteralCharacters () =
  many1 binaryLiteralCharacter

(*| octal-literal -> "0o" octal-digit octal-literal-characters ??? |*)
and octalLiteral () =
  mkNode "NumericLiteral"
  <:> mkProp "Type" (mkString "Octal")
  <* anyspace
  <* string "0o"
  <:> mkProp "StringValue" (
    pos >>= fun p ->
    octalDigit() >>= fun d ->
    option [] (octalLiteralCharacters ()) >>= fun ds ->
      return (NodeHolder (p, string_of_chars ('0'::'o'::d::ds)))
  )

(*| octal-digit -> Digit 0 through 7 |*)
and octalDigit () =
  satisfy (function
    | '0'..'7' -> true
    | _ -> false
    )

(*| octal-literal-character -> octal-digit | "_" |*)
and octalLiteralCharacter () =
  octalDigit ()
  <|> char '_'

(*| octal-literal-characters -> octal-literal-character octal-literal-characters ??? |*)
and octalLiteralCharacters () =
  many1 octalLiteralCharacter

(*| decimal-literal -> decimal-digit decimal-literal-characters ??? |*)
and decimalLiteral () =
  mkNode "NumericLiteral"
  <:> mkProp "Type" (mkString "Decimal")
  <* anyspace
  <:> mkProp "StringValue" (
    pos >>= fun p ->
    decimalDigit() >>= fun d ->
    option [] (decimalLiteralCharacters ()) >>= fun ds ->
      return (NodeHolder (p, string_of_chars (d::ds)))
  )

(*| decimal-digit -> Digit 0 through 9 |*)
and decimalDigit () =
  satisfy(function
    | '0'..'9' -> true
    | _ -> false)

(*| decimal-digits -> decimal-digit decimal-digits ??? |*)
and decimalDigits () =
  many1 decimalDigit >>= fun chars ->
    pos >>| fun pos -> NodeHolder (pos, string_of_chars chars)

(*| decimal-literal-character -> decimal-digit | "_" |*)
and decimalLiteralCharacter () =
  decimalDigit ()
  <|> char '_'

(*| decimal-literal-characters -> decimal-literal-character decimal-literal-characters ??? |*)
and decimalLiteralCharacters () =
  many1 decimalLiteralCharacter

(*| hexadecimal-literal -> "0x" hexadecimal-digit hexadecimal-literal-characters ??? |*)
and hexadecimalLiteral () =
  mkNode "NumericLiteral"
  <:> mkProp "Type" (mkString "Hexadecimal")
  <* anyspace
  <* string "0x"
  <:> mkProp "StringValue" (
    pos >>= fun p ->
    hexadecimalDigit() >>= fun d ->
    option [] (hexadecimalLiteralCharacters ()) >>= fun ds ->
      return (NodeHolder (p, string_of_chars ('0'::'x'::d::ds)))
  )

(*| hexadecimal-digit -> Digit 0 through 9, a through f, or A through F |*)
and hexadecimalDigit () =
  satisfy (function
    | '0'..'9'
    | 'a'..'f'
    | 'A'..'F' -> true
    | _ -> false
    )

(*| hexadecimal-literal-character -> hexadecimal-digit | "_" |*)
and hexadecimalLiteralCharacter () =
  hexadecimalDigit ()
  <|> char '_'

(*| hexadecimal-literal-characters -> hexadecimal-literal-character hexadecimal-literal-characters ??? |*)
and hexadecimalLiteralCharacters () =
  many1 hexadecimalLiteralCharacter

(*| GRAMMAR OF A FLOATING-POINT LITERAL |*)

(*| floating-point-literal -> decimal-literal decimal-fraction ??? decimal-exponent ??? |*)
(*| floating-point-literal -> hexadecimal-literal hexadecimal-fraction ??? hexadecimal-exponent |*)
and floatingPointLiteral () =
  (* NOTE: split each of the cases into two where at least one of the *)
  (* properties have to be present, otherwise integer literals will be parsed *)
  (* as floating point literals *)
  mkNode "FloatingPointLiteral"
  <:> (
    (
      mkPropE "Integer" decimalLiteral
      <:> mkPropE "Fraction" decimalFraction
      <:> mkOptPropE "Exponent" decimalExponent
    ) <|> (
      mkPropE "Integer" decimalLiteral
      <:> mkPropE "Exponent" decimalExponent
    ) <|> (
      mkPropE "Integer" hexadecimalLiteral
      <:> mkPropE "Fraction" hexadecimalFraction
      <:> mkOptPropE "Exponent" hexadecimalExponent
    ) <|> (
      mkPropE "Integer" hexadecimalLiteral
      <:> mkPropE "Exponent" hexadecimalExponent
    )
  )

(*| decimal-fraction -> "." decimal-literal |*)
and decimalFraction () =
  char '.' *> decimalLiteral()

(*| decimal-exponent -> floating-point-e sign ??? decimal-literal |*)
and decimalExponent () =
  floatingPointE () *> mkOptE sign >>= fun sign ->
    decimalLiteral ()
    <:> mkProp "Sign" (return sign)

(*| hexadecimal-fraction -> "." hexadecimal-digit hexadecimal-literal-characters ??? |*)
and hexadecimalFraction () =
  char '.' *> pos >>= fun p ->
    hexadecimalDigit() >>= fun d ->
      option [] (hexadecimalLiteralCharacters ()) >>| fun ds ->
        NodeHolder (p, string_of_chars (d::ds))

(*| hexadecimal-exponent -> floating-point-p sign ??? decimal-literal |*)
and hexadecimalExponent () =
  floatingPointP () *> mkOptE sign >>= fun sign ->
    decimalLiteral()
    <:> mkProp "Sign" (return sign)

(*| floating-point-e -> "e" | "E" |*)
and floatingPointE () =
  char 'e' <|> char 'E'

(*| floating-point-p -> "p" | "P" |*)
and floatingPointP () =
  char 'p' <|> char 'P'

(*| sign -> "+" | "-" |*)
and sign () =
  (wfstring "+" <|> wfstring "-") >>= mkString

(*| GRAMMAR OF A STRING LITERAL |*)

(*| string-literal -> static-string-literal | interpolated-string-literal |*)
(*| static-string-literal -> "\"" quoted-text? "\"" |*)
(*| static-string-literal -> "\"\"\"" multiline-quoted-text? "\"\"\"" |*)
(*| interpolated-string-literal -> "\"" interpolated-text? "\"" |*)
(*| interpolated-string-literal -> "\"\"\"" multiline-interpolated-text ? "\"\"\"" |*)

(* NOTE: this diverges from the original grammar as the static-string-literal
 * and interpolated-string-literal productions had to be inline so that we are
 * able to mix static and interpolated strings to always check for multiline
 * strings first
 *)
and stringLiteral () =
  multilineStaticStringLiteral ()
  <!> multilineInterpolatedStringLiteral
  <!> singleLineStaticStringLiteral
  <!> singleLineInterpolatedStringLiteral

and staticStringLiteral () =
  multilineStaticStringLiteral ()
  <!> singleLineStaticStringLiteral


and multilineStaticStringLiteral () =
  mkNode "StaticStringLiteral"
  <* anyspace
  <* string "\"\"\""
  <:> mkOptPropE "Value" multilineQuotedText
  <* string "\"\"\""

(*| multiline-quoted-text -> multiline-quoted-text-item multiline-quoted-text? |*)
and multilineQuotedText () =
  pos >>= fun pos ->
    many1 multilineQuotedTextItem >>| fun strs ->
      NodeHolder (pos, String (String.concat "" strs))

(*| multiline-quoted-text-item -> escaped-character |*)
(*| multiline-quoted-text-item -> Any Unicode scalar value except \ |*)
(*| multiline-quoted-text-item -> escaped-newline|*)
and multilineQuotedTextItem () =
  (
    (string "\"\"\"")
    <|> escapedCharacter ()
    <|> (
      satisfy (function
      | '\\' -> false
      | _ -> true
      ) >>| String.make 1
    )
    <|> escapedNewline ()
  ) >>= function
    | "\"\"\"" -> fail "end of multiline string"
    | str -> return str

and escapedNewline () =
  char '\\' *> anyspace *> char 'n' *> return "\n"

and singleLineStaticStringLiteral () =
  mkNode "StaticStringLiteral"
  <* anyspace
  <* char '"'
  <:> mkOptPropE "Value" quotedText
  <* char '"'

(*| quoted-text -> quoted-text-item quoted-text ??? |*)
and quotedText () =
  pos >>= fun pos ->
    many1 quotedTextItem >>| fun strs ->
      NodeHolder (pos, String (String.concat "" strs))

(*| quoted-text-item -> escaped-character |*)
(*| quoted-text-item -> Any Unicode scalar value except "\"", "\\", U+000A, or U+000D |*)
and quotedTextItem () =
  escapedCharacter ()
  <|>
  (satisfy(function
    | '"'
    | '\\'
    | '\x0a'
    | '\x0d'
    -> false
    | _ -> true) >>| String.make 1)

and multilineInterpolatedStringLiteral () =
  mkNode "InterpolatedStringLiteral"
  <* anyspace
  <* string "\"\"\""
  <:> mkOptPropE "Fragments" multilineInterpolatedText
  <* string "\"\"\""


(*| multiline-interpolated-text -> multiline-interpolated-text-item multiline-interpolated-text? |*)
and multilineInterpolatedText () = interpolatedText' multilineQuotedTextItem

and singleLineInterpolatedStringLiteral () =
  mkNode "InterpolatedStringLiteral"
  <* anyspace
  <* char '"'
  <:> mkOptPropE "Fragments" interpolatedText
  <* char '"'

(*| interpolated-text -> interpolated-text-item interpolated-text ??? |*)
(*| interpolated-text-item -> "\(" expression ")" | quoted-text-item |*)
and interpolatedText () = interpolatedText' quotedTextItem

and interpolatedText' textItem =
  many1 (fun () -> interpolatedTextItem' textItem) >>= fun l ->
    List.fold_left (fun acc i ->
      match acc,i with
      | (InterpolatedCodePoints cs)::rest, (InterpolatedCodePoints [c]) ->
          InterpolatedCodePoints (c::cs) :: rest
      | (InterpolatedCodePoints cs)::rest, (InterpolatedExp e) ->
          (InterpolatedExp e)::(InterpolatedCodePoints cs)::rest
      | l,e ->
          e::l
    ) [] l
    |> List.rev
    |> List.map (function
      | InterpolatedCodePoints cs -> NodeHolder (0, String (String.concat "" (List.rev cs)))
      | InterpolatedExp e -> e)
    |> toList

and interpolatedTextItem' textItem =
  (
    string "\\("
    *> anyspace
    *> (expressionList () >>| fun e -> InterpolatedExp e)
    <* wchar ')'
  ) <|> (
    (textItem () >>| fun c -> InterpolatedCodePoints [c])
  )

(*| escaped-character -> "\0" | "\\" | "\t" | "\n" | "\r" | "\"" | "\'" |*)
(*| escaped-character -> "\u" "{" unicode-scalar-digits "}" |*)
and escapedCharacter () =
  (string "\\0" *> return (String.make 1 (Char.chr 0)))
  <|>
  (string "\\\\" *> return "\\")
  <|>
  (string "\\t" *> return "\t")
  <|>
  (string "\\n" *> return "\n")
  <|>
  (string "\\r" *> return "\r")
  <|>
  (string "\\\""*> return "\"")
  <|>
  (string "\\\'" *> return "'")
  <|>
  (string "\\u{" *> unicodeScalarDigits () <* wchar '}')

(*| unicode-scalar-digits -> Between one and eight hexadecimal digits |*)
and unicodeScalarDigits () =
  many1 hexadecimalDigit >>= fun digits ->
    if List.length digits > 8
    then fail "Invalid unicode sequence"
    else match string_of_chars digits with
    | String s -> return ("\\u{" ^ s ^ "}")
    | _ -> pos >>= unreachable

(*| GRAMMAR OF OPERATORS |*)

(*| operator-head -> "/" | "=" | "-" | "+" | "!" | "*" | "%" | "<" | ">" | "&" | "|" | "^" | "~" | "?" |*)
(*| operator-head -> U+00A1–U+00A7 |*)
(*| operator-head -> U+00A9 or U+00AB |*)
(*| operator-head -> U+00AC or U+00AE |*)
(*| operator-head -> U+00B0–U+00B1, U+00B6, U+00BB, U+00BF, U+00D7, or U+00F7 |*)
(*| operator-head -> U+2016–U+2017 or U+2020–U+2027 |*)
(*| operator-head -> U+2030–U+203E |*)
(*| operator-head -> U+2041–U+2053 |*)
(*| operator-head -> U+2055–U+205E |*)
(*| operator-head -> U+2190–U+23FF |*)
(*| operator-head -> U+2500–U+2775 |*)
(*| operator-head -> U+2794–U+2BFF |*)
(*| operator-head -> U+2E00–U+2E7F |*)
(*| operator-head -> U+3001–U+3003 |*)
(*| operator-head -> U+3008–U+3030 |*)
(*TODO: unicode*)
and operatorHead () =
  satisfy (function
    | '/' | '=' | '-' | '+' | '!' | '*' | '%' | '<' | '>' | '&' | '|' | '^' | '~' | '?'
    | '\xA1'..'\xA7'
    | '\xA9' | '\xAB'
    | '\xAC' | '\xAE'
    | '\xB0'..'\xB1' | '\xB6' | '\xBB' | '\xBF' | '\xD7' | '\xF7'
    -> true
    | _ -> false
  )

(*| operator-character -> operator-head |*)
(*| operator-character -> U+0300–U+036F |*)
(*| operator-character -> U+1DC0–U+1DFF |*)
(*| operator-character -> U+20D0–U+20FF |*)
(*| operator-character -> U+FE00–U+FE0F |*)
(*| operator-character -> U+FE20–U+FE2F |*)
(*| operator-character -> U+E0100–U+E01EF |*)
(*TODO: unicode*)
and operatorCharacter () = operatorHead ()

(*| operator-characters -> operator-character operator-characters ??? |*)
and operatorCharacters () =
  many1 operatorCharacter

(*| dot-operator-head -> "." |*)
and dotOperatorHead () =
  char '.'

(*| dot-operator-character -> "." | operator-character |*)
and dotOperatorCharacter () =
  char '.' <|> operatorCharacter ()

(*| dot-operator-characters -> dot-operator-character dot-operator-characters ??? |*)
and dotOperatorCharacters () =
  many1 dotOperatorCharacter

and check_operator = function
  | [ '/'; '*' ]
  | [ '*'; '/' ]
  | [ '/'; '/' ]
  | [ '?' ]
  -> fail "Invalid operator"
  | _ -> return ()

(*| operator -> operator-head operator-characters ??? |*)
(*| operator -> dot-operator-head dot-operator-characters |*)
and operator' () =
  (*whitespace-less version that allows disambiguating operators*)
  (List.cons <$> operatorHead () <*> option [] (operatorCharacters())
  <|>
  (List.cons <$> dotOperatorHead () <*> dotOperatorCharacters ())
  >>= (fun c ->
    pos >>= fun pos ->
      check_operator c *> return (NodeHolder (pos, string_of_chars c))))

and operator () =
  anyspace *> operator' ()

and operatorWithGenerics () =
  let aux head tail =
    let rec f chars =
      (genericParameterClause () >>| fun gpc -> (chars, gpc))
      <|> (
        (tail >>= fun tl -> f (chars@[tl]))
        <|>
        (mkOpt (genericParameterClause ()) >>| fun gpc -> (chars, gpc))
      )
    in anyspace *> head >>= f
  in
  (
    aux (operatorHead () >>| fun c -> [c]) (operatorCharacter ())
    <|>
    aux (dotOperatorHead() >>= fun h ->
        dotOperatorCharacter () >>| fun c -> [h; c])
    (dotOperatorCharacter ())
  ) >>= fun (chars, gpc) ->
    check_operator chars
    *> mkProp "FunctionName" (return (NodeHolder (0, string_of_chars chars)))
    <:> mkProp "GenericParameterClause" (return gpc)

(*| binary-operator -> operator |*)
and binaryOperator () =
  (lhsOperatorWhitespace true *> anyspace *> operator' () <* rhsOperatorWhitespace true <* anyspace)
  <|>
  (lhsOperatorWhitespace false *> operator' () <* rhsOperatorWhitespace false)


(*| prefix-operator -> operator |*)
and prefixOperator () =
  (
    (lhsOperatorWhitespace true *> anyspace)
    <|>
    (lhsOperatorWhitespace false)
  ) *> operator' () <* rhsOperatorWhitespace false

(*| postfix-operator -> operator |*)
and postfixOperator exp =
  (lhsOperatorWhitespace false *> operator' ())
  >>= fun op ->
    (
      (match op with
      | NodeHolder(_, String(op')) when op' = "!" || op' = "?" ->
          fail "missing whitespace on right-hand side of postfix operator"
      | _ ->
          rhsOperatorWhitespace true
      )
    ) *>
    mkNode "PostfixOperation"
    <:> mkProp "Operand" (return exp)
    <:> mkProp "Operator" (return op)

(*| Types |*)

(*| GRAMMAR OF A TYPE IDENTIFIER |*)

(*| type-name -> identifier |*)
and typeName () = identifier ()

(*| GRAMMAR OF A GENERIC ARGUMENT CLAUSE |*)

(*| generic-argument -> type |*)
and genericArgument () =
  fix type'

(*| generic-argument-list -> generic-argument | generic-argument "," generic-argument-list |*)
and genericArgumentList () =
  commaSep genericArgument

(*| generic-argument-clause -> "<" generic-argument-list ">" |*)
and genericArgumentClause () =
  wchar '<' *> genericArgumentList () <* wchar '>'

(*| type-identifier -> type-name generic-argument-clause ??? | type-name generic-argument-clause ??? "." type-identifier |*)
and typeIdentifier' ~forceMultiple _ =
  mkNode "TypeIdentifier"
  <:> mkPropE "TypeName" typeName
  <:> mkOptPropE "GenericArgumentClause" genericArgumentClause
  <:> (
    if forceMultiple
    then mkProp "TypeIdentifier" (wchar '.' *> typeIdentifier ())
    else mkOptProp "TypeIdentifier" (wchar '.' *> typeIdentifier ())
  )

and typeIdentifier ?(forceMultiple=false) () =
  fix (typeIdentifier' ~forceMultiple)

(*| GRAMMAR OF A TYPE |*)

(*| type -> array-type | dictionary-type | function-type | type-identifier | tuple-type | optional-type | implicitly-unwrapped-optional-type | protocol-composition-type | metatype-type | "Any" | "Self" |*)
and type' _ =
  let aux () =
    arrayType ()
    <!> dictionaryType
    <!> functionType
    <!> tupleType
    <|> (typeIdentifier () >>= fun t -> option t (protocolCompositionType t))
    <|> (wstring "Any" *> mkNode "AnyType")
    <|> (wstring "Self" *> mkNode "SelfType")
  in
  let rec aux' t =
    option t (
      (
        metatypeType t
        <|> optionalType t
        <|> implicitlyUnwrappedOptionalType t
      ) >>= aux'
    )
  in
  aux () >>= aux'

(*| GRAMMAR OF A TYPE ANNOTATION |*)

(*| type-annotation -> ":" attributes ??? "inout"??? type |*)
and typeAnnotation () =
  mkNode "TypeAnnotation"
  <* wchar ':'
  <:> mkOptPropE "Attributes" attributes
  <:> mkOptPropEmpty (mkBoolProp "Inout" (wstring "inout"))
  <:> mkProp "Type" (fix type')

(*| GRAMMAR OF A TUPLE TYPE |*)

(*| tuple-type -> "(" tuple-type-element-list ??? ")" |*)
and tupleType () =
  mkNode "TupleType"
  <* wchar '('
  <:> mkOptPropE "Elements" tupleTypeElementList
  <* wchar ')'

(*| tuple-type-element-list -> tuple-type-element | tuple-type-element "," tuple-type-element-list |*)
and tupleTypeElementList () =
  commaSep tupleTypeElement

(*| tuple-type-element -> element-name type-annotation | type |*)
and tupleTypeElement () =
  (
    elementName ()
    <:> mkPropE "TypeAnnotation" typeAnnotation
  )
  <|> fix type'

(*| element-name -> identifier |*)
and elementName () =
  identifier ()

(*| function-properties -> async ??? throws ??? |*)
(*| function-properties -> async ??? rethrows ??? |*)
and functionProperties () = 
  mkPropHolder
  <:> mkOptPropEmpty (
    mkBoolProp "Async" (wstring "async")
  )
  <:> mkOptPropEmpty (
    mkBoolProp "Throws" (wstring "throws")
    <|>
    mkBoolProp "Rethrows" (wstring "rethrows")
  )

(*| GRAMMAR OF A FUNCTION TYPE |*)

(*| function-type -> attributes ??? function-type-argument-clause function-properties "->" type |*)
and functionType () =
  mkNode "FunctionType"
  <:> mkOptPropE "Attributes" attributes
  <:> mkPropE "FunctionTypeArgumentClause" functionTypeArgumentClause
  <:> functionProperties ()
  <* wfstring "->"
  <:> mkProp "ReturnType" (fix type')

(*| function-type-argument-clause -> "(" ")" |*)
(*| function-type-argument-clause -> "(" function-type-argument-list "... ???" ")" |*)
and functionTypeArgumentClause () =
  mkNode "FunctionTypeArgumentClause"
  <* wchar '('
  <:> mkOptPropEmpty (
    mkPropE "ArgumentList" functionTypeArgumentList
    <:> mkOptPropEmpty (mkBoolProp "Variadic" (wstring "..."))
  )
  <* wchar ')'

(*| function-type-argument-list -> function-type-argument | function-type-argument "," function-type-argument-list |*)
and functionTypeArgumentList () =
  commaSep functionTypeArgument

(*| function-type-argument -> attributes ??? "inout ???" type | argument-label type-annotation |*)
and functionTypeArgument () =
  mkNode "FunctionTypeAgument"
  <:> (
    (
      wstring "_"
      *> mkOptPropE "ArgumentLabel" argumentLabel
      <:> mkPropE "TypeAnnotation" typeAnnotation
    ) <|> (
      mkOptPropE "Attributes" attributes
      <:> mkOptPropEmpty (mkBoolProp "Inout" (wstring "inout"))
      <:> mkProp "Type" (fix type')
    )
  )

(*| argument-label -> identifier |*)
and argumentLabel () =
  identifier ()

(*| GRAMMAR OF AN ARRAY TYPE |*)

(*| array-type -> "[" type "]" |*)
and arrayType () =
  mkNode "ArrayType"
  <* wchar '['
  <:> mkProp "Type" (fix type')
  <* wchar ']'

(*| GRAMMAR OF A DICTIONARY TYPE |*)

(*| dictionary-type -> "[" type ":" type "]" |*)
and dictionaryType () =
  mkNode "DictionaryType"
  <* wchar '['
  <:> mkProp "KeyType" (fix type')
  <* wchar ':'
  <:> mkProp "ValueType" (fix type')
  <* wchar ']'

(*| GRAMMAR OF AN OPTIONAL TYPE |*)

(*| optional-type -> type "?" |*)
and optionalType t =
  return t
  <:> mkBoolProp "Optional" (char '?')

(*| GRAMMAR OF AN IMPLICITLY UNWRAPPED OPTIONAL TYPE |*)

(*| implicitly-unwrapped-optional-type -> type "!" |*)
and implicitlyUnwrappedOptionalType t =
  return t
  <:> mkBoolProp "ImplicitlyUnwrappedOptional" (wchar '!')

(*| GRAMMAR OF A PROTOCOL COMPOSITION TYPE |*)

(*| protocol-composition-type -> protocol-identifier "&" protocol-composition-continuation |*)
(*| protocol-composition-continuation -> protocol-identifier | protocol-composition-type |*)
and protocolCompositionType t =
  wchar '&' *>
  sepBy1 '&' protocolIdentifier >>= fun pis ->
    List.fold_left
      (fun p i ->
        mkNode "ProtocolCompositionType"
        <:> mkProp "Lhs" p
        <:> mkProp "Rhs" (return i)
      )
      (return t)
      pis

(*| protocol-identifier -> type-identifier |*)
and protocolIdentifier () =
  typeIdentifier ()

(*| GRAMMAR OF A METATYPE TYPE |*)

(*| metatype-type -> type "." "Type" | type "." "Protocol" |*)
and metatypeType t =
  return t
  <* wchar '.'
  <:> mkProp "Metatype" (
    (wstring "Type" *> mkString "Type")
    <|>
    (wstring "Protocol" *> mkString "Protocol")
  )

(*| GRAMMAR OF A TYPE INHERITANCE CLAUSE |*)

(*| type-inheritance-list -> type-identifier | type-identifier "," type-inheritance-list |*)
and typeInheritanceList () =
  commaSep typeIdentifier

(*| class-requirement -> "class" |*)
and classRequirement () =
  wstring "class"

(*| type-inheritance-clause -> ":" class-requirement "," type-inheritance-list |*)
(*| type-inheritance-clause -> ":" class-requirement |*)
(*| type-inheritance-clause -> ":" type-inheritance-list |*)
and typeInheritanceClause () =
  wchar ':' *> (
    (classRequirement () *> (mkOpt (wchar ',' *> typeInheritanceList ())))
    <!> typeInheritanceList
  )

(*| Expressions |*)
(*| GRAMMAR OF A TRY EXPRESSION |*)

(*| try-operator -> "try" | "try" "?" | "try" "!" |*)
and tryOperator () =
  (
    wstring "try?"
    <|> wstring "try!"
    <|> wstring "try"
  ) >>= mkString

(*| await-operator -> "await" |*)
and awaitOperator () =
  (
    wstring "await"
  ) >>= mkString

(*| GRAMMAR OF AN ASSIGNMENT OPERATOR |*)

(*| assignment-operator -> "=" |*)
and assignmentOperator () =
  wchar '='

(*| GRAMMAR OF A CONDITIONAL OPERATOR |*)

(*| conditional-operator -> "?" try-operator ??? expression ":" |*)
and conditionalOperator () =
  wchar '?'
  *> mkOptPropE "ConsequentTry" tryOperator
  <:> mkProp "Consequent" (fix expression)
  <* wchar ':'

(*| GRAMMAR OF A TYPE-CASTING OPERATOR |*)

(*| type-casting-operator -> "is" type |*)
(*| type-casting-operator -> "as" type |*)
(*| type-casting-operator -> "as" "?" type |*)
(*| type-casting-operator -> "as" "!" type |*)
and typeCastingOperator () =
  mkProp "Operator" (
    (
      wstring "is"
      <|> wstring "as?"
      <|> wstring "as!"
      <|> wstring "as"
    ) >>= mkString
  )
  <:> mkProp "Type" (fix type')

(*| GRAMMAR OF A PRIMARY EXPRESSION |*)

(*| primary-expression -> identifier generic-argument-clause ??? |*)
(*| primary-expression -> literal-expression |*)
(*| primary-expression -> self-expression |*)
(*| primary-expression -> superclass-expression |*)
(*| primary-expression -> closure-expression |*)
(*| primary-expression -> parenthesized-expression |*)
(*| primary-expression -> tuple-expression |*)
(*| primary-expression -> implicit-member-expression |*)
(*| primary-expression -> wildcard-expression |*)
(*| primary-expression -> selector-expression |*)
(*| primary-expression -> key-path-expression |*)
(*| primary-expression -> key-path-string-expression |*)
and primaryExpression () =
  literalExpression ()
  <!> selfExpression
  <!> superclassExpression
  <!> closureExpression
  <!> parenthesizedExpression
  <!> tupleExpression
  <!> wildcardExpression
  <!> selectorExpression
  <!> keyPathStringExpression
  <!> keyPathExpression
  <|> (identifier () <:> mkOptProp "GenericArgumentClause" (genericArgumentClause ()) >>= fun ident ->
    option ident (
      mkNode "FunctionReference"
      <* wchar '('
      <:> mkPropE "Parameters" argumentNames
      <* wchar ')'
    )
  )
  <!> implicitMemberExpression

(*| GRAMMAR OF A LITERAL EXPRESSION |*)

(*| literal-expression -> literal |*)
(*| literal-expression -> array-literal | dictionary-literal | playground-literal |*)
(*| literal-expression -> "#file" | "#line" | "#column" | "#function" |*)
and literalExpression () =
  literal ()
  <!> arrayLiteral
  <!> dictionaryLiteral
  <!> playgroundLiteral
  <|> (wstring "#file" *> mkNode "HashFileLiteral")
  <|> (wstring "#line" *> mkNode "HashLineLiteral")
  <|> (wstring "#column" *> mkNode "HashColumnLiteral")
  <|> (wstring "#function" *> mkNode "HashFunctionLiteral")

(*| array-literal -> "[" array-literal-items ??? "]" |*)
and arrayLiteral () =
  mkNode "ArrayLiteral"
  <* wchar '['
  <:> mkOptPropE "Items" arrayLiteralItems
  <* wchar ']'

(*| array-literal-items -> array-literal-item ", ???" | array-literal-item "," array-literal-items |*)
and arrayLiteralItems () =
  sepBy1 ',' arrayLiteralItem
  <* anyspace
  <* option () (skip (function ',' -> true | _ -> false))
  >>= toList

(*| array-literal-item -> expression |*)
and arrayLiteralItem () =
  fix expression

(*| dictionary-literal -> "[" dictionary-literal-items "]" | "[" ":" "]" |*)
and dictionaryLiteral () =
  mkNode "DictionaryLiteral"
  <* wchar '['
  <:> mkProp "Items" (
    dictionaryLiteralItems ()
    <|>
    (wchar ':' *> pos >>| fun pos -> NodeHolder (pos, List []))
  )
  <* wchar ']'

(*| dictionary-literal-items -> dictionary-literal-item ", ???" | dictionary-literal-item "," dictionary-literal-items |*)
and dictionaryLiteralItems () =
  sepBy1 ',' dictionaryLiteralItem
  <* anyspace
  <* option () (skip (function ',' -> true | _ -> false))
  >>= toList

(*| dictionary-literal-item -> expression ":" expression |*)
and dictionaryLiteralItem () =
  mkNode "DictionaryLiteralItem"
  <:> mkProp "Key" (fix expression)
  <* wchar ':'
  <:> mkProp "Value" (fix expression)

(*| playground-literal -> "#colorLiteral" "(" "red" ":" expression "," "green" ":" expression "," "blue" ":" expression "," "alpha" ":" expression ")" |*)
(*| playground-literal -> "#fileLiteral" "(" "resourceName" ":" expression ")" |*)
(*| playground-literal -> "#imageLiteral" "(" "resourceName" ":" expression ")" |*)
and playgroundLiteral () =
  (
    mkNode "ColorLiteral"
    <* wstring "#colorLiteral" <* wchar '('
    <:> mkProp "Red" (wstring "red" *> wchar ':' *> fix expression)
    <* wchar ','
    <:> mkProp "Green" (wstring "green" *> wchar ':' *> fix expression)
    <* wchar ','
    <:> mkProp "Blue" (wstring "blue" *> wchar ':' *> fix expression)
    <* wchar ','
    <:> mkProp "Alpha" (wstring "alpha" *> wchar ':' *> fix expression)
    <* wchar ')'
  ) <|> (
    mkNode "FileLiteral"
    <* wstring "#fileLiteral" <* wchar '('
    <:> mkProp "Name" (wstring "resourceName" *> wchar ':' *> fix expression)
    <* wchar ')'
  ) <|> (
    mkNode "ImageLiteral"
    <* wstring "#imageLiteral" <* wchar '('
    <:> mkProp "Name" (wstring "resourceName" *> wchar ':' *> fix expression)
    <* wchar ')'
  )

(*| GRAMMAR OF A SELF EXPRESSION |*)

(*| self-expression -> "self" | self-method-expression | self-subscript-expression | self-initializer-expression |*)
and selfExpression () =
  (wstring "self" *> mkNode "SelfExpression")
  <!> selfMethodExpression
  <!> selfSubscriptExpression
  <!> selfInitializerExpression

(*| self-method-expression -> "self" "." identifier |*)
and selfMethodExpression () =
  mkNode "SelfMethodExpression"
  <* wstring "self"
  <* wchar '.'
  <:> mkPropE "MethodName" identifier

(*| self-subscript-expression -> "self" "[" expression-list "]" |*)
and selfSubscriptExpression () =
  mkNode "SelfSubscriptExpression"
  <* wstring "self"
  <* wchar '['
  <:> mkPropE "SubscriptExpressionList" expressionList
  <* wchar ']'

(*| self-initializer-expression -> "self" "." "init" |*)
and selfInitializerExpression () =
  mkNode "SelfInitializerExpression"
  <* wstring "self"
  <* wchar '.'
  <* wstring "init"

(*| GRAMMAR OF A SUPERCLASS EXPRESSION |*)

(*| superclass-expression -> superclass-method-expression | superclass-subscript-expression | superclass-initializer-expression |*)
and superclassExpression () =
  superclassMethodExpression ()
  <!> superclassSubscriptExpression
  <!> superclassInitializerExpression

(*| superclass-method-expression -> "super" "." identifier |*)
and superclassMethodExpression () =
  mkNode "SuperclassMethodExpression"
  <* wstring "super"
  <* wchar '.'
  <:> mkPropE "MethodName" identifier

(*| superclass-subscript-expression -> "super" "[" expression-list "]" |*)
and superclassSubscriptExpression () =
  mkNode "SuperclassSubscriptExpression"
  <* wstring "super"
  <* wchar '['
  <:> mkPropE "SubscriptExpressionList" expressionList
  <* wchar ']'

(*| superclass-initializer-expression -> "super" "." "init" |*)
and superclassInitializerExpression () =
  mkNode "SuperclassInitializerExpression"
  <* wstring "super"
  <* wchar '.'
  <* wstring "init"

(*| GRAMMAR OF A CLOSURE EXPRESSION |*)

(*| closure-expression -> "{" closure-signature ??? statements ??? "}" |*)
and closureExpression () =
  mkNode "ClosureExpression"
  <* wchar '{'
  <:> mkOptPropEmptyE closureSignature
  <* anyspace
  <:> mkOptProp "Statements" (fix statements)
  <* wchar '}'

(*| closure-signature -> capture-list ??? closure-parameter-clause "throws ???" function-result ??? "in" |*)
(*| closure-signature -> capture-list "in" |*)
and closureSignature () =
  (
    mkOptProp "CaptureList" (captureList () <* anyspace)
    <:> mkPropE "ClosureParameterClause" closureParameterClause
    <:> mkOptPropEmpty (mkBoolProp "Throws" (wstring "throws"))
    <:> mkOptPropE "FunctionResult" functionResult
    <* wstring "in"
  ) <|> (
    mkPropE "CaptureList" captureList
    <* wstring "in"
  )

(*| closure-parameter-clause -> "(" ")" | "(" closure-parameter-list ")" | identifier-list |*)
and closureParameterClause () =
  (
    wchar '('
    *> mkOptE closureParameterList
    <* wchar ')'
  )
  <!> identifierList

(*| closure-parameter-list -> closure-parameter | closure-parameter "," closure-parameter-list |*)
and closureParameterList () =
  commaSep closureParameter

(*| closure-parameter -> closure-parameter-name type-annotation ??? |*)
(*| closure-parameter -> closure-parameter-name type-annotation "..." |*)
and closureParameter () =
  mkNode "ClosureParameter"
  <:> (
    (wstring "_" *> mkPropE "Name" closureParameterName)
    <|> mkPropE "Name" closureParameterName
  ) <:> (
    (
      mkPropE "TypeAnnotation" typeAnnotation
      <:> mkBoolProp "Variadic" (wstring "...")
    )
    <|> mkOptPropE "TypeAnnotation" typeAnnotation
  )

(*| closure-parameter-name -> identifier |*)
and closureParameterName () =
  identifier ()

(*| capture-list -> "[" capture-list-items "]" |*)
and captureList () =
  wchar '[' *> captureListItems () <* wchar ']'

(*| capture-list-items -> capture-list-item | capture-list-item "," capture-list-items |*)
and captureListItems () =
  commaSep captureListItem

(*| capture-list-item -> capture-specifier ??? expression |*)
and captureListItem () =
  mkOptPropEmptyE captureSpecifier >>= fun captureSpecifier ->
    mkNode "CaptureItem"
    <:> mkProp "Expression" (fix expression)
    <:> (return captureSpecifier)

(*| capture-specifier -> "weak" | "unowned" | "unowned(safe)" | "unowned(unsafe)" |*)
and captureSpecifier () =
  mkBoolProp "Weak" (wstring "weak")
  <|> mkBoolProp "Unowned" (wstring "unowned")
  <|> mkBoolProp "UnownedSafe" (wstring "unowned(safe)")
  <|> mkBoolProp "UnownedUnsafe" (wstring "unowned(unsafe)")

(*| GRAMMAR OF A IMPLICIT MEMBER EXPRESSION |*)

(*| implicit-member-expression -> "." identifier |*)
and implicitMemberExpression () =
  mkNode "ImplicitMemberExpression"
  <* wchar '.'
  <:> mkPropE "Property" identifier

(*| GRAMMAR OF A PARENTHESIZED EXPRESSION |*)

(*| parenthesized-expression -> "(" expression ")" |*)
and parenthesizedExpression () =
  wchar '(' *> fix expression <* wchar ')'

(*| GRAMMAR OF A TUPLE EXPRESSION |*)

(*| tuple-expression -> "(" ")" | "(" tuple-element "," tuple-element-list ")" |*)
and tupleExpression () =
  mkNode "TupleExpression"
  <* wchar '('
  <:> mkOptPropE "Items" tupleElementList
  <* wchar ')'

(*| tuple-element-list -> tuple-element | tuple-element "," tuple-element-list |*)
and tupleElementList () =
  commaSep tupleElement

(*| tuple-element -> expression | identifier ":" expression |*)
and tupleElement () =
  mkNode "TupleElement"
  <:> mkOptProp "Label" (identifier () <* wchar ':')
  <:> mkProp "Expression" (fix expression)

(*| GRAMMAR OF A WILDCARD EXPRESSION |*)

(*| wildcard-expression -> "_" |*)
and wildcardExpression () =
  mkNode "WildcardExpression"
  <* wstring "_"

(*| GRAMMAR OF A SELECTOR EXPRESSION |*)

(*| selector-expression -> "#selector" "(" expression ")" |*)
(*| selector-expression -> "#selector" "(" "getter:" expression ")" |*)
(*| selector-expression -> "#selector" "(" "setter:" expression ")" |*)
and selectorExpression () =
  mkNode "SelectorExpression"
  <* wstring "#selector"
  <* wchar '('
  <:> (
    mkProp "GetterExpression" (wstring "getter:" *> fix expression)
    <|> mkProp "SetterExpression" (wstring "setter:" *> fix expression)
    <|> mkProp "Expression" (fix expression)
  )
  <* wchar ')'

(*| GRAMMAR OF A KEY-PATH EXPRESSION |*)

(*| key-path-expression -> "\\" type? "." key-path-components |*)
and keyPathExpression () =
  mkNode "KeyPathExpression"
  <* wchar '\\'
  <:> mkProp "Expression" (fix expression)
  <:> mkOptProp "KeyPathComponents" (wchar '.' *> keyPathComponents ())

(*| key-path-components -> key-path-component | key-path-component . key-path-components |*)
and keyPathComponents () =
  sepBy1 '.' keyPathComponent >>= toList

(*| key-path-component -> identifier key-path-postfixes? | key-path-postfixes |*)
and keyPathComponent () =
  mkNode "KeyPathComponent"
  <:> ((
    mkPropHolder
    <:> mkPropE "Identifier" identifier
    <:> mkOptPropE "Postfixes" keyPathPostfixes
  ) <|> (
    mkPropHolder
    <:> mkPropE "Postfixes" keyPathPostfixes
  ))

(*| key-path-postfixes -> key-path-postfix key-path-postfixes? |*)
and keyPathPostfixes () =
  many1 keyPathPostfix >>= toList

(*| key-path-postfix -> "?" | "!" | "[" function-call-argument-list "]" |*)
and keyPathPostfix () =
  (
    wchar '?' *> mkNode "KeyPathOptionalPostfix"
  ) <|> (
    wchar '!' *> mkNode "KeyPathForcedPostfix"
  ) <|> (
    mkNode "KeyPathSubscriptPostfix"
    <* wchar '['
    <:> mkPropE "Arguments" functionCallArgumentList
    <* wchar ']'
  )

(*| key-path-string-expression -> "#keyPath" "(" expression ")" |*)
and keyPathStringExpression () =
  mkNode "KeyPathStringExpression"
  <* wstring "#keyPath"
  <* wchar '('
  <:> mkProp "Expression" (fix expression)
  <* wchar ')'

(*| GRAMMAR OF A FUNCTION CALL EXPRESSION |*)

(*| trailing-closure -> closure-expression |*)
and trailingClosure ~allowTrailingClosure () =
  if allowTrailingClosure
  then closureExpression ()
  else fail "trailing closure requires parentheses for disambiguation in this context"

(*| function-call-argument -> expression | identifier ":" expression |*)
(*| function-call-argument -> operator | identifier ":" operator |*)
and functionCallArgument () =
  mkNode "FunctionCallArgument"
  <:> mkOptProp "Label" (paramName () <* wchar ':')
  <:> (
    mkProp "Expression" (fix expression)
    <|> mkPropE "Operator" operator
  )

(*| function-call-argument-list -> function-call-argument | function-call-argument "," function-call-argument-list |*)
and functionCallArgumentList () =
  commaSep functionCallArgument

(*| function-call-argument-clause -> "(" ")" | "(" function-call-argument-list ")" |*)
and functionCallArgumentClause () =
  (wchar '(' *> wchar ')' *> toList [])
  <|>
  (wchar '(' *> functionCallArgumentList () <* wchar ')')

(*| function-call-expression -> postfix-expression function-call-argument-clause |*)
(*| function-call-expression -> postfix-expression function-call-argument-clause ??? trailing-closure |*)
and functionCallExpression ~allowTrailingClosure callee =
  (* This functions is absolutely awful. It turns out that you cannot have *)
  (* two trailing closures in a row, i.e. `f{}(){}` is ok but `f{}{}` is not. *)
  (* even though both expression are equivalent only the first one is *)
  (* succesfully parsed by the swift compiler. This is not encoded in the *)
  (* official grammar in any way, but it turns out a different version of the *)
  (* grammar is available in comments in the swift parser: *)
  (*https://github.com/apple/swift/blob/96f2a04f558891bbc48e82ce9373465dd00b9014/lib/Parse/ParseExpr.cpp#L1001*)
  pos >>= fun pos ->
    (
      mkNode ~pos:(Some pos) "CallExpression"
      <:> mkProp "Callee" (return callee)
      <:> mkOptPropE "Arguments" functionCallArgumentClause
      <:> mkPropE "Closure" (trailingClosure ~allowTrailingClosure)
      >>= fun exp ->
        postfixExpression' ~allowTrailingClosure:false exp
        >>= fun exp' ->
          if exp <> exp'
          then postfixExpression' ~allowTrailingClosure exp'
          else return exp'
    ) <|> (
      mkNode ~pos:(Some pos) "CallExpression"
      <:> mkProp "Callee" (return callee)
      <:> mkPropE "Arguments" functionCallArgumentClause
    )

(*| GRAMMAR OF AN INITIALIZER EXPRESSION |*)

(*| initializer-expression -> postfix-expression "." "init" |*)
(*| initializer-expression -> postfix-expression "." "init" "(" argument-names ")" |*)
and initializerExpression exp =
  mkNode "InitializerExpression"
  <* wchar '.'
  <* wstring "init"
  <:> mkProp "Expression" (return exp)
  <:> mkOptProp "Arguments" (wchar '(' *> argumentNames () <* wchar ')')


(*| GRAMMAR OF A POSTFIX EXPRESSION |*)

(*| postfix-expression -> primary-expression |*)
(*| postfix-expression -> postfix-expression postfix-operator |*)
(*| postfix-expression -> function-call-expression |*)
(*| postfix-expression -> initializer-expression |*)
(*| postfix-expression -> explicit-member-expression |*)
(*| postfix-expression -> postfix-self-expression |*)
(*| postfix-expression -> dynamic-type-expression |*)
(*| postfix-expression -> subscript-expression |*)
(*| postfix-expression -> forced-value-expression |*)
(*| postfix-expression -> optional-chaining-expression |*)
and postfixExpression ~allowTrailingClosure ~allowTypeAnnotation () =
  (
    (ifParsingPattern *> fix (pattern ~allowExpression:false ~allowIdentiifer:false ~allowTypeAnnotation))
    <!> primaryExpression
    <!> dynamicTypeExpression
  ) >>= postfixExpression' ~allowTrailingClosure

and postfixExpression' ~allowTrailingClosure exp =
  option exp (
    (
      explicitMemberExpression exp
      <|> optionalChainingExpression exp
      <|> postfixOperator exp
      <|> postfixSelfExpression exp
      <|> initializerExpression exp
      <|> subscriptExpression exp
      <|> forcedValueExpression exp
      <|> functionCallExpression ~allowTrailingClosure exp
    ) >>= postfixExpression' ~allowTrailingClosure
  )

(*| GRAMMAR OF A PREFIX EXPRESSION |*)

(*| in-out-expression -> "&" identifier |*)
and inOutExpression () =
  mkNode "InOutExpression"
  <:> mkBoolProp "Inout" (wchar '&')
  <:> mkPropE "Value" identifier

(*| prefix-expression -> prefix-operator ??? postfix-expression |*)
(*| prefix-expression -> in-out-expression |*)
and prefixExpression ~allowTrailingClosure ~allowTypeAnnotation () =
  (
    mkNode "PrefixExpression"
    <:> mkPropE "Operator" prefixOperator
    <:> mkPropE "Operand" (postfixExpression ~allowTrailingClosure ~allowTypeAnnotation)
  )
  <!> postfixExpression ~allowTrailingClosure ~allowTypeAnnotation
  <!> inOutExpression

(*| GRAMMAR OF A BINARY EXPRESSION |*)

(*| binary-expression -> binary-operator prefix-expression |*)
(*| binary-expression -> assignment-operator try-operator ??? prefix-expression |*)
(*| binary-expression -> conditional-operator try-operator ??? prefix-expression |*)
(*| binary-expression -> type-casting-operator |*)
and binaryExpression ~allowAssignment ~allowTrailingClosure ~allowTypeAnnotation exp =
  let assignmentExpression () =
    mkNode "AssignmentExpression"
    <:> mkProp "Assignee" (return exp)
    <* assignmentOperator ()
    <:> mkOptPropE "Await" awaitOperator
    <:> mkOptPropE "Try" tryOperator
    <:> mkPropE "Value" (prefixExpression ~allowTrailingClosure ~allowTypeAnnotation)
  and conditionalExpression () =
    mkNode "ConditionalExpression"
    <:> conditionalOperator ()
    <:> mkOptPropE "Await" awaitOperator
    <:> mkOptPropE "AlternateTry" tryOperator
    <:> mkPropE "Alternate" (prefixExpression ~allowTrailingClosure ~allowTypeAnnotation)
  and typeCastingExpression () =
    mkNode "TypeCastingExpresion"
    <:> typeCastingOperator ()
  and binaryExpression () =
    mkNode "BinaryExpression"
    <:> mkProp "Lhs" (return exp)
    <:> mkPropE "Operator" binaryOperator
    <:> mkPropE "Rhs" (prefixExpression ~allowTrailingClosure ~allowTypeAnnotation)
  in
  (assignmentExpression () >>| fun assignment ->
    if allowAssignment
    then assignment
    (* we have to return a placeholder here to break the recursion on
     * `binary-expressions` since if we just `fail` if will fall through and
     * match `binaryExpression`
     *)
    else invalidNode
  )
  <!> conditionalExpression
  <!> typeCastingExpression
  <!> binaryExpression

(*| binary-expressions -> binary-expression binary-expressions ??? |*)
and binaryExpressions ~allowAssignment ~allowTrailingClosure ~allowTypeAnnotation exp =
    option exp (
      binaryExpression ~allowAssignment ~allowTrailingClosure ~allowTypeAnnotation exp
      >>= (function
        (* Here we look for the placeholder emitted above and break the
         * recursion if we hit an assignment in a context where it's not allowed
         *)
        | e when e = invalidNode -> fail "Terminate BinaryExpression"
        | e -> return e
      ) >>= binaryExpressions ~allowAssignment ~allowTrailingClosure ~allowTypeAnnotation
    )

(*| GRAMMAR OF AN EXPRESSION |*)

(*| expression -> await-operator ??? try-operator ??? prefix-expression binary-expressions ??? |*)
and expression
?allowAssignment:(allowAssignment:bool = true)
?allowTrailingClosure:(allowTrailingClosure:bool = true)
?(allowTypeAnnotation=true)
_
=
  mkOptE tryOperator 
  >>= 
    fun tryop ->
      mkOptE awaitOperator 
  >>=
    fun awaitop ->
      prefixExpression ~allowTrailingClosure ~allowTypeAnnotation () 
  >>= 
    fun pre ->
      option pre (binaryExpressions ~allowAssignment ~allowTrailingClosure ~allowTypeAnnotation pre)
  <:> mkProp "Try" (return tryop)
  <:> mkProp "Await" (return awaitop)

(*| expression-list -> expression | expression "," expression-list |*)
and expressionList () =
  sepBy1 ',' (fun () ->
    (
      mkNode "LabeledExpression"
      <:> mkPropE "Label" identifier
      <* wchar ':'
      <:> mkProp "Expression" (fix expression)
    ) <|> fix expression
  ) >>= function
    | [e] -> return e
    | l -> toList l

(*| GRAMMAR OF AN EXPLICIT MEMBER EXPRESSION |*)

(*| explicit-member-expression -> postfix-expression "." decimal-digits |*)
(*| explicit-member-expression -> postfix-expression "." identifier generic-argument-clause ??? |*)
(*| explicit-member-expression -> postfix-expression "." identifier "(" argument-names ")" |*)
and explicitMemberExpression exp =
  wchar '.'
  *> (
    (
      mkNode "TupleMemberExpression"
      <:> mkProp "Expression" (return exp)
      <:> mkPropE "Field" decimalDigits
    ) <|> (
      mkNode "ExplicitMemberExpression"
      <:> mkProp "Object" (return exp)
      <:> mkPropE "Member" identifier
      <* wchar '('
      <:> mkPropE "Parameters" argumentNames
      <* wchar ')'
    ) <|> (
      mkNode "ExplicitMemberExpression"
      <:> mkProp "Object" (return exp)
      <:> mkPropE "Member" identifier
      <:> mkOptPropE "GenericArgumentClause" genericArgumentClause
    )
  )

(*| argument-names -> argument-name argument-names ??? |*)
and argumentNames () =
  mkList1 argumentName

(*| argument-name -> identifier ":" |*)
and argumentName () =
  identifier ()
  <* wchar ':'

(*| GRAMMAR OF A SELF EXPRESSION |*)

(*| postfix-self-expression -> postfix-expression "." "self" |*)
and postfixSelfExpression exp =
  mkNode "SelfExpression"
  <* wchar '.'
  <* wstring "self"
  <:> mkProp "Expression" (return exp)

(*| GRAMMAR OF A DYNAMIC TYPE EXPRESSION |*)

(*| dynamic-type-expression -> "type" "(" "of" ":" expression ")" |*)
and dynamicTypeExpression () =
  mkNode "DynamicTypeExpression"
  <* wstring "type"
  <* wchar '('
  <* wstring "of"
  <* wchar ':'
  <:> mkProp "Expression" (fix expression)
  <* wchar ')'

(*| GRAMMAR OF A SUBSCRIPT EXPRESSION |*)

(*| subscript-expression -> postfix-expression "[" expression-list "]" |*)
and subscriptExpression exp =
  mkNode "SubscriptExpression"
  <* wchar '['
  <:> mkProp "Expression" (return exp)
  <:> mkPropE "SubscriptExpressionList" expressionList
  <* wchar ']'

(*| GRAMMAR OF A FORCED-VALUE EXPRESSION |*)

(*| forced-value-expression -> postfix-expression "!" |*)
and forcedValueExpression exp =
  mkNode "ForcedValueExpression"
  <* char '!'
  <:> mkProp "Expression" (return exp)

(*| GRAMMAR OF AN OPTIONAL-CHAINING EXPRESSION |*)

(*| optional-chaining-expression -> postfix-expression "?" |*)
and optionalChainingExpression exp =
  mkNode "OptionalChainingExpression"
  <* char '?'
  <:> mkProp "Expression" (return exp)

(*| GRAMMAR OF A LOOP STATEMENT |*)

(*| loop-statement -> for-in-statement |*)
(*| loop-statement -> while-statement |*)
(*| loop-statement -> repeat-while-statement |*)
and loopStatement () =
  forInStatement ()
  <!> whileStatement
  <!> repeatWhileStatement

(*| GRAMMAR OF A FOR-IN STATEMENT |*)

(*| async-sequence-operator -> "try ??? await" |*)
and asyncSequenceOperator () =
  (wstring  "await") <|> (wstring "try" *> anyspace *> wstring "await")


(*| for-in-statement -> "for" "case ???" pattern "in" expression where-clause ??? code-block |*)
(*| for-in-statement -> "for" async-sequence-operator ??? pattern "in" expression where-clause ??? code-block |*)
and forInStatement () =
  mkNode "ForInStatement"
  <* wstring "for"
  <:> mkOptPropEmpty (mkBoolProp "Case" (wstring "case"))
  <:> mkOptPropEmpty (mkBoolProp "AsyncSequence" (asyncSequenceOperator ()))
  <:> mkProp "Pattern" (fix (pattern ~allowExpression:false))
  <* wstring "in"
  <:> mkProp "Condition" (fix (expression ~allowTrailingClosure:false))
  <:> mkOptPropE "WhereClause" whereClause
  <:> mkPropE "Body" codeBlock

(*| GRAMMAR OF A WHILE STATEMENT |*)

(*| while-statement -> "while" condition-list code-block |*)
and whileStatement () =
  mkNode "WhileStatement"
  <* wstring "while"
  <:> mkPropE "Conditions" conditionList
  <:> mkPropE "Body" codeBlock

(*| condition-list -> condition | condition "," condition-list |*)
and conditionList () =
  commaSep condition

(*| condition -> expression | availability-condition | case-condition | optional-binding-condition |*)
and condition () =
  let allowTrailingClosure = false in
  availabilityCondition ()
  <!> caseCondition ~allowTrailingClosure
  <!> optionalBindingCondition ~allowTrailingClosure
  <|> fix (expression ~allowTrailingClosure)

(*| case-condition -> "case" pattern initializer |*)
and caseCondition ~allowTrailingClosure () =
  mkNode "CaseCondition"
  <* wstring "case"
  <:> mkProp "Pattern" (fix (pattern ~allowAssignment:false))
  <:> mkProp "Initializer" (initializer' ~allowTrailingClosure ())

(*| optional-binding-condition -> "let" pattern initializer | "var" pattern initializer |*)
and optionalBindingCondition ~allowTrailingClosure () =
  mkNode "OptionalBindingInitializer"
  <:> (
    mkBoolProp "Constant" (wstring "let")
    <|> (wstring "var" *> mkPropHolder)
  )
  <:> mkProp "Pattern" (fix (pattern ~allowAssignment:false ~allowExpression:false))
  <:> mkProp "Initializer" (initializer' ~allowTrailingClosure ())

(*| GRAMMAR OF A REPEAT-WHILE STATEMENT |*)

(*| repeat-while-statement -> "repeat" code-block "while" expression |*)
and repeatWhileStatement () =
  mkNode "RepeatWhileStatement"
  <* wstring "repeat"
  <:> mkPropE "Body" codeBlock
  <* wstring "while"
  <:> mkProp "Condition" (fix expression)

(*| GRAMMAR OF A BRANCH STATEMENT |*)

(*| branch-statement -> if-statement |*)
(*| branch-statement -> guard-statement |*)
(*| branch-statement -> switch-statement |*)
and branchStatement () =
  fix ifStatement
  <!> guardStatement
  <!> switchStatement

(*| GRAMMAR OF AN IF STATEMENT |*)

(*| if-statement -> "if" condition-list code-block else-clause ??? |*)
and ifStatement _ =
  mkNode "IfStatement"
  <* wstring "if"
  <:> mkPropE "Conditions" conditionList
  <:> mkPropE "Consequent" codeBlock
  <:> mkOptPropE "Alternate" elseClause

(*| else-clause -> "else" code-block | "else" if-statement |*)
and elseClause () =
  wstring "else"
  *> (
    codeBlock ()
    <|> fix ifStatement
  )

(*| GRAMMAR OF A GUARD STATEMENT |*)

(*| guard-statement -> "guard" condition-list "else" code-block |*)
and guardStatement () =
  mkNode "GuardStatement"
  <* wstring "guard"
  <:> mkPropE "Conditions" conditionList
  <* wstring "else"
  <:> mkPropE "Alternate" codeBlock

(*| GRAMMAR OF A SWITCH STATEMENT |*)

(*| switch-statement -> "switch" expression "{" switch-cases ??? "}" |*)
and switchStatement () =
  mkNode "SwitchStatement"
  <* wstring "switch"
  <:> mkProp "Value" (fix expression)
  <* wchar '{'
  <:> mkOptPropE "Cases" switchCases
  <* wchar '}'

(*| switch-cases -> switch-case switch-cases ??? |*)
and switchCases () =
  mkList1 switchCase

(*| switch-case -> case-label statements | default-label statements |*)
and switchCase () =
  mkNode "SwitchCase"
  <:> (
    mkPropE "CaseLabel" caseLabel
    <|> mkPropE "DefaultLabel" defaultLabel
  )
  <:> mkProp "Body" (fix statements)

(*| case-label -> "case" case-item-list ":" |*)
and caseLabel () =
  mkNode "CaseLabel"
  <* wstring "case"
  <:> mkPropE "Items" caseItemList
  <* wchar ':'

(*| case-item-list -> pattern where-clause ??? | pattern where-clause ??? "," case-item-list |*)
and caseItemList () =
  commaSep (fun () ->
    mkNode "CaseItem"
    <:> mkProp "Pattern" (fix (pattern ~allowTypeAnnotation:false))
    <:> mkOptPropE "WhereClause" whereClause
  )

(*| default-label -> "default" ":" |*)
and defaultLabel () =
  mkNode "DefaultLabel"
  <* wstring "default"
  <* wchar ':'

(*| where-clause -> "where" where-expression |*)
and whereClause () =
  wstring "where" *> whereExpression ()

(*| where-expression -> expression |*)
and whereExpression () =
  fix (expression ~allowTrailingClosure:false)

(*| GRAMMAR OF A LABELED STATEMENT |*)

(*| labeled-statement -> statement-label loop-statement |*)
(*| labeled-statement -> statement-label if-statement |*)
(*| labeled-statement -> statement-label switch-statement |*)
(*| labeled-statement -> statement-label do-statement |*)
and labeledStatement () =
  mkNode "LabeledStatement"
  <:> mkPropE "Label" statementLabel
  <:> mkProp "Statement" (
    loopStatement ()
    <|> fix ifStatement
    <!> switchStatement
    <!> doStatement
  )

(*| statement-label -> label-name ":" |*)
and statementLabel () =
  labelName ()
  <* wchar ':'

(*| label-name -> identifier |*)
and labelName () =
  identifier ()

(*This is not part of the grammar, but `default` is not a valid label*)
(*for `break` or `continue` statements.*)
and nonReservedLabelName () =
  ((wstring "default" <|> wstring "case") *> return invalidNode)
  <|> labelName ()
  >>= function
  | l when l = invalidNode ->
      fail "Default is a reserved label name"
  | l -> return l

(*| GRAMMAR OF A CONTROL TRANSFER STATEMENT |*)

(*| control-transfer-statement -> break-statement |*)
(*| control-transfer-statement -> continue-statement |*)
(*| control-transfer-statement -> fallthrough-statement |*)
(*| control-transfer-statement -> return-statement |*)
(*| control-transfer-statement -> throw-statement |*)
and controlTransferStatement () =
  breakStatement ()
  <!> continueStatement
  <!> fallthroughStatement
  <!> returnStatement
  <!> throwStatement

(*| GRAMMAR OF A BREAK STATEMENT |*)

(*| break-statement -> "break" label-name ??? |*)
and breakStatement () =
  mkNode "BreakStatement"
  <* wstring "break"
  <:> mkOptPropE "Label" nonReservedLabelName

(*| GRAMMAR OF A CONTINUE STATEMENT |*)

(*| continue-statement -> "continue" label-name ??? |*)
and continueStatement () =
  mkNode "ContinueStatement"
  <* wstring "continue"
  <:> mkOptPropE "Label" nonReservedLabelName

(*| GRAMMAR OF A FALLTHROUGH STATEMENT |*)

(*| fallthrough-statement -> "fallthrough" |*)
and fallthroughStatement () =
  mkNode "FallthroughStatement"
  <* wstring "fallthrough"

(*| GRAMMAR OF A RETURN STATEMENT |*)

(*| return-statement -> "return" expression ??? |*)
and returnStatement () =
  mkNode "ReturnStatement"
  <* wstring "return"
  <:> mkOptProp "Expression" (fix expression)

(*| GRAMMAR OF A THROW STATEMENT |*)

(*| throw-statement -> "throw" expression |*)
and throwStatement () =
  mkNode "ThrowStatement"
  <* wstring "throw"
  <:> mkProp "Expression" (fix expression)

(*| GRAMMAR OF A DEFER STATEMENT |*)

(*| defer-statement -> "defer" code-block |*)
and deferStatement () =
  mkNode "DeferStatement"
  <* wstring "defer"
  <:> mkPropE "Body" codeBlock

(*| GRAMMAR OF A DO STATEMENT |*)

(*| do-statement -> "do" code-block catch-clauses ??? |*)
and doStatement () =
  mkNode "DoStatement"
  <* wstring "do"
  <:> mkPropE "Body" codeBlock
  <:> mkOptPropE "CatchClauses" catchClauses

(*| catch-clauses -> catch-clause catch-clauses ??? |*)
and catchClauses () =
  mkList1 catchClause

(*| catch-clause -> "catch" pattern ??? where-clause ??? code-block |*)
and catchClause () =
  mkNode "Catch"
  <* wstring "catch"
  <:> (
    (* We need to try to match the body first, as it has higher precedence *)
    (* than a closure expression (arising from pattern) *)
    mkPropE "Body" codeBlock
    <|> (
      mkOptProp "Pattern" (fix (pattern ~allowTrailingClosure:false))
      <:> mkOptPropE "Where" whereClause
      <:> mkPropE "Body" codeBlock
    )
  )

(*| GRAMMAR OF A COMPILER CONTROL STATEMENT |*)

(*| compiler-control-statement -> conditional-compilation-block |*)
(*| compiler-control-statement -> line-control-statement |*)
and compilerControlStatement () =
  conditionalCompilationBlock ()
  <|>
  lineControlStatement ()

(*| GRAMMAR OF A CONDITIONAL COMPILATION BLOCK |*)

(*| conditional-compilation-block -> if-directive-clause elseif-directive-clauses ??? else-directive-clause ??? endif-directive |*)
and conditionalCompilationBlock () =
  mkNode "ConditionalCompilationBlock"
  <:> mkPropE "IfDirectiveClause" ifDirectiveClause
  <:> mkOptPropE "ElseifDirectiveClauses" elseifDirectiveClauses
  <:> mkOptPropE "ElseDirectiveClause" elseDirectiveClause
  <* endifDirective ()

(*| if-directive-clause -> if-directive compilation-condition statements ??? |*)
and ifDirectiveClause () =
  ifDirective ()
  *> mkNode "IfDirective"
  <:> mkProp "CompilationCondition" (fix compilationCondition)
  <* anyspace
  <:> mkOptProp "Statements" (fix statements)

(*| elseif-directive-clauses -> elseif-directive-clause elseif-directive-clauses ??? |*)
and elseifDirectiveClauses () =
  mkList1 elseifDirectiveClause

(*| elseif-directive-clause -> elseif-directive compilation-condition statements ??? |*)
and elseifDirectiveClause () =
  elseifDirective ()
  *> mkNode "ElseifDirective"
  <:> mkProp "CompilationCondition" (fix compilationCondition)
  <* anyspace
  <:> mkOptProp "Statements" (fix statements)

(*| else-directive-clause -> else-directive statements ??? |*)
and elseDirectiveClause () =
  elseDirective ()
  *> mkNode "ElseDirectiveClause"
  <:> mkOptProp "Statements" (fix statements)

(*| if-directive -> "#if" |*)
and ifDirective () =
  wstring "#if"

(*| elseif-directive -> "#elseif" |*)
and elseifDirective () =
  wstring "#elseif"

(*| else-directive -> "#else" |*)
and elseDirective () =
  wstring "#else"

(*| endif-directive -> "#endif" |*)
and endifDirective () =
  wstring "#endif"

(*| compilation-condition -> platform-condition |*)
(*| compilation-condition -> identifier |*)
(*| compilation-condition -> boolean-literal |*)
(*| compilation-condition -> "(" compilation-condition ")" |*)
(*| compilation-condition -> "!" compilation-condition |*)
(*| compilation-condition -> compilation-condition "&&" compilation-condition |*)
(*| compilation-condition -> compilation-condition "||" compilation-condition |*)
and compilationCondition _ =
  let aux () =
    platformCondition ()
    <!> identifier
    <!> booleanLiteral
    <|> (wchar '(' *> fix compilationCondition <* wchar ')')
    <|> (
      mkNode "CompilationConditionExpression"
      <:> mkProp "Operator" (wchar '!' *> mkString "!")
      <* commit
      <:> mkProp "Operand" (fix compilationCondition)
    )
  in
  let rec aux' t =
    option t (
      (
      mkNode "CompilationConditionExpression"
      <:> mkProp "Lhs" (return t)
      <:> mkProp "Operator" ((wfstring "&&" <|> wfstring "||") >>= mkString)
      <:> mkProp "Rhs" (fix compilationCondition)
      ) >>= aux'
    )
  in
  aux () >>= aux'

(*| platform-condition -> "os" "(" operating-system ")" |*)
(*| platform-condition -> "arch" "(" architecture ")" |*)
(*| platform-condition -> "swift" "(" ">=" swift-version ")" |*)
and platformCondition () =
  (wstring "os" *> wchar '(' *> operatingSystem () <* wchar ')')
  <|>
  (wstring "arch" *> wchar '(' *> architecture () <* wchar ')')
  <|>
  (wstring "swift" *> wchar '(' *> wstring ">=" *> swiftVersion () <* wchar ')')

(*| operating-system -> "macOS" | "iOS" | "watchOS" | "tvOS" |*)
and operatingSystem () =
  mkNode "OperatingSystem"
  <:> mkProp "Value" (
    (wstring "macOS" >>= mkString)
    <|>
    (wstring "iOS" >>= mkString)
    <|>
    (wstring "watchOS" >>= mkString)
    <|>
    (wstring "tvOS" >>= mkString)
  )

(*| architecture -> "i386" | "x86_64" | "arm" | "arm64" |*)
and architecture () =
  mkNode "Architecture"
  <:> mkProp "Value" (
    (wstring "i386" >>= mkString)
    <|>
    (wstring "x86_64" >>= mkString)
    <|>
    (wstring "arm" >>= mkString)
    <|>
    (wstring "arm64" >>= mkString)
  )

(*| swift-version -> decimal-digits "." decimal-digits |*)
and swiftVersion () =
  mkNode "SwiftVersion"
  <:> mkPropE "Major" decimalDigits
  <* wchar '.'
  <:> mkPropE "Minor" decimalDigits

(*| GRAMMAR OF A LINE CONTROL STATEMENT |*)

(*| line-control-statement -> "#sourceLocation" "(" "file:" file-name "," "line:" line-number ")" |*)
(*| line-control-statement -> "#sourceLocation" "(" ")" |*)
and lineControlStatement () =
  wstring "#sourceLocation"
  *> wchar '('
  *> mkNode "LineControlStatement"
  <:> (
    mkOptPropEmpty (
      mkPropHolder
      <:> mkProp "FileName" (wstring "file:" *> fileName ())
      <:> mkProp "LineNumber" (wstring "file:" *> lineNumber ())
    )
  ) <* wchar ')'

(*| line-number -> A decimal integer greater than zero |*)
and lineNumber () = decimalDigits ()

(*| file-name -> static-string-literal |*)
and fileName () = staticStringLiteral ()

(*| GRAMMAR OF AN AVAILABILITY CONDITION |*)

(*| availability-condition -> "#available" "(" availability-arguments ")" |*)
and availabilityCondition () =
  mkNode "AvailabilityCondition"
  <* wstring "#available"
  <* wchar '('
  <:> mkPropE "Arguments" availabilityArguments
  <* wchar ')'

(*| availability-arguments -> availability-argument | availability-argument "," availability-arguments |*)
and availabilityArguments () =
  commaSep availabilityArgument

(*| availability-argument -> platform-name platform-version |*)
(*| availability-argument -> "*" |*)
and availabilityArgument () =
  mkNode "AvailabilityArgument"
  <:> (
    (
      mkPropHolder
      <:> platformName ()
      <:> mkPropE "Version" platformVersion
    ) <|> (
      mkBoolProp "Wildcard" (wchar '*')
    )
  )

(*| platform-name -> "iOS" | "iOSApplicationExtension" |*)
(*| platform-name -> "macOS" | "macOSApplicationExtension" |*)
(*| platform-name -> "watchOS" |*)
(*| platform-name -> "tvOS" |*)
and platformName () =
  let plat str =
    mkBoolProp str (wstring str)
  in
  plat "iOS"
  <|> plat "iOSApplicationExtension"
  <|> plat "macOS"
  <|> plat "macOSApplicationExtension"
  <|> plat "watchOS"
  <|> plat "tvOS"

(*| platform-version -> decimal-digits |*)
(*| platform-version -> decimal-digits "." decimal-digits |*)
(*| platform-version -> decimal-digits "." decimal-digits "." decimal-digits |*)
and platformVersion () =
  mkNode "PlatformVersion"
  <* whitespace
  <:> mkPropE "Major" decimalDigits
  <:> mkOptProp "Minor" (wchar '.' *> decimalDigits ())
  <:> mkOptProp "Patch" (wchar '.' *> decimalDigits ())

(*| GRAMMAR OF AN IMPORT DECLARATION |*)

(*| import-path-identifier -> identifier | operator |*)
and importPathIdentifier () =
  identifier ()
  <|> operator ()

(*| import-path -> import-path-identifier | import-path-identifier "." import-path |*)
and importPath () =
  sepBy1 '.' importPathIdentifier >>= toList

(*| import-kind -> "typealias" | "struct" | "class" | "enum" | "protocol" | "var" | "func" |*)
and importKind () =
  wstring "typealias"
  <|> wstring "struct"
  <|> wstring "class"
  <|> wstring "enum"
  <|> wstring "protocol"
  <|> wstring "var"
  <|> wstring "func"

(*| import-declaration -> attributes ??? "import" import-kind ??? import-path |*)
and importDeclaration () =
  mkNode "ImportDeclaration"
  <:> mkOptProp "Attributes" (attributes ())
  <* wstring "import"
  <:> mkOptProp "Kind" (importKind () >>= mkString)
  <:> mkProp "ImportPath" (importPath ())

(*| GRAMMAR OF A CONSTANT DECLARATION |*)

(*| constant-declaration -> attributes ??? declaration-modifiers ??? "let" pattern-initializer-list |*)
and constantDeclaration () =
  mkNode "ConstantDeclaration"
  <:> mkOptPropE "Attributes" attributes
  <:> mkOptPropEmptyE declarationModifiers
  <* wstring "let"
  <:> mkPropE "PatternInitializerList" patternInitializerList

(*| pattern-initializer-list -> pattern-initializer | pattern-initializer "," pattern-initializer-list |*)
and patternInitializerList () =
  commaSep patternInitializer

(*| pattern-initializer -> pattern initializer ??? |*)
and patternInitializer () =
  mkNode "PatternInitializer"
  <:> mkProp "Pattern" (fix (pattern ~allowExpression:false ~allowAssignment:false))
  <:> mkOptPropE "Initializer" initializer'

(*| initializer -> "=" expression |*)
and initializer' ?allowTrailingClosure:(allowTrailingClosure:bool = true) () =
  wchar '=' *> fix (expression ~allowTrailingClosure)

(*| GRAMMAR OF A VARIABLE DECLARATION |*)

(*| variable-declaration -> variable-declaration-head pattern-initializer-list |*)
(*| variable-declaration -> variable-declaration-head variable-name type-annotation code-block |*)
(*| variable-declaration -> variable-declaration-head variable-name type-annotation getter-setter-block |*)
(*| variable-declaration -> variable-declaration-head variable-name type-annotation getter-setter-keyword-block |*)
(*| variable-declaration -> variable-declaration-head variable-name initializer willSet-didSet-block |*)
(*| variable-declaration -> variable-declaration-head variable-name type-annotation initializer ??? willSet-didSet-block |*)
and variableDeclaration () =
  mkNode "VariableDeclaration"
  <:> variableDeclarationHead ()
  <:> (
    (
      mkPropHolder
      <:> mkPropE "VariableName" variableName
      <:> mkPropE "TypeAnnotation" typeAnnotation
      <:> mkProp "Initializer" (
        getterSetterBlock()
        <|> getterSetterKeywordBlock()
        <|> codeBlock ()
      )
    ) <|> (
      mkPropHolder
      <:> mkPropE "VariableName" variableName
      <:> mkPropE "Initializer" initializer'
      <:> willSetDidSetBlock()
    ) <|> (
      mkPropHolder
      <:> mkPropE "VariableName" variableName
      <:> mkPropE "TypeAnnotation" typeAnnotation
      <:> mkOptPropE "Initializer" initializer'
      <:> willSetDidSetBlock()
    )
    <|> mkPropE "PatternInitializerList" patternInitializerList
  )

(*| variable-declaration-head -> attributes ??? declaration-modifiers ??? "var" |*)
and variableDeclarationHead () =
  mkPropHolder
  <:> mkOptPropE "Attributes" attributes
  <:> mkOptPropEmptyE declarationModifiers
  <* wstring "var"

(*| variable-name -> identifier |*)
and variableName () =
  identifier ()

(*| getter-setter-block -> code-block |*)
(*| getter-setter-block -> "{" getter-clause setter-clause ??? "}" |*)
(*| getter-setter-block -> "{" setter-clause getter-clause "}" |*)
and getterSetterBlock () =
  (
    wchar '{'
    *> mkNode "GetterSetterBlock"
    <:> mkPropE "GetterClause" getterClause
    <:> mkOptPropE "SetterClause" setterClause
    <* wchar '}'
  ) <|> (
    wchar '{'
    *> mkNode "GetterSetterBlock"
    <:> mkPropE "SetterClause" setterClause
    <:> mkPropE "GetterClause" getterClause
    <* wchar '}'
  ) <|> codeBlock ()

(*| getter-clause -> attributes ??? mutation-modifier ??? "get" code-block |*)
and getterClause () =
  mkNode "GetterBlock"
  <:> mkOptPropE "Attributes" attributes
  <:> mkOptPropEmptyE mutationModifier
  <* wstring "get"
  <:> mkPropE "Body" codeBlock

(*| setter-clause -> attributes ??? mutation-modifier ??? "set" setter-name ??? code-block |*)
and setterClause () =
  mkNode "SetterBlock"
  <:> mkOptPropE "Attributes" attributes
  <:> mkOptPropEmptyE mutationModifier
  <* wstring "set"
  <:> mkOptPropE "Name" setterName
  <:> mkPropE "Body" codeBlock

(*| setter-name -> "(" identifier ")" |*)
and setterName () =
  wchar '(' *> identifier () <* wchar ')'

(*| getter-setter-keyword-block -> "{" getter-keyword-clause setter-keyword-clause ??? "}" |*)
(*| getter-setter-keyword-block -> "{" setter-keyword-clause getter-keyword-clause "}" |*)
and getterSetterKeywordBlock () =
  (
    wchar '{'
    *> mkNode "GetterSetterKeywordBlock"
    <:> mkPropE "GetterKeywordClause" getterKeywordClause
    <:> mkOptPropE "SetterKeywordClause" setterKeywordClause
    <* wchar '}'
  ) <|> (
    wchar '{'
    *> mkNode "GetterSetterKeywordBlock"
    <:> mkPropE "SetterKeywordClause" setterKeywordClause
    <:> mkPropE "GetterKeywordClause" getterKeywordClause
    <* wchar '}'
  )

(*| getter-keyword-clause -> attributes ??? mutation-modifier ??? "get" |*)
and getterKeywordClause () =
  mkNode "GetterKeywordBlock"
  <:> mkOptPropE "Attributes" attributes
  <:> mkOptPropEmptyE mutationModifier
  <* wstring "get"

(*| setter-keyword-clause -> attributes ??? mutation-modifier ??? "set" |*)
and setterKeywordClause () =
  mkNode "GetterKeywordBlock"
  <:> mkOptPropE "Attributes" attributes
  <:> mkOptPropEmptyE mutationModifier
  <* wstring "set"

(*| willSet-didSet-block -> "{" willSet-clause didSet-clause ??? "}" |*)
(*| willSet-didSet-block -> "{" didSet-clause willSet-clause ??? "}" |*)
and willSetDidSetBlock () =
  wchar '{' *>
  (
    (
      mkPropHolder
      <:> mkPropE "WillSetClause" willSetClause
      <:> mkOptPropE "DidSetClause" didSetClause
    ) <|> (
      mkPropHolder
      <:> mkPropE "DidSetClause" didSetClause
      <:> mkOptPropE "WillSetClause" willSetClause
    )
  )
  <* wchar '}'

(*| willSet-clause -> attributes ??? "willSet" setter-name ??? code-block |*)
and willSetClause () =
  mkNode "WillSetClause"
  <:> mkOptPropE "Attributes" attributes
  <* wstring "willSet"
  <:> mkOptPropE "SetterName" setterName
  <:> mkPropE "CodeBlock" codeBlock

(*| didSet-clause -> attributes ??? "didSet" setter-name ??? code-block |*)
and didSetClause () =
  mkNode "DidSetClause"
  <:> mkOptPropE "Attributes" attributes
  <* wstring "didSet"
  <:> mkOptPropE "SetterName" setterName
  <:> mkPropE "CodeBlock" codeBlock

(*| GRAMMAR OF A TYPE ALIAS DECLARATION |*)

(*| typealias-declaration -> attributes ??? access-level-modifier ??? "typealias" typealias-name generic-parameter-clause ??? typealias-assignment |*)
and typealiasDeclaration () =
  mkNode "TypealiasDeclaration"
  <:> mkOptPropE "Attributes" attributes
  <:> mkOptPropEmptyE accessLevelModifier
  <* wstring "typealias"
  <:> mkPropE "TypealiasName" typealiasName
  <:> mkOptPropE "GenericParameterClause" genericParameterClause
  <:> mkPropE "TypealiasAssignment" typealiasAssignment

(*| typealias-name -> identifier |*)
and typealiasName () =
  identifier ()

(*| typealias-assignment -> "=" type |*)
and typealiasAssignment () =
  wchar '=' *> fix type'

(*| GRAMMAR OF A CODE BLOCK |*)

(*| code-block -> "{" statements ??? "}" |*)
and codeBlock () =
  wchar '{' *> mkOpt (fix statements) <* wchar '}'

(*| GRAMMAR OF A FUNCTION DECLARATION |*)

(*| function-head -> attributes ??? declaration-modifiers ??? "func" |*)
and functionHead () =
  mkPropHolder
  <* anyspace
  <:> mkOptPropE "Attributes" attributes
  <:> mkOptPropEmptyE declarationModifiers
  <* wstring "func"

(*| function-name -> identifier | operator |*)
and functionName () =
  (
    mkPropE "FunctionName" identifier
    <* anyspace
    <:> mkOptPropE "GenericParameterClause" genericParameterClause
  ) <|> operatorWithGenerics ()

(*| function-result -> "->" attributes ??? type |*)
and functionResult () =
  wfstring "->"
  *> mkNode "FunctionResult"
  <:> mkOptPropE "Attributes" attributes
  <:> mkProp "Type" (fix type')

(*| function-body -> code-block |*)
and functionBody () = codeBlock ()

(*| external-parameter-name -> identifier |*)
and externalParameterName () = paramName ()

(*| local-parameter-name -> identifier |*)
and localParameterName () = paramName ()

(*| default-argument-clause -> "=" expression |*)
and defaultArgumentClause () =
  mkNode "DefaultArgumentClause"
  <* wchar '='
  <:> mkProp "Expression" (fix expression)

(*| parameter -> external-parameter-name ??? local-parameter-name type-annotation default-argument-clause ??? |*)
(*| parameter -> external-parameter-name ??? local-parameter-name type-annotation |*)
(*| parameter -> external-parameter-name ??? local-parameter-name type-annotation "..." |*)
and parameter () =
  mkNode "Parameter"
  <:> (
    (
      mkPropE "ExternalParameterName" externalParameterName
      <:> mkPropE "LocalParameterName" localParameterName
    )
    <|> mkPropE "LocalParameterName" localParameterName
  )
  <:> mkPropE "TypeAnnotation" typeAnnotation
  <:> mkOptPropEmpty (
    mkPropE "DefaultArgumentClause" defaultArgumentClause
    <|> mkBoolProp "Variadic" (wstring "...")
  )

(*| parameter-list -> parameter | parameter "," parameter-list |*)
and parameterList () =
  commaSep parameter

(*| parameter-clause -> "(" ")" | "(" parameter-list ")" |*)
and parameterClause () =
  wchar '(' *> mkOptE parameterList <* wchar ')'


(*| function-signature -> parameter-clause function-properties function-result??? |*)
and functionSignature () =
  mkPropHolder
  <:> mkPropE "ParameterClause" parameterClause
  <:> functionProperties ()
  <:> mkOptPropE "FunctionResult" functionResult

(*| function-declaration -> function-head function-name generic-parameter-clause ??? function-signature generic-where-clause ??? function-body ??? |*)
and functionDeclaration () =
  mkNode "FunctionDeclaration"
  <:> functionHead ()
  <:> functionName ()
  <:> mkOptPropE "genericParameterClause" genericParameterClause
  <:> functionSignature ()
  <:> mkOptPropE "GenericWhereClause" genericWhereClause
  <:> mkOptPropE "FunctionBody" functionBody

(*| GRAMMAR OF AN ENUMERATION DECLARATION |*)

(*| enum-declaration -> attributes ??? access-level-modifier ??? union-style-enum |*)
(*| enum-declaration -> attributes ??? access-level-modifier ??? raw-value-style-enum |*)
and enumDeclaration () =
  mkNode "EnumDeclaration"
  <:> mkOptPropE "Attributes" attributes
  <:> mkOptPropEmptyE accessLevelModifier
  <:> (
    unionStyleEnum ()
    <|> rawValueStyleEnum ()
  )

(*| union-style-enum -> "indirect ???" "enum" enum-name generic-parameter-clause ??? type-inheritance-clause ??? generic-where-clause ??? "{" union-style-enum-members ??? "}" |*)
and unionStyleEnum () =
  mkPropHolder
  <:> mkOptPropEmpty (mkBoolProp "Indirect" (wstring "indirect"))
  <* wstring "enum"
  <:> mkPropE "Name" enumName
  <:> mkOptPropE "genericParameterClause" genericParameterClause
  <:> mkOptPropE "TypeInheritanceClause" typeInheritanceClause
  <* wchar '{'
  <:> mkOptPropE "Members" unionStyleEnumMembers
  <* wchar '}'

(*| union-style-enum-members -> union-style-enum-member union-style-enum-members ??? |*)
and unionStyleEnumMembers () =
  mkList1 unionStyleEnumMember

(*| union-style-enum-member -> declaration | union-style-enum-case-clause | compiler-control-statement |*)
and unionStyleEnumMember () =
  fix declaration
  <|> unionStyleEnumCaseClause ()
  <|> compilerControlStatement ()
  <* optSemi

(*| union-style-enum-case-clause -> attributes ??? "indirect ???" "case" union-style-enum-case-list |*)
and unionStyleEnumCaseClause () =
  mkNode "EnumCaseClause"
  <:> mkOptPropE "Attributes" attributes
  <:> mkOptPropEmpty (mkBoolProp "Indirect" (wstring "indirect"))
  <* wstring "case"
  <:> mkPropE "Cases" unionStyleEnumCaseList

(*| union-style-enum-case-list -> union-style-enum-case | union-style-enum-case "," union-style-enum-case-list |*)
and unionStyleEnumCaseList () =
  commaSep unionStyleEnumCase

(*| union-style-enum-case -> enum-case-name tuple-type ??? |*)
and unionStyleEnumCase () =
  mkNode "EnumCase"
  <:> mkPropE "Name" enumCaseName
  <:> mkOptPropE "Type" tupleType

(*| enum-name -> identifier |*)
and enumName () =
  identifier ()

(*| enum-case-name -> identifier |*)
and enumCaseName () =
  identifier ()

(*| raw-value-style-enum -> "enum" enum-name generic-parameter-clause ??? type-inheritance-clause generic-where-clause ??? "{" raw-value-style-enum-members "}" |*)
and rawValueStyleEnum () =
  mkPropHolder
  <* wstring "enum"
  <:> mkPropE "Name" enumName
  <:> mkOptPropE "GenericParameterClause" genericParameterClause
  <:> mkPropE "TypeInheritanceClause" typeInheritanceClause
  <:> mkOptPropE "GenericWhereClause" genericWhereClause
  <* wchar '{'
  <:> mkPropE "Members" rawValueStyleEnumMembers
  <* wchar '}'

(*| raw-value-style-enum-members -> raw-value-style-enum-member raw-value-style-enum-members ??? |*)
and rawValueStyleEnumMembers () =
  mkList1 rawValueStyleEnumMember

(*| raw-value-style-enum-member -> declaration | raw-value-style-enum-case-clause | compiler-control-statement |*)
and rawValueStyleEnumMember () =
  fix declaration
  <|> rawValueStyleEnumCaseClause ()
  <|> compilerControlStatement ()
  <* optSemi

(*| raw-value-style-enum-case-clause -> attributes ??? "case" raw-value-style-enum-case-list |*)
and rawValueStyleEnumCaseClause () =
  mkNode "EnumCaseClause"
  <:> mkOptPropE "Attributes" attributes
  <* wstring "case"
  <:> mkPropE "Cases" rawValueStyleEnumCaseList

(*| raw-value-style-enum-case-list -> raw-value-style-enum-case | raw-value-style-enum-case "," raw-value-style-enum-case-list |*)
and rawValueStyleEnumCaseList () =
  commaSep rawValueStyleEnumCase

(*| raw-value-style-enum-case -> enum-case-name raw-value-assignment ??? |*)
and rawValueStyleEnumCase () =
  mkNode "EnumCase"
  <:> mkPropE "Name" enumCaseName
  <:> mkOptPropE "Value" rawValueAssignment

(*| raw-value-assignment -> "=" raw-value-literal |*)
and rawValueAssignment () =
  wchar '=' *> rawValueLiteral ()

(*| raw-value-literal -> numeric-literal | static-string-literal | boolean-literal |*)
and rawValueLiteral () =
  numericLiteral ()
  <!> staticStringLiteral
  <!> booleanLiteral

(*| GRAMMAR OF A STRUCTURE DECLARATION |*)

(*| struct-declaration -> attributes ??? access-level-modifier ??? "struct" struct-name generic-parameter-clause ??? type-inheritance-clause ??? generic-where-clause ??? struct-body |*)
and structDeclaration () =
  mkNode "StructDeclaration"
  <:> mkOptPropE "Attributes" attributes
  <:> mkOptPropEmptyE accessLevelModifier
  <* wstring "struct"
  <:> mkPropE "Name" structName
  <:> mkOptPropE "GenericParameterClause" genericParameterClause
  <:> mkOptPropE "TypeInheritanceClause" typeInheritanceClause
  <:> mkOptPropE "GenericWhereClause" genericWhereClause
  <:> mkPropE "Body" structBody

(*| struct-name -> identifier |*)
and structName () =
  identifier ()

(*| struct-body -> "{" struct-members ??? "}" |*)
and structBody () =
  wchar '{' *> mkOpt (commit *> structMembers ()) <* wchar '}'

(*| struct-members -> struct-member struct-members ??? |*)
and structMembers () =
  mkList1 structMember

(*| struct-member -> declaration | compiler-control-statement |*)
and structMember () =
  fix declaration
  <!> compilerControlStatement
  <* optSemi

(*| GRAMMAR OF A CLASS DECLARATION |*)

(*| class-name -> identifier |*)
and className () = identifier ()

(*| class-member -> declaration | compiler-control-statement |*)
and classMember () =
  fix declaration
  <!> compilerControlStatement
  <* optSemi

(*| class-members -> class-member class-members ??? |*)
and classMembers () =
  mkList1 (fun () -> commit *> classMember ())

(*| class-body -> "{" class-members ??? "}" |*)
and classBody () =
  wchar '{' *> mkOptE classMembers <* wchar '}'

(*| reference-type -> class | actor |*)
and referenceType () =
  (
    mkBoolProp "Actor" (wstring "actor")
    <|> (wstring "class" *> mkPropHolder)
  )

(*| class-declaration -> attributes??? access-level-modifier??? "final"??? "class" class-name generic-parameter-clause??? type-inheritance-clause??? generic-where-clause??? class-body |*)
(*| class-declaration -> attributes??? "final" access-level-modifier??? "class" class-name generic-parameter-clause??? type-inheritance-clause??? generic-where-clause??? class-body |*)
and classDeclaration () =
  mkNode "ClassDeclaration"
  <:> mkOptPropE "Attributes" attributes
  <:> (
    (
      mkBoolProp "Final" (wstring "final")
      <:> mkOptPropEmptyE accessLevelModifier
    ) <|> (
      mkOptPropEmptyE accessLevelModifier
      <:> mkOptPropEmpty (mkBoolProp "Final" (wstring "final"))
    )
  )
  <:> referenceType ()
  <:> mkPropE "ClassName" className
  <:> mkOptPropE "GenericParameterClause" genericParameterClause
  <:> mkOptPropE "TypeInheritanceClause" typeInheritanceClause
  <:> mkOptPropE "GenericWhereClause" genericWhereClause
  <:> mkPropE "ClassBody" classBody

(*| GRAMMAR OF A PROTOCOL DECLARATION |*)

(*| protocol-declaration -> attributes ??? access-level-modifier ??? "protocol" protocol-name type-inheritance-clause ??? protocol-body |*)
and protocolDeclaration () =
  mkNode "ProtocolDeclaration"
  <:> mkOptPropE "Attributes" attributes
  <:> mkOptPropEmptyE accessLevelModifier
  <* wstring "protocol"
  <:> mkPropE "Name" protocolName
  <:> mkOptPropE "TypeInheritanceClause" typeInheritanceClause
  <:> mkPropE "ProtocolBody" protocolBody

(*| protocol-name -> identifier |*)
and protocolName () = identifier ()

(*| protocol-body -> "{" protocol-members ??? "}" |*)
and protocolBody () =
  wchar '{' *> mkOptE protocolMembers <* wchar '}'

(*| protocol-members -> protocol-member protocol-members ??? |*)
and protocolMembers () =
  mkList1 protocolMember

(*| protocol-member -> protocol-member-declaration | compiler-control-statement |*)
and protocolMember () =
  protocolMemberDeclaration ()
  <|>
  compilerControlStatement ()

(*| protocol-member-declaration -> protocol-property-declaration |*)
(*| protocol-member-declaration -> protocol-method-declaration |*)
(*| protocol-member-declaration -> protocol-initializer-declaration |*)
(*| protocol-member-declaration -> protocol-subscript-declaration |*)
(*| protocol-member-declaration -> protocol-associated-type-declaration |*)
(*| protocol-member-declaration -> typealias-declaration |*)
and protocolMemberDeclaration () =
  protocolPropertyDeclaration ()
  <!> protocolMethodDeclaration
  <!> protocolInitializerDeclaration
  <!> protocolSubscriptDeclaration
  <!> protocolAssociatedTypeDeclaration
  <!> typealiasDeclaration

(*| GRAMMAR OF A PROTOCOL PROPERTY DECLARATION |*)

(*| protocol-property-declaration -> variable-declaration-head variable-name type-annotation getter-setter-keyword-block |*)
and protocolPropertyDeclaration () =
  mkNode "ProtocolPropertyDeclaration"
  <:> variableDeclarationHead ()
  <:> mkPropE "VariableName" variableName
  <:> mkPropE "TypeAnnotation" typeAnnotation
  <:> mkPropE "GetterSetterKeywordBlock" getterSetterKeywordBlock

(*| GRAMMAR OF A PROTOCOL METHOD DECLARATION |*)

(*| protocol-method-declaration -> function-head function-name generic-parameter-clause ??? function-signature generic-where-clause ??? |*)
and protocolMethodDeclaration () =
  mkNode "ProtocolMethodDeclaration"
  <:> functionHead ()
  <:> functionName ()
  <:> mkOptPropE "GenericParameterClause" genericParameterClause
  <:> functionSignature ()
  <:> mkOptPropE "GenericWhereClause" genericWhereClause

(*| GRAMMAR OF A PROTOCOL INITIALIZER DECLARATION |*)

(*| protocol-initializer-declaration -> initializer-head generic-parameter-clause ??? parameter-clause function-properties generic-where-clause ??? |*)
and protocolInitializerDeclaration () =
  mkNode "ProtocolInitializerDeclaration"
  <:> initializerHead ()
  <:> mkOptPropE "GenericParameterClause" genericParameterClause
  <:> mkPropE "ParameterClause" parameterClause
  <:> functionProperties ()
  <:> mkOptPropE "GenericWhereClause" genericWhereClause

(*| GRAMMAR OF A PROTOCOL SUBSCRIPT DECLARATION |*)

(*| protocol-subscript-declaration -> subscript-head subscript-result getter-setter-keyword-block |*)
and protocolSubscriptDeclaration () =
  mkNode "ProtocolSubscriptDeclaration"
  <:> subscriptHead ()
  <:> mkPropE "SubscriptResult" subscriptResult
  <:> mkPropE "GetterSetterKeywordBlock" getterSetterKeywordBlock

(*| GRAMMAR OF A PROTOCOL ASSOCIATED TYPE DECLARATION |*)

(*| protocol-associated-type-declaration -> attributes ??? access-level-modifier ??? "associatedtype" typealias-name type-inheritance-clause ??? typealias-assignment ??? |*)
and protocolAssociatedTypeDeclaration () =
  mkNode "ProtocolAssociatedTypeDeclaration"
  <:> mkOptPropE "Attributes" attributes
  <:> mkOptPropEmptyE accessLevelModifier
  <* wstring "associatedtype"
  <:> mkPropE "TypealiasName" typealiasName
  <:> mkOptPropE "TypeInheritanceClause" typeInheritanceClause
  <:> mkOptPropE "TypealiasAssignment" typealiasAssignment

(*| GRAMMAR OF AN INITIALIZER DECLARATION |*)

(*| initializer-declaration -> initializer-head generic-parameter-clause ??? parameter-clause function-properties generic-where-clause ??? initializer-body |*)
and initializerDeclaration () =
  mkNode "InitializerDeclaration"
  <:> initializerHead ()
  <:> mkOptPropE "GenericParameterClause" genericParameterClause
  <:> mkPropE "ParameterClause" parameterClause
  <:> mkOptPropEmpty (
    mkBoolProp "Throws" (wstring "throws")
    <|>
    mkBoolProp "Rethrows" (wstring "rethrows")
  )
  <:> functionProperties ()
  <:> mkOptPropE "GenericWhereClause" genericWhereClause
  <:> mkPropE "InitializerBody" initializerBody

(*| initializer-head -> attributes ??? declaration-modifiers ??? "init" |*)
(*| initializer-head -> attributes ??? declaration-modifiers ??? "init" "?" |*)
(*| initializer-head -> attributes ??? declaration-modifiers ??? "init" "!" |*)
and initializerHead () =
  (
    mkPropHolder
    <:> mkOptPropE "Attributes" attributes
    <:> mkOptPropEmptyE declarationModifiers
  )
  <* wstring "init"
  <:> mkOptPropEmpty (
    mkBoolProp "Optional" (char '?')
    <|>
    mkBoolProp "Force" (char '!')
  )

(*| initializer-body -> code-block |*)
and initializerBody () = codeBlock ()

(*| GRAMMAR OF A DEINITIALIZER DECLARATION |*)

(*| deinitializer-declaration -> attributes ??? "deinit" code-block |*)
and deinitializerDeclaration () =
  mkNode "DeinitializerDeclaration"
  <:> mkOptPropE "Attributes" attributes
  <* wstring "deinit"
  <:> mkPropE "Body" codeBlock

(*| GRAMMAR OF AN EXTENSION DECLARATION |*)

(*| extension-declaration -> attributes ??? access-level-modifier ??? "extension" type-identifier type-inheritance-clause ??? extension-body |*)
(*| extension-declaration -> attributes ??? access-level-modifier ??? "extension" type-identifier generic-where-clause extension-body |*)
and extensionDeclaration () =
  mkNode "ExtensionDeclaration"
  <:> mkOptPropE "Attributes" attributes
  <:> mkOptPropEmptyE accessLevelModifier
  <* wstring "extension"
  <:> mkPropE "Name" typeIdentifier
  <:> mkOptPropEmpty (
    mkPropE "TypeInheritanceClause" typeInheritanceClause
    <|>
    mkPropE "GenericWhereClause" genericWhereClause
  )
  <:> mkPropE "Body" extensionBody

(*| extension-body -> "{" extension-members ??? "}" |*)
and extensionBody () =
  wchar '{' *> mkOptE extensionMembers <* wchar '}'

(*| extension-members -> extension-member extension-members ??? |*)
and extensionMembers () =
  mkList1 (fun () -> commit *> extensionMember ())

(*| extension-member -> declaration | compiler-control-statement |*)
and extensionMember () =
  fix declaration
  <!> compilerControlStatement
  <* optSemi

(*| GRAMMAR OF A SUBSCRIPT DECLARATION |*)

(*| subscript-declaration -> subscript-head subscript-result code-block |*)
(*| subscript-declaration -> subscript-head subscript-result getter-setter-block |*)
(*| subscript-declaration -> subscript-head subscript-result getter-setter-keyword-block |*)
and subscriptDeclaration () =
  mkNode "SubscriptDeclaration"
  <:> subscriptHead ()
  <:> mkPropE "ResultType" subscriptResult
  <:> mkProp "Body" (
    codeBlock ()
    <!> getterSetterBlock
    <!> getterSetterKeywordBlock
  )

(*| subscript-head -> attributes ??? declaration-modifiers ??? "subscript" parameter-clause |*)
and subscriptHead () =
  mkPropHolder
  <|> mkOptPropE "Attributes" attributes
  <:> mkOptPropEmptyE declarationModifiers
  <* wstring "subscript"
  <:> mkPropE "ParameterClause" parameterClause

(*| subscript-result -> "->" attributes ??? type |*)
and subscriptResult () =
  wfstring "->"
  *> mkNode "SubscriptResult"
  <:> mkOptPropE "Attributes" attributes
  <:> mkProp "Type" (fix type')

(*| GRAMMAR OF AN OPERATOR DECLARATION |*)

(*| operator-declaration -> prefix-operator-declaration | postfix-operator-declaration | infix-operator-declaration |*)
and operatorDeclaration () =
  prefixOperatorDeclaration ()
  <!> postfixOperatorDeclaration
  <!> infixOperatorDeclaration

(*| prefix-operator-declaration -> "prefix" "operator" operator |*)
and prefixOperatorDeclaration () =
  mkNode "PrefixOperatorDeclaration"
  <* wstring "prefix"
  <* wstring "operator"
  <:> mkPropE "Operator" operator

(*| postfix-operator-declaration -> "postfix" "operator" operator |*)
and postfixOperatorDeclaration () =
  mkNode "PostfixOperatorDeclaration"
  <* wstring "postfix"
  <* wstring "operator"
  <:> mkPropE "Operator" operator

(*| infix-operator-declaration -> "infix" "operator" operator infix-operator-group ??? |*)
and infixOperatorDeclaration () =
  mkNode "InfixOperatorDeclaration"
  <* wstring "infix"
  <* wstring "operator"
  <:> mkPropE "Operator" operator
  <:> mkOptPropE "Group" infixOperatorGroup

(*| infix-operator-group -> ":" precedence-group-name |*)
and infixOperatorGroup () =
  wchar ':' *> precedenceGroupName ()

(*| GRAMMAR OF A PRECEDENCE GROUP DECLARATION |*)

(*| precedence-group-declaration -> "precedencegroup" precedence-group-name "{" precedence-group-attributes ??? "}" |*)
and precedenceGroupDeclaration () =
  mkNode "PrecedenceGroupDeclaration"
  <* wstring "precedencegroup"
  <:> mkPropE "Name" precedenceGroupName
  <* wchar '{'
  <:> mkOptPropE "Attributes" precedenceGroupAttributes
  <* wchar '}'

(*| precedence-group-attributes -> precedence-group-attribute precedence-group-attributes ??? |*)
and precedenceGroupAttributes () =
  mkList1 precedenceGroupAttribute

(*| precedence-group-attribute -> precedence-group-relation |*)
(*| precedence-group-attribute -> precedence-group-assignment |*)
(*| precedence-group-attribute -> precedence-group-associativity |*)
and precedenceGroupAttribute () =
  precedenceGroupRelation ()
  <!> precedenceGroupAssignment
  <!> precedenceGroupAssociativity

(*| precedence-group-relation -> "higherThan" ":" precedence-group-names |*)
(*| precedence-group-relation -> "lowerThan" ":" precedence-group-names |*)
and precedenceGroupRelation () =
  (
    mkNode "HigherThanPrecedenceRelation"
    <* wstring "higherThan"
    <* wchar ':'
    <:> mkPropE "Relatives" precedenceGroupNames
  ) <|> (
    mkNode "LowerThanPrecedenceRelation"
    <* wstring "lowerThan"
    <* wchar ':'
    <:> mkPropE "Relatives" precedenceGroupNames
  )

(*| precedence-group-assignment -> "assignment" ":" boolean-literal |*)
and precedenceGroupAssignment () =
  mkNode "PrecedenceGroupAssignment"
  <* wstring "assignment"
  <* wchar ':'
  <:> mkPropE "Value" booleanLiteral

(*| precedence-group-associativity -> "associativity" ":" "left" |*)
(*| precedence-group-associativity -> "associativity" ":" "right" |*)
(*| precedence-group-associativity -> "associativity" ":" "none" |*)
and precedenceGroupAssociativity () =
  wstring "associativity" *>
  wchar ':' *> (
    (wstring "left" *> mkNode "LeftAssociative")
    <|>
    (wstring "right" *> mkNode "RightAssociative")
    <|>
    (wstring "none" *> mkNode "NonAssociative")
  )
(*| precedence-group-names -> precedence-group-name | precedence-group-name "," precedence-group-names |*)
and precedenceGroupNames () =
  commaSep precedenceGroupName

(*| precedence-group-name -> identifier |*)
and precedenceGroupName () =
  identifier ()

(*| GRAMMAR OF A DECLARATION MODIFIER |*)

(*| declaration-modifier -> "class" | "convenience" | "dynamic" | "final" | "infix" | "lazy" | "optional" | "override" | "postfix" | "prefix" | "required" | "static" | "unowned" | "unowned" "(" "safe" ")" | "unowned" "(" "unsafe" ")" | "weak" |*)
(*| declaration-modifier -> access-level-modifier |*)
(*| declaration-modifier -> mutation-modifier |*)
and declarationModifier () =
  mkBoolProp "Class" (wstring "class")
  <|>
  mkBoolProp "Convenience" (wstring "convenience")
  <|>
  mkBoolProp "Dynamic" (wstring "dynamic")
  <|>
  mkBoolProp "Final" (wstring "final")
  <|>
  mkBoolProp "Infix" (wstring "infix")
  <|>
  mkBoolProp "Lazy" (wstring "lazy")
  <|>
  mkBoolProp "Optional" (wstring "optional")
  <|>
  mkBoolProp "Override" (wstring "override")
  <|>
  mkBoolProp "Postfix" (wstring "postfix")
  <|>
  mkBoolProp "Prefix" (wstring "prefix")
  <|>
  mkBoolProp "Required" (wstring "required")
  <|>
  mkBoolProp "Static" (wstring "static")
  <|>
  mkBoolProp "Unowned" (wstring "unowned")
  <|>
  mkBoolProp "UnownedSafe" (wstring "unowned" *> wchar '(' *> wstring "safe" *> wchar ')')
  <|>
  mkBoolProp "UnownedUnsafe" (wstring "unowned" *> wchar '(' *> wstring "unsafe" *> wchar ')')
  <|>
  mkBoolProp "Weak" (wstring "weak")
  <|>
  mkBoolProp "ParallelAsync" (wstring "async") 
  <|>
  accessLevelModifier ()
  <|>
  mutationModifier ()

(*| declaration-modifiers -> declaration-modifier declaration-modifiers ??? |*)
and declarationModifiers () =
  many1 declarationModifier >>= fun mods ->
    List.fold_left (fun p m -> p <:> (return m)) mkPropHolder mods

(*| access-level-modifier -> "private" | "private" "(" "set" ")" |*)
(*| access-level-modifier -> "fileprivate" | "fileprivate" "(" "set" ")" |*)
(*| access-level-modifier -> "internal" | "internal" "(" "set" ")" |*)
(*| access-level-modifier -> "public" | "public" "(" "set" ")" |*)
(*| access-level-modifier -> "open" | "open" "(" "set" ")" |*)
and accessLevelModifier () =
  mkPropHolder
  <:> (
    mkBoolProp "PrivateSet" (wstring "private" *> wchar '(' *> wstring "set" *> wchar ')')
    <|> mkBoolProp "Private" (wstring "private")
    <|> mkBoolProp "FileprivateSet" (wstring "fileprivate" *> wchar '(' *> wstring "set" *> wchar ')')
    <|> mkBoolProp "Fileprivate" (wstring "fileprivate")
    <|> mkBoolProp "InternalSet" (wstring "internal" *> wchar '(' *> wstring "set" *> wchar ')')
    <|> mkBoolProp "Internal" (wstring "internal")
    <|> mkBoolProp "PublicSet" (wstring "public" *> wchar '(' *> wstring "set" *> wchar ')')
    <|> mkBoolProp "Public" (wstring "public")
    <|> mkBoolProp "OpenSet" (wstring "open" *> wchar '(' *> wstring "set" *> wchar ')')
    <|> mkBoolProp "Open" (wstring "open")
  )

(*| mutation-modifier -> "mutating" | "nonmutating" |*)
and mutationModifier () =
  mkBoolProp "Mutating" (wstring "mutating")
  <|>
  mkBoolProp "Nonmutating" (wstring "nonmutating")

(*| Declarations |*)
(*| GRAMMAR OF A DECLARATION |*)

(*| declaration -> import-declaration |*)
(*| declaration -> constant-declaration |*)
(*| declaration -> variable-declaration |*)
(*| declaration -> typealias-declaration |*)
(*| declaration -> function-declaration |*)
(*| declaration -> enum-declaration |*)
(*| declaration -> struct-declaration |*)
(*| declaration -> class-declaration |*)
(*| declaration -> protocol-declaration |*)
(*| declaration -> initializer-declaration |*)
(*| declaration -> deinitializer-declaration |*)
(*| declaration -> extension-declaration |*)
(*| declaration -> subscript-declaration |*)
(*| declaration -> operator-declaration |*)
(*| declaration -> precedence-group-declaration |*)
and declaration _ =
  importDeclaration ()
  <|> constantDeclaration ()
  <|> variableDeclaration ()
  <|> typealiasDeclaration ()
  <|> functionDeclaration ()
  <|> enumDeclaration ()
  <|> structDeclaration ()
  <|> classDeclaration ()
  <|> protocolDeclaration ()
  <|> initializerDeclaration ()
  <|> deinitializerDeclaration ()
  <|> extensionDeclaration ()
  <|> subscriptDeclaration ()
  <|> operatorDeclaration ()
  <|> precedenceGroupDeclaration ()

(*| declarations -> declaration declarations ??? |*)
and declarations () = many1 (fun () -> fix declaration <* optSemi)

(*| Statements |*)
(*| GRAMMAR OF A STATEMENT |*)

(*| statement -> expression "; ???" |*)
(*| statement -> declaration "; ???" |*)
(*| statement -> loop-statement "; ???" |*)
(*| statement -> branch-statement "; ???" |*)
(*| statement -> labeled-statement "; ???" |*)
(*| statement -> control-transfer-statement "; ???" |*)
(*| statement -> defer-statement "; ???" |*)
(*| statement -> do-statement ": ???" |*)
(*| statement -> compiler-control-statement |*)
and statement () =
  fix declaration
  <|> loopStatement ()
  <|> branchStatement ()
  <|> labeledStatement ()
  <|> controlTransferStatement ()
  <|> deferStatement ()
  <|> doStatement ()
  <|> compilerControlStatement ()
  (* leave expression last otherwise any keyword will be matched as an identifier *)
  <|> fix expression
  <* optSemi

(*| statements -> statement statements ??? |*)
and statements _ =
  mkList1 statement

(*| Attributes |*)
(*| GRAMMAR OF AN ATTRIBUTE |*)

(*| attribute -> "@" attribute-name attribute-argument-clause ??? |*)
and attribute () =
  anyspace
  *> wchar '@'
  *> mkNode "Attribute"
  <:> mkPropE "AttributeName" attributeName
  <:> mkOptPropE "AttributeArgumentClause" attributeArgumentClause

(*| attribute-name -> identifier |*)
and attributeName () =
  identifier ()

(*| attribute-argument-clause -> "(" balanced-tokens ??? ")" |*)
and attributeArgumentClause () =
  char '(' *> mkOpt (fix balancedTokens) <* wchar ')'

(*| attributes -> attribute attributes ??? |*)
and attributes () =
  mkList1 attribute <* anyspace

(*| balanced-tokens -> balanced-token balanced-tokens ??? |*)
and balancedTokens _ =
  mkList1 (fun () -> fix balancedToken)

(*| balanced-token -> "(" balanced-tokens ??? ")" |*)
(*| balanced-token -> "[" balanced-tokens ??? "]" |*)
(*| balanced-token -> "{" balanced-tokens ??? "}" |*)
(*| balanced-token -> Any identifier, keyword, literal, or operator |*)
(*| balanced-token -> Any punctuation except "(" , ")" , "[" , "]" , "{" , or "}" |*)
and balancedToken _ =
  wchar '(' *> fix balancedTokens <* wchar '*'
  <|>
  wchar '[' *> fix balancedTokens <* wchar ']'
  <|>
  wchar '{' *> fix balancedTokens <* wchar '}'
  <|> (
    pos >>= fun pos ->
      many1(fun () -> satisfy(function
        | '(' | ')'
        | '[' | ']'
        | '{' | '}'
        -> false
        | _ -> true
      )) >>| fun chars ->
        NodeHolder(pos, string_of_chars chars)
  )

(*| GRAMMAR OF A PATTERN |*)

(*| pattern -> wildcard-pattern type-annotation ??? |*)
(*| pattern -> identifier-pattern type-annotation ??? |*)
(*| pattern -> value-binding-pattern |*)
(*| pattern -> tuple-pattern type-annotation ??? |*)
(*| pattern -> enum-case-pattern |*)
(*| pattern -> optional-pattern |*)
(*| pattern -> type-casting-pattern |*)
(*| pattern -> expression-pattern |*)
and pattern
?allowExpression:(allowExpression=true)
?allowAssignment:(allowAssignment=true)
?allowTrailingClosure:(allowTrailingClosure=true)
?allowTypeAnnotation:(allowTypeAnnotation=true)
?(allowIdentiifer=true)
_
=
  let typeAnnot =
    if allowTypeAnnotation then
      mkOptProp "TypeAnnotation" (typeAnnotation ())
    else
      mkPropHolder
  in
  let rec aux pattern =
    option pattern (asPattern pattern >>= aux)
  in
  (startPattern *> (
    (wildcardPattern () <:> typeAnnot)
    <|> (tuplePattern () <:> typeAnnot)
    <|> fix (valueBindingPattern ~allowTrailingClosure ~allowTypeAnnotation ~allowExpression)
    <|> isPattern ()
    <|> (
      if allowExpression
      then expressionPattern ~allowAssignment ~allowTrailingClosure ~allowTypeAnnotation ()
      else fail "" (* skip to next case *)
    )
    <|> enumCasePattern ()
    <|> (
      if allowIdentiifer
      then identifierPattern () <:> typeAnnot
      else fail "" (* skip to next case *)
    )
    >>= aux
  ) <* stopPattern
  (* TODO: This is a terrible hack to ensure we record when we exit a pattern *)
  (* I should come up with a better fix at some point *)
  ) <|> stopPattern *> fail "Not a pattern"

(*| GRAMMAR OF A WILDCARD PATTERN |*)

(*| wildcard-pattern -> "_" |*)
and wildcardPattern () =
  wstring "_" *> mkNode "WildcardPattern"

(*| GRAMMAR OF AN IDENTIFIER PATTERN |*)

(*| identifier-pattern -> identifier |*)
and identifierPattern () =
  mkNode "IdentifierPattern"
  <:> mkPropE "Identifier" identifier

(*| GRAMMAR OF A VALUE-BINDING PATTERN |*)

(*| value-binding-pattern -> "var" pattern | "let" pattern |*)
and valueBindingPattern ~allowTrailingClosure ~allowTypeAnnotation ~allowExpression _ =
  (
    (wstring "var" *> mkNode "VarBinding")
    <|>
    (wstring "let" *> mkNode "LetBinding")
  )
  <:> mkProp "Pattern" (fix (pattern ~allowTrailingClosure ~allowTypeAnnotation ~allowExpression ~allowAssignment:false))

(*| GRAMMAR OF A TUPLE PATTERN |*)

(*| tuple-pattern -> "(" tuple-pattern-element-list ??? ")" |*)
and tuplePattern () =
  mkNode "TuplePattern"
  <* wchar '('
  <:> mkOptPropE "Elements" tuplePatternElementList
  <* wchar ')'

(*| tuple-pattern-element-list -> tuple-pattern-element | tuple-pattern-element "," tuple-pattern-element-list |*)
and tuplePatternElementList () =
  commaSep tuplePatternElement

(*| tuple-pattern-element -> pattern | identifier ":" pattern |*)
and tuplePatternElement () =
  (
    mkNode "TuplePatternElement"
    <:> mkPropE "Identifier" identifier
    <* wchar ':'
    <:> mkProp "Pattern" (fix pattern)
  ) <|> fix pattern

(*| GRAMMAR OF AN ENUMERATION CASE PATTERN |*)

(*| enum-case-pattern -> type-identifier ??? "." enum-case-name tuple-pattern ??? |*)
and enumCasePattern () =
  mkNode "EnumCasePattern"
  <:> mkProp "EnumCaseName" (
    wchar '.' *> enumCaseName ()
    <|> typeIdentifier ~forceMultiple:true ()
  )
  <:> mkOptPropE "TuplePattern" tuplePattern

(*| GRAMMAR OF AN OPTIONAL PATTERN |*)

(*| optional-pattern -> identifier-pattern "?" |*)

(* actually parsed by the Swift compiler as an expression-pattern with
 * optional-chaining-expression inside *)

(*| GRAMMAR OF A TYPE CASTING PATTERN |*)

(*| type-casting-pattern -> is-pattern | as-pattern |*)
(* Both cases can't be handled together due to incompatible prefixes. *)
(* `is-pattern` and `as-pattern` will be called directly from `pattern` *)
(*and typeCastingPattern () =*)
  (*isPattern () <|> asPattern ()*)

(*| is-pattern -> "is" type |*)
and isPattern () =
  mkNode "IsPattern"
  <* wstring "is"
  <:> mkProp "Type" (fix type')

(*| as-pattern -> pattern "as" type |*)
and asPattern pattern =
  mkNode "AsPattern"
  <:> mkProp "Pattern" (return pattern)
  <* wstring "as"
  <:> mkProp "Type" (fix type')

(*| GRAMMAR OF AN EXPRESSION PATTERN |*)

(*| expression-pattern -> expression |*)
and expressionPattern ~allowAssignment ~allowTrailingClosure ~allowTypeAnnotation () =
  mkNode "ExpressionPattern"
  <:> mkProp "Expression" (fix (expression ~allowAssignment ~allowTrailingClosure ~allowTypeAnnotation))

(*| Generic Parameters and Arguments |*)
(*| GRAMMAR OF A GENERIC PARAMETER CLAUSE |*)

(*| generic-parameter-clause -> "<" generic-parameter-list ">" |*)
and genericParameterClause () =
  wchar '<'
  *> mkNode "GenericParameterClause"
  <:> mkPropE "GenericParameterList" genericParameterList
  <* anyspace
  <* wchar '>'
  <* anyspace

(*| generic-parameter-list -> generic-parameter | generic-parameter "," generic-parameter-list |*)
and genericParameterList () =
  commaSep genericParameter

(*| generic-parameter -> type-name |*)
(*| generic-parameter -> type-name ":" type-identifier |*)
(*| generic-parameter -> type-name ":" protocol-composition-type |*)
and genericParameter () =
  mkNode "GenericParameter"
  <:> mkPropE "TypeName" typeName
  <:> mkOptPropEmpty (
    wchar ':' *> (
      mkProp "TypeIdentifier" (typeIdentifier () >>= fun t -> option t (protocolCompositionType t))
    )
  )

(*| generic-where-clause -> "where" requirement-list |*)
and genericWhereClause () =
  wstring "where"
  *> requirementList ()

(*| requirement-list -> requirement | requirement "," requirement-list |*)
and requirementList () =
  commaSep requirement

(*| requirement -> conformance-requirement | same-type-requirement |*)
and requirement () =
  conformanceRequirement ()
  <|>
  sameTypeRequirement ()

(*| conformance-requirement -> type-identifier ":" type-identifier |*)
(*| conformance-requirement -> type-identifier ":" protocol-composition-type |*)
and conformanceRequirement () =
  mkNode "ConformanceRequirement"
  <:> mkPropE "Lhs" typeIdentifier
  <* wchar ':'
  <:> mkProp "Rhs" (
    typeIdentifier () >>= fun t -> option t (protocolCompositionType t)
  )

(*| same-type-requirement -> type-identifier "==" type |*)
and sameTypeRequirement () =
  mkNode "SameTypeRequirement"
  <:> mkPropE "Lhs" typeIdentifier
  <* wfstring "=="
  <:> mkProp "Rhs" (fix type')

(*| GRAMMAR OF A TOP-LEVEL DECLARATION |*)

(*| top-level-declaration -> statements ??? |*)
let topLevelDeclaration () =
  anyspace *> mkOpt (fix statements) >>= fun stm ->
    pos >>= fun pos ->
      (
        anyspace *> end_of_input <?> (" at offset " ^ string_of_int pos ^ ", expected")
      ) *> return stm

let parse file input =
  isParsingPattern := false;
  patternDepth := 0;
  match parse_only (topLevelDeclaration ()) (`String input) with
  | Ok ast ->
      let program = extractNode ast in
      let comments' = get_comments () in
      List (comments' @ [program])
  | Error e ->
      failwith (Printf.sprintf "%s: SyntaxError%s" file e)
