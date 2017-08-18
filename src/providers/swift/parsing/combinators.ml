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
open Neal.Absyn

(* HELPERS *)

exception Unreachable of int
let unreachable pos = raise (Unreachable pos)

type holder =
  | NodeHolder of (int * absyn)
  | PropHolder of int * (string * absyn) list

type interpolated =
  | InterpolatedExp of holder
  | InterpolatedCodePoints of string list

let string_of_chars chars =
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  String (Buffer.contents buf)

let invalidNode = NodeHolder (-1, Null)

let mkNode ?pos:(p:int option = None) name =
  let p' =
    match p with
    | Some p -> return p
    | None -> pos
  in p' >>| fun pos ->
    NodeHolder (pos, Node (name, Off pos, []))

let mkPropHolder =
  pos >>| fun pos -> PropHolder (pos, [])

let extractNode = function
  | NodeHolder (_,n) -> n
  | PropHolder (pos,_) -> unreachable pos

let mkProp name p =
  pos >>= fun pos ->
    p >>| fun x ->
      PropHolder (pos, [(name, extractNode x)])

let mkPropE name p = mkProp name (p ())

let mkBoolProp name p =
  p *> pos >>| fun pos -> PropHolder (pos, [(name, Bool true)])

let mkBoolPropE name p = mkBoolProp name (p ())

let mkOpt p =
  pos >>= fun pos -> option (NodeHolder (pos, Null)) p

let mkOptE p = mkOpt (p ())

let mkOptProp name p =
  pos >>= fun pos -> option (PropHolder (pos, [(name, Null)])) (mkProp name p)

let mkOptPropE name p = mkOptProp name (p ())

let mkOptPropEmpty p =
  pos >>= fun pos -> option (PropHolder (pos, [])) p

let mkOptPropEmptyE p = mkOptPropEmpty (p ())

let toList x =
  pos >>| fun pos -> NodeHolder (pos, List (List.map extractNode x))

let mkList1 p =
  many1 (p ()) >>= toList

let mkString str =
  pos >>| fun pos -> NodeHolder (pos, String str)

let (<!>) lhs rhs =
  lhs <|> (rhs ())

let many1 p =
  Angstrom.many1 (p ())

let (<:>) node p =
  node >>= function
    | NodeHolder (_, Node (name,pos,props)) -> begin
        p >>| function
          | PropHolder (pos, props') -> NodeHolder (pos, Node (name,Off pos,props@props'))
          | NodeHolder (pos, _) -> unreachable pos
    end
    | PropHolder (_, props) -> begin
        p >>| function
          | PropHolder (pos, props') ->
              PropHolder (pos, props@props')
          | NodeHolder (pos, _) ->
              unreachable pos
    end
    | NodeHolder (pos, n) ->
        unreachable pos

let comments = Hashtbl.create 16
let add_comment comment =
  mkNode "Comment"
  <:> mkProp "Value" (mkString comment)
  >>| fun comment' ->
    match comment' with
    | NodeHolder (pos, _) ->
        if Hashtbl.mem comments pos
        then ()
        else Hashtbl.add comments pos comment'
    | _ -> assert false

let get_comments () =
  let to_list key value lst =
    (key, value) :: lst
  in
  let cs =
    Hashtbl.fold to_list comments []
    |> List.sort (fun (a,_) (b,_) -> compare a b)
    |> List.map (fun (_, c) -> extractNode c)
  in Hashtbl.reset comments; cs

let multiline_comment ~newline =
  let rec body buf nl =
    (Angstrom.string "*/" >>= fun _ ->
      if newline && not nl then
        fail "Multiline comment doesn't include newline"
      else begin
        add_comment ("/*" ^ Buffer.contents buf ^ "*/")
      end
    ) <|> (
      any_char >>= fun c ->
      Buffer.add_char buf c;
      match c with
      | '\n' -> body buf true
      | _ -> body buf nl
    )
  in
  Angstrom.string "/*"
  *> body (Buffer.create 256) false

let single_line_comment =
  let rec aux () =
    Angstrom.string "//"
    *> take_while(function
      | '\n' -> false
      | _ -> true)
    >>= fun comment ->
      option "" (char '\n' *> aux ()) >>= fun comment' ->
          return ("//" ^ comment ^ "\n" ^ comment')
  in aux () >>= add_comment

let comment ~newline = (
    single_line_comment
  ) <|> (
    multiline_comment ~newline *> return ()
  )

let whitespace' =
  satisfy (function
    | ' '
    | '\t'
    -> true
    | _ -> false
  ) *> return ()

let whitespace =
  skip_many (whitespace' <|> comment ~newline:false)

let linebreak =
  skip_many (whitespace' <|> multiline_comment ~newline: false)
  *> (
    comment ~newline:true
    <|> (satisfy (function
      | '\r'
      | '\n' -> true
      | _ -> false
    ) *> return ())
  )
  *> whitespace

let anyspace =
  skip_many (linebreak <|> comment ~newline:false <|> whitespace')

let operatorWhitespace lhs allow =
  let done_ has =
    if allow <> has then
      fail "operator whitespace"
    else
      return ()
  in
  peek_char >>= function
    | None ->
        done_ true
    | Some c ->
        match c with
        | ' '
        | '\t'
        | '\r'
        | '\n'
        | ','
        | ';'
        | ':' ->
            done_ true
        | _ ->
            match lhs,c with
            | true,'('
            | true,'['
            | true,'{'
            | false,')'
            | false,']'
            | false,'}'
            | false,'.' ->
                done_ true
            | _ ->
                done_ false

let lhsOperatorWhitespace = operatorWhitespace true
let rhsOperatorWhitespace = operatorWhitespace false

let wstring' s =
  pos >>= fun pos ->
  anyspace *> string s

let wfstring s =
  wstring' s <* anyspace

let wstring s =
  wstring' s <* (
      (*TODO: this is not the correct heuristic to find the longest keyword*)
      peek_char >>= function
        | None -> return ()
        | Some c -> match c with
          | 'a'..'z'
          | 'A'..'Z'
          | '0'..'9'
          | '_' -> fail "Not the longest match"
          | _ -> return ()
    )

let wchar c = pos >>= fun pos ->
  (match c with
  | '{'
  | '}'
  | ']'
  | ')'
  | '.'
  | ','
  | ':'
  | '?'
  | '=' -> anyspace
  | _ -> whitespace
  )
  *> char c
  <* (match c with
  | '{'
  | '['
  | '('
  | '.'
  | ','
  | '?'
  | ':'
  | '=' -> anyspace
  | _ -> return ()
  )

let sepBy1 chr p =
  sep_by1 (wchar chr) (p ())

let commaSep p =
  sepBy1 ',' p >>= toList

let optSemi =
  (anyspace *> char ';' *> anyspace)
  <|> (skip_many1 linebreak *> anyspace)
  <|> (whitespace *> peek_char >>= function
    | None -> return ()
    | Some c -> match c with
      | '}' -> return ()
      | _ ->
          pos >>= fun p ->
          fail ("missing linebreak or semicolon")
  )

(* Pattern helpers *)

let isParsingPattern = ref false
let patternDepth = ref 0
let togglePattern b = return () >>| fun _ ->
  if b
  then incr patternDepth
  else decr patternDepth;

  if b then isParsingPattern := b
  else if !patternDepth = 0 then isParsingPattern := false

let startPattern = togglePattern true
let stopPattern = togglePattern false

let ifParsingPattern = return () >>= fun _ ->
  if !isParsingPattern
  then return ()
  else fail "Pattern not allowed here"
