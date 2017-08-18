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

exception Found
let inherits_from ctx args =
  let aux () =
    match args with
    | [`String name] -> begin
        match List.assoc "TypeInheritanceClause" ctx.Ctx.props with
        | Absyn.List(typeInheritanceClause) -> begin
            List.iter (fun node ->
              match node with
              | Absyn.Node("TypeIdentifier",_,props) -> begin
                  match List.assoc "TypeName" props with
                  | Absyn.Node("Identifier",_,props) -> begin
                      match List.assoc "Value" props with
                      | Absyn.String str when str = name ->
                          raise Found
                      | _ -> ()
                      end
                  | _ -> ()
                  end
              | _ -> ()
            ) typeInheritanceClause
        end
        | _ -> ()
    end
    | _ -> ()
  in
  try
    aux ();
    `Bool false
  with Found -> `Bool true

let () = Provider.register(module struct
  let name = "Swift"
  let extensions = [".swift"]
  let parse = Parser.parse
  let exported_macros = [
    ("inheritsFrom", inherits_from)
  ]
  let exported_actions = []
end)
