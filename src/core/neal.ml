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

module Absyn = struct
  type line = int
  type column = int
  type loc = Off of int | Pos of line * column
  type absyn =
    | Node of string * loc * (string * absyn) list
    | String of string
    | Bool of bool
    | Int of int
    | List of absyn list
    | Null
end

module Rule = struct
  exception SyntaxError of string

  type import = {
    rules : [ `All | `Rules of string list ];
    file : string;
  }

  type scope =
    | Child of string
    | Descendant of string
    | Unscoped

  type matcher = { scope: scope; node: string; predicate: exp option; body: stmt list; _uid: int }

  and stmt =
    | Action of call
    | Matcher of matcher
    | Var of assignment
    | Assignment of assignment
    | Condition of condition

  and exp =
    | Binop of exp * string * exp
    | Call of call
    | Value of value

  and call = string * exp list

  and assignment = var * bool
  and var = string * int

  and condition = exp * stmt list

  and value =
    | Literal of literal
    | Exp of exp

  and literal = [
    | `Int of int
    | `String of string
    | `Id of string
    | `ConditionalVar of var
    | `Bool of bool
    | `PropAccess of string * string list
    | `RegExp of Str.regexp
    | `Node of Absyn.absyn
    | `List of literal list
  ]

  type loc = int

  type input = [
    | `String of string
    | `File of string
  ]
  type expectation = [
    | `Ok
    | `Warn
    | `Fail
  ]
  type test = string * expectation * input * string option
  type filename = string
  type rule = Rule of string * filename * matcher
  type ruleset = Ruleset of (string * rule list * test list) list
  type file = import list * ruleset

  module Set = Set.Make(struct
    type t = rule
    let compare (Rule (_, _, {_uid})) (Rule (_, _, {_uid=_uid'})) =
      _uid - _uid'
end)
end

module rec Ctx : sig
  type ctx = {
    provider: (module Provider.PROVIDER);
    reporters: (module Reporter.REPORTER) list;
    kind: string;
    prop: string;
    props: (string * Absyn.absyn) list;
    depth: int;
    file: string;
    source: string;
    loc: Absyn.loc option;
    rule: Rule.rule;
    vars: (Rule.var * bool ref) list
  }
end = Ctx


and Provider : sig
  type 'a exports = (string * (Ctx.ctx -> Rule.literal list -> 'a)) list
  type exported_macros = Rule.literal exports
  type exported_actions = unit exports

  (** The interface of an AST Provider *)
  module type PROVIDER = sig

    (** The name of the provider which will be used to refer to it from a rule, e.g.
     *
     * ProviderName::AstNodeName { ... }
     *)
    val name: string

    (** The file extensions this provider can handle *)
    val extensions: string list

    (** The entry point function to parse source files
     * @param filename The name of the file to be parsed
     * @param source The contents of the file
     * @return The AST representation of the `source`
     *)
    val parse: string -> string -> Absyn.absyn

    (**
     * An associative list of macros exported to be called from rules, e.g.
     *
     *   AstNodeName where exported_macro(arg1, arg2) { ... }
     *
     * @param context The context in which the macro was called
     * @param arguments The arguments passed to the macro
     * @return A literal value, usually a boolean
     *)
    val exported_macros : exported_macros

    (**
     * An associative list functions that can be exported to be called from rules, e.g.
     *
     *   AstNodeName { exported_action(args...) }
     *
     * @param context The context in which the action was called
     * @param arguments The arguments passed to the action
     *)
    val exported_actions : exported_actions

  end

  (** The Loader interface for providers. This imports 2 functions: `register` to
   * be called by providers that want to be registered with the driver and `load`
   * to be called by the driver to load registered providers.
   * @see 'neal/driver/loader.ml' for the Loader interface
   *)
  include Loader.LOADER with type t = (module PROVIDER)
end = struct
  include Provider
  include Loader.Make(struct
    type t = (module PROVIDER)
  end)
end

and Reporter : sig
  type severity =
    | Error
    | Warning
    | Other

  type fix =
    | Suggestion of string

  type report = severity -> Ctx.ctx -> fix -> unit

  type violation = severity * Ctx.ctx * fix
  val string_of_severity : severity -> string
  val report : report
  val get_violations : unit -> violation list

  module type REPORTER = sig
    val report : report
    val name : string
  end

  include Loader.LOADER with type t = (module REPORTER)
end = struct
  include Reporter

  let string_of_severity = function
    | Error -> "error"
    | Warning -> "warning"
    | Other -> "info"


  let violations = ref []
  let report severity ctx fix =
    violations := (severity, ctx, fix) :: !violations

  let get_violations () =
    let v = !violations in
    violations := [];
    List.rev v

  include Loader.Make(struct
    type t = (module REPORTER)
  end)
end

module Utils = struct
  let neal_root =
  IFDEF RELATIVE_LIB_PATH THEN
    let exec_path = Core.Filename.realpath Sys.executable_name in
    let _build = Filename.dirname exec_path in
    Filename.concat _build RELATIVE_LIB_PATH
  ELSE
    IFDEF LIB_PATH THEN
      LIB_PATH
    ELSE
      let exec_path = Core.Filename.realpath Sys.executable_name in
      let _build = Filename.dirname exec_path in
      let neal = Filename.dirname _build in
      Filename.dirname neal
    END
  END

  let relative_path path =
    Filename.concat neal_root path
end
