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

type rule_name = string
type test_name = string
type error = [
  | `Parsing_failure of Neal.Rule.input
  | `No_such_file of string
]

exception Error of rule_name * test_name * error

let successes = ref 0
let failures = ref 0
let errors = ref 0

let pass rule_name test_name =
  incr successes;
  Printf.fprintf stdout "✓ Test `%s' of rule `%s' passed\n"
    test_name
    rule_name

let fail rule_name test_name =
  incr failures;
  Printf.fprintf stdout "ⅹ Test `%s' of rule `%s' failed:\n\t"
    test_name
    rule_name;
  function
    | `Wrong_count (expected, actual) ->
        Printf.fprintf stdout "Expected %d violations, but found %d\n"
          expected actual
    | `Wrong_message (expected, actual) ->
        Printf.fprintf stdout "Expected violation with message `%s', but found `%s'\n"
          expected actual
    | `Wrong_type (expected, actual) ->
        Printf.fprintf stdout "Expected to find `%s', but found `%s'\n"
         (Neal.Reporter.string_of_severity expected) (Neal.Reporter.string_of_severity actual)


let cwd = Sys.getcwd ()
let error_ rule_name test_name error =
  incr errors;
  Printf.fprintf stdout "! Failed to parse input of test `%s' from rule `%s':\n\t"
    test_name rule_name;
  match error with
  | `No_such_file path ->
      Printf.fprintf stdout "No such file: `%s'\n" path
  | `Parsing_failure (`String s) ->
      Printf.fprintf stdout "input string `%s'\n" s
  | `Parsing_failure (`File f) ->
      let f' = Fs.relative_to cwd f in
      Printf.fprintf stdout "input file `%s'\n" f'

let run rule_name provider rule (test_name, result, input, message) =
  let Neal.Rule.Rule(_, path, _) = rule in
  let input, file, source = match input with
  | `String s -> `String s, "(string)", s
  | `File f ->
      try
        let f' = Fs.join_path (Filename.dirname path) f in
        `File f', f', Fs.read f'
      with Unix.Unix_error _ ->
        raise (Error (rule_name, test_name, `No_such_file f))
  in
  let absyn =
      try Driver.parse provider file source
      with Driver.Done (_, Stats.Parsing_fail) ->
        raise (Error (rule_name, test_name, (`Parsing_failure input)))
  in
  let rules = Neal.Rule.Set.singleton rule in
  Driver.visitor provider [] file source rules absyn

let report_result rule_name (test_name, result, _, message) =
  let violations = Neal.Reporter.get_violations () in
  let aux t type_ =
      if List.length violations <> 1 then
        fail rule_name test_name (`Wrong_count (1, List.length violations))
      else begin match violations with
      | [(type_', _, Neal.Reporter.Suggestion fix)] when type_ = type_'->
          begin match message with
          | None -> pass rule_name test_name
          | Some m when m = fix -> pass rule_name test_name
          | Some m -> fail rule_name test_name (`Wrong_message (m, fix))
          end
      | (type_', _, _) :: _ ->
          fail rule_name test_name (`Wrong_type (type_, type_'))
      | [] -> assert false
      end
  in
  match result with
  | `Ok ->
      if violations <> [] then
        fail rule_name test_name (`Wrong_count (0, List.length violations))
      else
        pass rule_name test_name
  | `Warn -> aux `Warn Neal.Reporter.Warning
  | `Fail -> aux `Fail Neal.Reporter.Error

let select_provider provider providers =
  let aux (module P : Neal.Provider.PROVIDER) =
    P.name = provider
  in
  try
    List.find aux providers
  with Not_found ->
    Printf.fprintf stdout "Unknown provider: `%s'\n" provider;
    exit (-1)

let test_rules rule_name providers rules test =
    let aux (Neal.Rule.Rule (provider, file, matcher) as rule) =
      let provider = select_provider provider providers in
      run rule_name provider rule test
    in
    List.iter aux @@ List.rev rules

let run_test providers rule_name rules test =
  try
    test_rules rule_name providers rules test;
    report_result rule_name test
  with Error (rule_name, test_name, error) ->
    error_ rule_name test_name error

let test_triple providers (rule_name, rules, tests) =
  List.iter (run_test providers rule_name rules) tests

let test_ruleset providers (Neal.Rule.Ruleset rule_triples) =
  List.iter (test_triple providers) rule_triples

let test_rulesets providers = function
| (_, rulesets) ->
    List.iter (test_ruleset providers) rulesets

let test_all_rules config =
  List.iter (test_rulesets config.Pipeline.providers) config.Pipeline.rules;
  let s = function
    | 1 -> ""
    | _ -> "s"
  in
  Printf.fprintf stdout "\n✓ %d test%s passed, ⅹ %d test%s failed, ! %d test%s errors\n"
    !successes (s !successes) !failures (s !failures) !errors (s !errors);
  if !failures + !errors > 0 then
    exit 1
  else exit 0

let test rules providers =
  Rule_loader.set_parser Rule_parser.prog_non_flip;
  Rule_loader.disable_imports();

  let module P = Pipeline.Make(struct
    let configuration_steps = [
      Resolver.Providers.default_path_step;
      Resolver.Providers.search_step;
      Resolver.Rules.search_step;
    ]
    let process_step = test_all_rules
    let filter_steps = []
  end) in
  P.run { Pipeline.default_config with
    Pipeline.rules_paths = rules;
    Pipeline.provider_paths = providers;
  }
