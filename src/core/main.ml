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

open Pipeline
open Cmdliner

let () =
  Gc.set { (Gc.get()) with Gc.minor_heap_size = 8_192_000; Gc.major_heap_increment = 100; Gc.space_overhead = 100; }

let rules =
  let doc = "Path to look for rules" in
  Arg.(value & opt_all file [] & info ["r";"rules"] ~docv:"<path>" ~doc)

let providers =
  let doc = "Path to look for providers" in
  Arg.(value & opt_all file [] & info ["p"; "provider"] ~docv:"<path>" ~doc)

let reporters =
  let doc = "Name or path of the reporter to use for messages" in Arg.(value & opt_all string [] & info ["reporter"] ~docv:"<path>" ~doc)

let print_ast =
  let doc = "Pretty print the AST for each source file to stdout" in
  let print_ast = (`Pretty, Arg.info ["print-ast"] ~doc) in
  let doc = "Print a JSON serialized AST for each source file to stdout" in
  let print_json_ast = (`Json, Arg.info ["print-json-ast"] ~doc) in
  Arg.(value & vflag `No [print_ast; print_json_ast])

let strict =
  let doc = "Treat warnings as errors" in
  Arg.(value & flag & info ["strict"] ~doc)

let strict_parse =
  let doc = "Treat parsing failures as errors" in Arg.(value & flag & info ["strict-parse"] ~doc)
let stdin =
  let doc = "The file extension that should be used to select a provider to parse the input from stdin" in
  Arg.(value & opt (some string) None & info ["stdin"] ~docv:"<extension>" ~doc)

let filelist =
  let doc = "string path to a file containing a list of files that should be analysed" in
  Arg.(value & opt (some non_dir_file) None & info ["filelist"] ~docv:"<file>" ~doc)

let files =
  let doc = "A list of paths to be analysed" in
  Arg.(value & pos_all file [] & info [] ~docv:"<paths>" ~doc)

let main rules providers reporters stdin filelist print_ast strict strict_parse files =
  if files = [] && filelist = None && stdin = None then
    `Help (`Pager, None)
  else

  let files' =
    match filelist with
    | None -> files
    | Some file -> Core.Std.In_channel.read_lines file
  in
  let rule_step =
    match rules with
    | [] -> Resolver.Config.load_step
    | _ -> Resolver.Rules.search_step
  in
  let process_step =
    match stdin with
    | None -> Driver.process
    | Some ext -> fun config ->
        let read () = Fs.read_file Pervasives.stdin in
        let ext' = Some ("." ^ ext) in
        Driver.check config "(stdin)" ext' read
  in
  let module P = Pipeline.Make(struct
    let configuration_steps = [
      Resolver.Providers.default_path_step;
      Resolver.Reporters.default_reporter_step;
      Resolver.Reporters.adjust_paths_step;
      Resolver.Reporters.load_step;
      Resolver.Files.expand_step;
      rule_step;
      Resolver.Providers.search_step;
      Resolver.Files.find_files_step;
    ]
    let process_step = process_step
    let filter_steps = [ Directive.filter ]
  end) in
  P.run { Pipeline.default_config with
    strict;
    strict_parse;
    print_ast;
    files = files';
    rules_paths = rules;
    provider_paths = providers;
    reporter_paths = reporters;
  }

let neal =
  let doc = "Not Exactly A Linter" in
  Term.(ret (const main $ rules $ providers $ reporters $ stdin $ filelist $ print_ast $ strict $ strict_parse $ files)),
  Term.info "neal" ~version:"0.2.4" ~doc

let test =
  let doc = "A list of rules to be tested" in
  let files = Arg.(non_empty & pos_all file [] & info [] ~doc) in
  Term.(ret (const Rule_tester.test $ files $ providers)),
  let doc = "Run your NEAL rules` tests" in
  Term.info "test" ~doc

let fail =
  Term.(ret @@ const @@ `Error (false, "")), Term.info "neal"

let () =
 match Term.(eval_choice ~err:Format.str_formatter neal [ test ]) with
 | `Error `Parse as e ->
     let error = Format.flush_str_formatter() in
     let re = Str.regexp_string "unknown command" in
     begin try
       ignore (Str.search_forward re error 0);
       Term.(exit @@ eval neal)
     with Not_found ->
       Format.pp_print_string Format.err_formatter error;
       Term.exit e
     end
 | `Error _ as e ->
     Format.pp_print_string Format.err_formatter (Format.flush_str_formatter());
     Term.exit e
 | e -> Term.exit e
