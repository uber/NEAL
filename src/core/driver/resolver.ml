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

let find = Fs.find ~onFailure:(fun file -> Stats.add_stat file Stats.Invalid_path)

let lib_ext =
  match Sys.backend_type with
  | Sys.Native -> ".cmxs"
  | Sys.Bytecode -> ".cmo"
  | _ -> failwith "Unsupported backend"

module Reporters = struct
  let builtin_reporters_path = Neal.Utils.relative_path "reporters"
  let cli_reporter = Filename.concat builtin_reporters_path "cli"

  let default_reporter_step config =
    let reporter_paths =
      match config.reporter_paths with
      | [] -> [ cli_reporter ]
      | l -> l
    in { config with reporter_paths }

  let adjust_paths_step config =
    let aux path =
      let path' = Filename.concat builtin_reporters_path path in
      if Sys.file_exists path then
        path
      else if Sys.file_exists path' then
        path'
      else failwith (Printf.sprintf "Could not find reporter: %s" path)
    in
    let reporter_paths = List.map aux config.reporter_paths in
    { config with reporter_paths }

  let load_step config =
    let filter f = Filename.check_suffix f (".reporter" ^ lib_ext) in
    find config.reporter_paths filter
      |> StringSet.elements
      |> List.map Neal.Reporter.load
      |> fun reporters -> { config with reporters }
end

module Rules = struct
  let find_rules paths =
    let filter f = Filename.check_suffix f ".rules" in
    find paths filter
      |> StringSet.elements
      |> List.map Rule_loader.load

  let augment_rule rule = (`Global, [rule])

  let search_step config =
    let rules =
      find_rules config.rules_paths
      |> List.map augment_rule
    in { config with rules = config.rules @ rules }
end

module Providers = struct
  let default_paths = [ Neal.Utils.relative_path "providers" ]

  let default_path_step config =
    { config with provider_paths = config.provider_paths @ default_paths }

  let search_step config =
    let filter f = Filename.check_suffix f (".provider" ^ lib_ext) in
    find config.provider_paths filter
      |> StringSet.elements
      |> List.map Neal.Provider.load
      |> fun providers -> { config with providers }
end

module Config = struct
  let find_scoped_rules (includes, ignores, rules) =
    let rules' = Rules.find_rules (StringSet.elements rules) in
    (`Scoped { includes; ignores }, rules')

  let make_provider_map (path, maps) =
    { path; maps }

  let load_step config =
    let aux (acc_rulemaps, acc_providermaps) file =
      let rule_maps, provider_maps = Config.find_configs file in
      (acc_rulemaps @ rule_maps, provider_maps @ acc_providermaps)
    in
    let rule_maps, provider_maps = List.fold_left aux ([], []) config.files in
    let rules = List.map find_scoped_rules rule_maps in
    let provider_maps = List.map make_provider_map provider_maps in
    { config with
      rules = config.rules @ rules;
      provider_maps = provider_maps @ config.provider_maps;
    }
end

module Files = struct
  let expand_step config =
    { config with files = List.map Fs.realpath config.files }

  let find_files_step config =
    { config with files = Fs.find_files config.files }
end
