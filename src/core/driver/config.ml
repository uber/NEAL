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

open Fs
open Pipeline

module Y = Yojson.Safe.Util

exception Invalid_configuration

type provider_map =
  Str.regexp

let config_file path =
  Filename.concat path "neal.json"

let fix_path path files =
  let dirname = Filename.dirname path in
  List.fold_left (fun files file ->
    try
      StringSet.add (join_path dirname file) files
    with Unix.Unix_error _ ->
      Printf.printf "WARNING: Invalid path at configuration file %s:\nNo such file or directory: %s\n\n" path file;
      Stats.add_stat (Filename.concat dirname file) Stats.Invalid_path;
      files
  ) StringSet.empty files

let default_regex path = Str.regexp (Filename.dirname path ^ "/.*")

let parse_rules path json =
  try
    Y.member "rules" json
    |> Y.convert_each Y.to_string
    |> fix_path path
  with Not_found -> StringSet.empty

let regex_list default member json =
  json
  |> Y.member member
  |> Y.to_option Y.to_list
  |> function
    | None -> default
    | Some l ->
        l |> Y.filter_string |> List.map Str.regexp

let parse_rule_map path json =
  let globs = regex_list [default_regex path] "glob" json in
  let exclude = regex_list [] "exclude" json in
  let rules = parse_rules path json in
  (globs, exclude, rules)

let load_rule_map path json =
  let json = Y.member "rule-map" json in
  Y.convert_each (parse_rule_map path) json

let load_provider_map path json =
  let aux (provider, regexes) =
    Y.convert_each Y.to_string regexes
    |> List.map (fun regex -> (Str.regexp regex, provider))
  in
  try
    let json = Y.to_assoc (Y.member "provider-map" json) in
    List.map aux json |> List.concat
  with Y.Type_error _ ->
    []

let parse_dict path json =
  let exclude = regex_list [] "exclude" json in
  let rule_maps =
    try load_rule_map path json
    with Y.Type_error _ ->
      let rules = parse_rules path json in
      [([default_regex path], exclude, rules)]
  in
  let provider_map = load_provider_map path json in
  (rule_maps, provider_map)

let visited = Hashtbl.create 16
let parse_config path =
  if Hashtbl.mem visited path then ([], []) else
  let () = Hashtbl.add visited path () in
  try
    Utils.debug ("Using config file at " ^ path);
    let rec read json =
      match json with
      | `Assoc l -> parse_dict path (`Assoc l)
      | `List l ->
          let aux (acc_rules, acc_providers) json =
            let rule_maps, provider_maps = read json in
            (rule_maps @ acc_rules, provider_maps @ acc_providers)
          in List.fold_left aux ([], []) l
      | _ -> raise Invalid_configuration
    in read (Yojson.Safe.from_file path)
  with Invalid_configuration | Not_found ->
    Printf.fprintf stderr "Invalid configuration found at path %s\n" path;
    exit 1

(*Aggregate provider paths and rule paths*)
let find_configs path =
  let try_path (acc_rulemaps, acc_providermaps) path =
    let p = config_file path in
    if Sys.file_exists p then
      let rule_maps, provider_maps = parse_config p in
      (acc_rulemaps @ rule_maps, (path, provider_maps) :: acc_providermaps)
    else
      (acc_rulemaps, acc_providermaps)
  in
  let rec aux paths = function
    | "/" -> paths
    | path -> aux (try_path paths path) (Filename.dirname path)
  in
  let config' = aux ([], []) (Filename.dirname path) in
  let dirs = find_dirs [path] in
  List.fold_left try_path config' (StringSet.elements dirs)
