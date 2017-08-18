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

type stat =
  | Parsing_ok
  | Parsing_fail
  | No_available_provider
  | No_rules_found
  | Invalid_path
  | Reported_error
  | Reported_warning
  | Reported_other

type stats = {
  parsing_ok : string list ref;
  parsing_fail : string list ref;
  no_available_provider : string list ref;
  no_rules_found : string list ref;
  invalid_path : string list ref;
  reported_error : string list ref;
  reported_warning : string list ref;
  reported_other : string list ref;
}

let stats = {
  parsing_ok = ref [];
  parsing_fail = ref [];
  no_available_provider = ref [];
  no_rules_found = ref [];
  invalid_path = ref [];
  reported_error = ref [];
  reported_warning = ref [];
  reported_other = ref [];
}

let map = function
  | Parsing_ok -> stats.parsing_ok
  | Parsing_fail -> stats.parsing_fail
  | No_available_provider -> stats.no_available_provider
  | No_rules_found -> stats.no_rules_found
  | Invalid_path -> stats.invalid_path
  | Reported_error -> stats.reported_error
  | Reported_warning -> stats.reported_warning
  | Reported_other -> stats.reported_other

let str = function
  | Parsing_ok -> "Parsing_ok"
  | Parsing_fail -> "Parsing_fail"
  | No_available_provider -> "No_available_provider"
  | No_rules_found -> "No_rules_found"
  | Invalid_path -> "Invalid_path"
  | Reported_error -> "Reported_error"
  | Reported_warning -> "Reported_warning"
  | Reported_other -> "Reported_other"

let add_stat file stat =
  let field = map stat in
  field := file :: !field

open Format
let finish strict strict_parse =
  let print stat =
    Utils.stats_logger ~sep:(str stat ^ ": ") (string_of_int (List.length !(map stat)))
  in
  Utils.parsing_failure_logger "Parsing failed for:";
  List.iter Utils.parsing_failure_logger !(map Parsing_fail);
  print Parsing_ok;
  print Parsing_fail;
  print No_available_provider;
  print No_rules_found;
  print Invalid_path;
  print Reported_error;
  print Reported_warning;
  print Reported_other;
  if
    List.length !(map Reported_error) > 0 ||
    (strict && List.length !(map Reported_warning) > 0) ||
    (strict_parse && List.length !(map Parsing_fail) > 0)
  then
    exit 1
  else
    exit 0
