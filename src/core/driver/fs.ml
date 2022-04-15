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

module StringSet = Set.Make(String)

let realpath = Unix.realpath

let join_path dir path =
  Filename.concat dir path |> realpath

let relative_to p1 p2 =
  let lp1 = String.length p1
  and lp2 = String.length p2
  in
  if lp1 < lp2 && String.equal p1 (String.sub p2 0 lp1)
  then String.sub p2 (lp1 + 1) (lp2 - lp1 - 1)
  else p2

let find ?(onFailure=fun _ -> ()) paths f =
  let fold_union = List.fold_left StringSet.union StringSet.empty in
  let rec find' p =
    let paths =
      if Sys.is_directory p then
        Sys.readdir p
        |> Array.to_list
        |> List.map (fun file ->
            try
              join_path p file |> find'
            with _ ->
              onFailure (Filename.concat p file);
              StringSet.empty
        )
        |> fold_union
      else StringSet.empty
    in
      if f p
      then StringSet.add p paths
      else paths
  in
  paths
    |> List.map find'
    |> fold_union

let extname file =
  try
    let index = String.rindex file '.' in
    Some (String.sub file index (String.length file - index))
  with Not_found ->
    None

let read_file input =
  let buf =  Buffer.create 257 in
  let rec aux () =
    try
      Buffer.add_string buf (input_line input);
      Buffer.add_char buf '\n';
      aux ()
    with End_of_file -> ()
  in
  aux ();
  Buffer.contents buf

let with_file filename fn =
  let input = open_in filename in
  let res = fn input in
  close_in input;
  res

let read filename = with_file filename read_file

let find_dirs paths =
  List.fold_left (fun paths f ->
    if Sys.file_exists f && not (Sys.is_directory f)
    then StringSet.add f paths
    else
      let fs = find [f] (fun p -> Sys.is_directory p) in
      StringSet.union fs paths) StringSet.empty paths

let find_files paths =
  find paths (fun p -> not (Sys.is_directory p))
  |> StringSet.elements
