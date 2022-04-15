open Neal

exception Type_error

let to_string = function
  | `String s -> s
  | _ -> raise Type_error

let to_int = function
  | `Int i -> i
  | _ -> raise Type_error

let assoc key = function
  | `Assoc props -> List.assoc key props
  | _ -> raise Type_error

let get_kind props =
  List.assoc "type" props
  |> to_string
  |> String.capitalize_ascii

let get_loc (props : (string * Yojson.Safe.t) list) =
  try
    List.assoc "loc" props
    |> assoc "start"
    |> fun x ->
        Absyn.Pos (assoc "line" x |> to_int, assoc "column" x |> to_int)
  with Not_found ->
    Absyn.Pos (0, 0)

let rec get_props props =
  List.fold_left (fun ps (k,v) ->
    match k with
    | "start" | "end" | "loc" | "tokens" -> ps
    | k -> (String.capitalize_ascii k, absyn_of_json v) :: ps
  ) [] props

and absyn_of_json x =
  try
    match x with
  | `Assoc props -> Absyn.Node (get_kind props, get_loc props, get_props props)
  | `Int i -> Absyn.Int i
  | `String s -> Absyn.String s
  | `Bool b -> Absyn.Bool b
  | `Null -> Absyn.Null | `List l -> Absyn.List (List.map absyn_of_json l)
  | _ -> raise Type_error
  with Not_found ->
    print_string "WTF";
    print_string (Yojson.Safe.show x);
    raise Type_error

let exec_name = Neal.Utils.relative_path "providers/helpers/dump_javascript_ast.js"

let parse _ source =
  let stdout, stdin = Unix.open_process exec_name in
  output_string stdin source;
  close_out stdin;
  let json = Yojson.Safe.from_channel stdout in
  close_in stdout;
  let _ = Unix.close_process (stdout, stdin) in
  absyn_of_json json

let () = Provider.register(module struct
  let name = "JavaScript"
  let extensions = [".js"]
  let parse = parse
  let exported_macros = []
  let exported_actions = []
end)
