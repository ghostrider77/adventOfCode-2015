module StringMap = Map.Make(String)

type aunt = {id : int; compounds : int StringMap.t}

let detected = StringMap.of_list [
  ("children", 3);
  ("cats", 7);
  ("samoyeds", 2);
  ("pomeranians", 3);
  ("akitas", 0);
  ("vizslas", 0);
  ("goldfish", 5);
  ("trees", 3);
  ("cars", 2);
  ("perfumes", 1)
]


let does_match {compounds; _} : bool =
  StringMap.for_all (fun name k -> StringMap.find name detected = k) compounds


let parse_input (lines : string list) : aunt list =
  let scan_compound s = Scanf.sscanf s "%s@: %d" (fun name k -> (name, k)) in
  let parse line =
    let (id, content) = Scanf.sscanf line "%s %d: %[^\n]" (fun _ id rest -> id, rest) in
    let compounds = content |> Str.split (Str.regexp ", ") |> List.map scan_compound |> StringMap.of_list in
    {id; compounds} in
  List.map parse lines


let find_matching_id (aunts : aunt list) : int =
  match List.filter_map (fun ({id; _} as aunt) -> if does_match aunt then Some id else None) aunts with
    | [id] -> id
    | _ -> failwith "No unique solution found."


let () =
  let lines = In_channel.input_lines stdin in
  let aunts = parse_input lines in
  let result = find_matching_id aunts in
  print_int result; print_newline ();
