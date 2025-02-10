module StringMap = Map.Make(String)

type string_or_int = S of string | I of int
type gate =
  | Assignment of {value : int}
  | Transfer of {wire : string}
  | And of {left : string_or_int; right_wire : string}
  | Or of {left_wire : string; right_wire : string}
  | LeftShift of {wire : string; n : int}
  | RightShift of {wire : string; n : int}
  | Not of {wire : string}
type operation = {gate : gate; target_wire : string}


let parse_input (lines : string list) : operation list =
  let parse line = match Str.(line |> split (regexp " -> ")) with
    | [instruction; target_wire] ->
        (match String.split_on_char ' ' instruction with
          | [s] -> (match int_of_string_opt s with
              | None -> {gate = Transfer {wire = s}; target_wire}
              | Some value -> {gate = Assignment {value}; target_wire})
          | [left; "AND"; right_wire] ->
              if left = "1" then {gate = And {left = I 1; right_wire}; target_wire}
              else {gate = And {left = S left; right_wire}; target_wire}
          | [left_wire; "OR"; right_wire] -> {gate = Or {left_wire; right_wire}; target_wire}
          | [wire; "LSHIFT"; s] -> {gate = LeftShift {wire; n = int_of_string s}; target_wire}
          | [wire; "RSHIFT"; s] -> {gate = RightShift {wire; n = int_of_string s}; target_wire}
          | ["NOT"; wire] -> {gate = Not {wire}; target_wire}
          | _ -> failwith "Unknown instruction type.")
    | _ -> failwith "Malformed input line." in
  List.map parse lines


let perform_operation (wires : int StringMap.t) ({gate; target_wire} : operation) : int StringMap.t option =
  let target_value = match gate with
    | Assignment {value} -> Some value
    | Transfer {wire} -> StringMap.find_opt wire wires
    | And {left = S left_wire; right_wire} ->
        (match (StringMap.find_opt left_wire wires, StringMap.find_opt right_wire wires) with
          | (Some left, Some right) -> Some (left land right)
          | _ -> None)
    | And {left = I k; right_wire} -> Option.map (fun value -> k land value) (StringMap.find_opt right_wire wires)
    | Or {left_wire; right_wire} ->
        (match (StringMap.find_opt left_wire wires, StringMap.find_opt right_wire wires) with
          | (Some left, Some right) -> Some (left lor right)
          | _ -> None)
    | LeftShift {wire; n} -> Option.map (fun value -> (value lsl n) mod 65536) (StringMap.find_opt wire wires)
    | RightShift {wire; n} -> Option.map (fun value -> value lsr n) (StringMap.find_opt wire wires)
    | Not {wire} -> Option.map (fun value -> (lnot value) + 65536) (StringMap.find_opt wire wires)
  in Option.map (fun value -> StringMap.add target_wire value wires) target_value


let connect_wires (operations : operation list) : int =
  let queue = Queue.of_seq (List.to_seq operations) in
  let rec loop state = match StringMap.find_opt "a" state with
    | Some value -> value
    | None ->
        let op = Queue.take queue in
        match perform_operation state op with
          | None ->
              Queue.add op queue;
              loop state
          | Some state' -> loop state' in
  loop StringMap.empty


let connect_and_reset_wires (operations : operation list) : int =
  let result = connect_wires operations in
  let operations' = List.filter (fun {target_wire; _} -> target_wire <> "b") operations in
  connect_wires ({gate = Assignment {value = result}; target_wire = "b"} :: operations')


let () =
  let lines = In_channel.input_lines stdin in
  let operations = parse_input lines in
  let result = connect_and_reset_wires operations in
  print_int result; print_newline ();
