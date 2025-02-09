type coord = {x : int; y : int}
type action = TurnOn | TurnOff | Toggle
type instruction = {switch : action; topLeft : coord; bottomRight : coord}

let action_of_string = function
  | "on" -> TurnOn
  | "off" -> TurnOff
  | _ -> failwith "Unknown action."


let parse_input (lines : string list) : instruction list =
  let parse line =
    if String.starts_with ~prefix:"toggle" line then
      let (a, b, c, d) = Scanf.sscanf line "toggle %d,%d through %d,%d" (fun a b c d -> (a, b, c, d)) in
      {switch = Toggle; topLeft = {x = a; y = b}; bottomRight = {x = c; y = d}}
    else
      let (switch, a, b, c, d) = Scanf.sscanf line "turn %s %d,%d through %d,%d" (fun s a b c d -> (s, a, b, c, d)) in
      {switch = action_of_string switch; topLeft = {x = a; y = b}; bottomRight = {x = c; y = d}} in
  List.map parse lines


let adjust_lights (grid : int array array) ({switch; topLeft; bottomRight} : instruction) : unit =
  let light_adjuster = match switch with
    | Toggle -> (+) 2
    | TurnOn -> (+) 1
    | TurnOff -> fun x -> max 0 (x - 1) in
  for i = topLeft.x to bottomRight.x do
    for j = topLeft.y to bottomRight.y do
      grid.(i).(j) <- light_adjuster grid.(i).(j);
    done;
  done


let calc_nr_lits_on (instructions : instruction list) : int =
  let grid = Array.make_matrix 1000 1000 0 in
  List.iter (adjust_lights grid) instructions;
  Array.fold_left (fun acc row -> Array.fold_left (+) acc row) 0 grid


let () =
  let lines = In_channel.input_lines stdin in
  let instructions = parse_input lines in
  let result = calc_nr_lits_on instructions in
  print_int result; print_newline ();
