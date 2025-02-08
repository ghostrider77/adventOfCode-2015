type box = {length : int; width : int; height : int}


let parse_input (lines : string list) : box list =
  let parse_line line =
    Scanf.sscanf line "%dx%dx%d" (fun length width height -> {length; width; height}) in
  List.map parse_line lines


let calc_ribbon_length ({length; width; height} : box) : int =
  match List.sort compare [length; width; height] with
    | a :: b :: _ -> 2 * (a + b) + length * width * height
    | _ -> failwith "Malformed box."


let calc_total_ribbon_length (boxes : box list) : int =
  List.fold_left (fun acc box -> acc + calc_ribbon_length box) 0 boxes


let () =
  let lines = In_channel.input_lines stdin in
  let boxes = parse_input lines in
  let result = calc_total_ribbon_length boxes in
  print_int result; print_newline ();
