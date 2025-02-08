type box = {length : int; width : int; height : int}


let parse_input (lines : string list) : box list =
  let parse_line line =
    Scanf.sscanf line "%dx%dx%d" (fun length width height -> {length; width; height}) in
  List.map parse_line lines


let calc_wrapping_paper ({length; width; height} : box) : int =
  let a = length * width in
  let b = length * height in
  let c = width * height in
  let smallest = min a (min b c) in
  2 * (a + b + c) + smallest


let calc_total_wrapping_paper (boxes : box list) : int =
  List.fold_left (fun acc box -> acc + calc_wrapping_paper box) 0 boxes


let () =
  let lines = In_channel.input_lines stdin in
  let boxes = parse_input lines in
  let result = calc_total_wrapping_paper boxes in
  print_int result; print_newline ();
