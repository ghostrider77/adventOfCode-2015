let calc_floor (instructions : string) : int =
  String.fold_left (fun acc c -> if c = '(' then acc + 1 else acc - 1) 0 instructions


let () =
  let line = read_line () in
  let result = calc_floor line in
  print_int result; print_newline ();
