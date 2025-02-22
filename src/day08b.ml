let () =
  let lines = In_channel.input_lines stdin in
  let n1 = List.fold_left (fun acc line -> acc + String.length line) 0 lines in
  let n2 = List.fold_left (fun acc line -> acc + 2 + String.(line |> escaped |> length)) 0 lines in
  let result = n2 - n1 in
  print_int result; print_newline ();
