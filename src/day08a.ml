let unescape (line : string) : string =
  let n = String.length line in
  Scanf.unescaped @@ String.sub line 1 (n - 2)


let () =
  let lines = In_channel.input_lines stdin in
  let n1 = List.fold_left (fun acc line -> acc + String.length line) 0 lines in
  let n2 = List.fold_left (fun acc line -> acc + (String.length @@ unescape line)) 0 lines in
  let result = n1 - n2 in
  print_int result; print_newline ();
