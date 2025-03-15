let generate_next (n : int) : int =
  (n * 252533) mod 33554393


let identifiy_code (row : int) (column : int) : int =
  let diagonal = row + column - 1 in
  column + (diagonal - 1) * diagonal / 2


let read_input (line : string) : int * int =
  Scanf.sscanf line "%s@.  Enter the code at row %d, column %d." (fun _ r c -> r, c)


let calc_code (row : int) (column : int) : int =
  let n = identifiy_code row column in
  let rec loop code k =
    if k = n then code
    else loop (generate_next code) (k + 1)
  in loop 20151125 1


let () =
  let line = read_line () in
  let row, column = read_input line in
  let result = calc_code row column in
  print_int result; print_newline ();
