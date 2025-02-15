let extract_numbers (line : string) : int list =
  let regexp = Str.regexp "[-+]?[0-9]+" in
  let rec loop acc ix =
    try
      let _ = Str.search_forward regexp line ix in
      let n = line |> Str.matched_string |> int_of_string in
      let next_ix = Str.match_end () in
      loop (n :: acc) next_ix
    with Not_found -> List.rev acc in
  loop [] 0


let () =
  let line = read_line () in
  let numbers = extract_numbers line in
  let result = List.fold_left (+) 0 numbers in
  print_int result; print_newline ();
