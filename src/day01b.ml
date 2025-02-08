let get_first_basement_position (instructions : string) : int =
  let n = String.length instructions in
  let rec loop acc ix =
    if ix = n then failwith "No basement position found."
    else
      let c = instructions.[ix] in
      let acc' = acc + if c = '(' then 1 else -1 in
      if acc' = -1 then ix + 1
      else loop acc' (ix + 1)
  in loop 0 0


let () =
  let line = read_line () in
  let result = get_first_basement_position line in
  print_int result; print_newline ();
