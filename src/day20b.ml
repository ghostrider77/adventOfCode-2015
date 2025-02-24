let calc_nr_of_presents_delivered (n : int) : int =
  let limit = n |> float |> sqrt |> int_of_float in
  let rec loop acc d =
    if d > limit then acc
    else if n mod d <> 0 then loop acc (d + 1)
    else if d * d = limit then
      let acc' = if 50 * d >= n then acc + 11 * d else acc in loop acc' (d + 1)
    else
      let acc' = if 50 * d >= n then acc + 11 * d else acc in
      let k = n / d in
      let acc'' = if 50 * k >= n then acc' + 11 * k else acc' in
      loop acc'' (d + 1) in
  loop 0 1


let find_smallest_house_number (target : int) : int =
  Option.get (Seq.find_map (fun n -> if (calc_nr_of_presents_delivered n) >= target then Some n else None) (Seq.ints 1))


let () =
  let target = 33100000 in
  let result = find_smallest_house_number target in
  print_int result; print_newline ();
