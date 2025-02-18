type ingredient = {capacity : int; durability : int; flavor : int; texture : int; calories : int}


let parse_input (lines : string list) : ingredient list =
  let extract _ capacity durability flavor texture calories = {capacity; durability; flavor; texture; calories} in
  let parse line = Scanf.sscanf line "%s capacity %d, durability %d, flavor %d, texture %d, calories %d" extract in
  List.map parse lines


let calc_total_score (ingredients : ingredient list) (coeffs : int list) : int =
  let c = List.fold_left2 (fun acc {capacity; _} k -> acc + capacity * k) 0 ingredients coeffs in
  let d = List.fold_left2 (fun acc {durability; _} k -> acc + durability * k) 0 ingredients coeffs in
  let f = List.fold_left2 (fun acc {flavor; _} k -> acc + flavor * k) 0 ingredients coeffs in
  let t = List.fold_left2 (fun acc {texture; _} k -> acc + texture * k) 0 ingredients coeffs in
  let cal = List.fold_left2 (fun acc {calories; _} k -> acc + calories * k) 0 ingredients coeffs in
  if c < 0 || d < 0 || f < 0 || t < 0 || cal <> 500 then 0
  else c * d * f * t


let calc_max_score (ingredients : ingredient list) (limit : int) : int =
  let n = List.length ingredients in
  let rec calc_product k =
    if k = 1 then Seq.init (limit + 1) (fun ix -> [ix])
    else
      let xs = calc_product (k - 1) in
      Seq.concat_map (fun x -> Seq.init (limit + 1) (fun ix -> ix :: x)) xs in
  let add_last_coeff cs =
    let sum = List.fold_left (+) 0 cs in
    let c = limit - sum in
    if c < 0 then None else Some (c :: cs) in
  let coeffs = Seq.filter_map add_last_coeff @@ calc_product (n - 1) in
  Seq.fold_left (fun acc cs -> max acc @@ calc_total_score ingredients cs) 0 coeffs


let () =
  let lines = In_channel.input_lines stdin in
  let ingredients = parse_input lines in
  let k = 100 in
  let result = calc_max_score ingredients k in
  print_int result; print_newline ();
