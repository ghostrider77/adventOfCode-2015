module IntMap = Map.Make(Int)


let extract_prime_factor (n : int) (p : int) : int * int =
  let rec loop m exponent =
    if m mod p = 0 then loop (m / p) (exponent + 1)
    else (m, exponent) in
  loop n 0


let calc_factorization (n : int) : int IntMap.t =
  let n', e2 = extract_prime_factor n 2 in
  let factorization = if e2 > 0 then IntMap.singleton 2 e2 else IntMap.empty in
  let limit = n |> float |> sqrt |> int_of_float in
  let rec loop acc m p =
    if p > limit then if m > 1 then IntMap.add m 1 acc else acc
    else
      let m', exponent = extract_prime_factor m p in
      let acc' = if exponent > 0 then IntMap.add p exponent acc else acc in
      loop acc' m' (p + 2) in
  loop factorization n' 3


let calc_sum_of_divisors (n : int) : int =
  let factorization = calc_factorization n in
  let power n k = Seq.(Fun.id |> Seq.init k |> fold_left (fun acc _ -> acc * n) 1) in
  IntMap.fold (fun p e acc -> acc * ((power p (e + 1)) - 1) / (p - 1)) factorization 1


let find_smallest_house_number (target : int) : int =
  Option.get @@ Seq.find_map (fun n -> if (calc_sum_of_divisors n) >= target then Some n else None) @@ Seq.ints 1


let () =
  let target = 33100000 in
  let result = find_smallest_house_number (target / 10) in
  print_int result; print_newline ();
