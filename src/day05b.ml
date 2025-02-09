module StringMap = Map.Make(String)


let contains_pair_of_two_grams (str : string) : bool =
  let n = String.length str in
  let has_non_overlapping_start = function
    | [] -> false
    | ix1 :: rest -> List.exists (fun ix2 -> abs (ix1 - ix2) >= 2) rest in
  let rec loop acc ix =
    if ix = n - 1 then
      let start_ixs = acc |> StringMap.to_seq |> Seq.map snd in
      Seq.exists has_non_overlapping_start start_ixs
    else
      let two_gram = String.sub str ix 2 in
      loop (StringMap.add_to_list two_gram ix acc) (ix + 1)
  in n < 2 || loop StringMap.empty 0


let has_symmetric_triplet (str : string) : bool =
  let rec loop = function
    | a :: b :: c :: rest ->
        if a = c then true
        else loop (b :: c :: rest)
    | _ -> false
  in loop (str |> String.to_seq |> List.of_seq)


let is_nice (str : string) : bool =
  List.for_all ((|>) str) [contains_pair_of_two_grams; has_symmetric_triplet]


let get_nice_words (words : string list) : int =
  List.fold_left (fun acc word -> if is_nice word then acc + 1 else acc) 0 words


let () =
  let lines = In_channel.input_lines stdin in
  let result = get_nice_words lines in
  print_int result; print_newline ();
