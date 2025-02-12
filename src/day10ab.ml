let look_and_say (strs : string list) : string list =
  let rec loop acc = function
    | [] -> List.rev acc
    | (x :: _) as xs ->
        let bs = List.take_while ((=) x) xs in
        let k = List.length bs in
        loop (x :: string_of_int k :: acc) (List.drop k xs) in
  loop [] strs


let play_rounds (sequence : string) (nr_rounds : int) : int =
  let rec loop acc k =
    if k = nr_rounds then List.length acc
    else loop (look_and_say acc) (succ k)
  in loop (sequence |> String.to_seq |> Seq.map (String.make 1) |> List.of_seq) 0


let () =
  let line = "1113122113" in
  let nr_rounds = 50 in
  let result = play_rounds line nr_rounds in
  print_int result; print_newline ();
