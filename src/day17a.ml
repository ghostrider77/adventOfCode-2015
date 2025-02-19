let calc_nr_combinations (sizes : int list) (amount : int) : int =
  let sorted_sizes = List.sort compare sizes in
  let rec loop x = function
    | [] -> if x = 0 then 1 else 0
    | size :: rest ->
        if size > x then loop x rest
        else loop x rest + loop (x - size) rest in
  loop amount sorted_sizes


let () =
  let lines = In_channel.input_lines stdin in
  let container_sizes = List.map int_of_string lines in
  let amount = 150 in
  let result = calc_nr_combinations container_sizes amount in
  print_int result; print_newline ();
