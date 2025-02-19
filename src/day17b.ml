type inventory = {capacity : int; containers : int list}


let calc_combinations (sizes : int list) (amount : int) : int =
  let sorted_sizes = List.sort compare sizes in
  let rec loop acc = function
    | [] ->
        let valid_containers {capacity; containers} =
          if capacity = amount then Some (List.length containers) else None in
        let nr_containers = List.filter_map valid_containers acc in
        let smallest_size = List.fold_left min max_int nr_containers in
        List.fold_left (fun count s -> if s = smallest_size then count + 1 else count) 0 nr_containers
    | size :: rest ->
        let extend c {capacity; containers} =
          let capacity' = capacity + c in
          if capacity' > amount then None
          else Some {capacity = capacity'; containers = c :: containers} in
        loop ((List.filter_map (extend size) acc) @ acc) rest in
  loop [{capacity = 0; containers = []}] sorted_sizes


let () =
  let lines = In_channel.input_lines stdin in
  let container_sizes = List.map int_of_string lines in
  let amount = 150 in
  let result = calc_combinations container_sizes amount in
  print_int result; print_newline ();
