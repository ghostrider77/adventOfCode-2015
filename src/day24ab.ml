type group = {qe : int; sum : int; size : int}


let calc_first_group (package_weights : int list) (nr_groups : int) : int =
  let target = (List.fold_left (+) 0 package_weights) / nr_groups in
  let rec loop acc = function
    | [] ->
        let valid_groups = List.filter (fun {sum; _} -> sum = target) acc in
        let process (smallest_size, smallest_qe) {qe; size; _} =
          if size > smallest_size then (smallest_size, smallest_qe)
          else if size = smallest_size && qe > smallest_qe then (smallest_size, smallest_qe)
          else (size, qe) in
        snd @@ List.fold_left process (max_int, max_int) valid_groups
    | w :: ws ->
        let extend {qe; sum; size} =
          if w + sum > target then None
          else Some {qe = w * qe; sum = sum + w; size = size + 1} in
        let acc' = acc @ (List.filter_map extend acc) in
        loop acc' ws
  in loop [{qe = 1; sum = 0; size = 0}] package_weights


let () =
  let lines = In_channel.input_lines stdin in
  let package_weights = List.sort (Fun.flip compare) @@ List.map int_of_string lines in
  let nr_groups = 3 in
  let result = calc_first_group package_weights nr_groups in
  print_int result; print_newline ();
