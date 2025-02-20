type coord = {x : int; y : int}

module CoordSet = Set.Make(
  struct
    type t = coord
    let compare = Stdlib.compare
  end)

type configuration = {lights : CoordSet.t; nr_rows : int; nr_cols : int}


let parse_input = function
  | [] -> failwith "Empty input."
  | (row :: _) as rows ->
      let nr_rows = List.length rows in
      let nr_cols = String.length row in
      let lights = rows
        |> List.to_seq
        |> Seq.mapi (fun x r -> Seq.filter_map (fun (y, c) -> if c = '#' then Some {x; y} else None) (String.to_seqi r))
        |> Seq.concat
        |> CoordSet.of_seq in
      let corners =
        [{x = 0; y = 0}; {x = 0; y = nr_cols - 1}; {x = nr_rows - 1; y = 0}; {x = nr_rows - 1; y = nr_cols - 1}] in
      {lights = CoordSet.add_seq (List.to_seq corners) lights; nr_rows; nr_cols}


let run_animation ({nr_rows; nr_cols; _} as initial_state : configuration) (nr_steps : int) : int =
  let get_neighbors {x; y} = [
      {x = x - 1; y}; {x = x - 1; y = y + 1}; {x; y = y + 1}; {x = x + 1; y = y + 1};
      {x = x + 1; y}; {x = x + 1; y = y - 1}; {x; y = y - 1}; {x = x - 1; y = y - 1}
    ] in
  let coords = List.concat_map (fun x -> List.init nr_cols (fun y -> {x; y})) @@ List.init nr_rows Fun.id in
  let corners =
    [{x = 0; y = 0}; {x = 0; y = nr_cols - 1}; {x = nr_rows - 1; y = 0}; {x = nr_rows - 1; y = nr_cols - 1}] in
  let rec loop ({lights; _} as state) k =
    if k = nr_steps then CoordSet.cardinal lights
    else
      let is_light_on c =
        if List.mem c corners then true
        else
          let neighbors_on = c |> get_neighbors |> List.filter (fun n -> CoordSet.mem n lights) |> List.length in
          if CoordSet.mem c lights then (neighbors_on = 2 || neighbors_on = 3)
          else neighbors_on = 3 in
      loop {state with lights = coords |> List.filter is_light_on |> CoordSet.of_list} (k + 1) in
  loop initial_state 0


let () =
  let lines = In_channel.input_lines stdin in
  let grid = parse_input lines in
  let nr_steps = 100 in
  let result = run_animation grid nr_steps in
  print_int result; print_newline ();
