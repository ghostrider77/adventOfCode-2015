module StringMap = Map.Make(String)

type reindeer = {name : string; velocity : int; stamina : int; rest_time : int}


let parse_input (lines : string list) : reindeer list =
  let extract name velocity stamina rest_time = {name; velocity; stamina; rest_time} in
  let parse line = Scanf.sscanf line "%s can fly %d km/s for %d seconds, but then must rest for %d seconds." extract in
  List.map parse lines


let find_winner (reindeers : reindeer list) (race_time : int) : int =
  let calc_distance {velocity; stamina; rest_time; _} time =
    let k = time / (stamina + rest_time) in
    let m = time mod (stamina + rest_time) in
    k * stamina * velocity + (min m stamina) * velocity in
  let increment = function
    | None -> Some 1
    | Some n -> Some (n + 1) in
  let rec loop leaders time =
    if time > race_time then StringMap.fold (fun _ k acc -> max k acc) leaders 0
    else
      let distances = List.map (fun reindeer -> calc_distance reindeer time) reindeers in
      let max_dist = List.fold_left max 0 distances in
      let leading_deers =
        distances
          |> List.combine reindeers
          |> List.filter_map (fun ({name; _}, d) -> if d = max_dist then Some name else None) in
      let leaders' = List.fold_left (fun acc name -> StringMap.update name increment acc) leaders leading_deers in
      loop leaders' (time + 1)
    in loop StringMap.empty 1


let () =
  let lines = In_channel.input_lines stdin in
  let reindeers = parse_input lines in
  let race_time = 2503 in
  let result = find_winner reindeers race_time in
  print_int result; print_newline ();
