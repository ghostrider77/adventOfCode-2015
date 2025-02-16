type reindeer = {velocity : int; stamina : int; rest_time : int}


let parse_input (lines : string list) : reindeer list =
  let extract _ velocity stamina rest_time = {velocity; stamina; rest_time} in
  let parse line = Scanf.sscanf line "%s can fly %d km/s for %d seconds, but then must rest for %d seconds." extract in
  List.map parse lines


let find_winner (reindeers : reindeer list) (race_time : int) : int =
  let calc_distance {velocity; stamina; rest_time; _} =
    let k = race_time / (stamina + rest_time) in
    let m = race_time mod (stamina + rest_time) in
    k * stamina * velocity + (min m stamina) * velocity in
  List.fold_left (fun acc reindeer -> max acc (calc_distance reindeer)) 0 reindeers


let () =
  let lines = In_channel.input_lines stdin in
  let reindeers = parse_input lines in
  let race_time = 2503 in
  let result = find_winner reindeers race_time in
  print_int result; print_newline ();
