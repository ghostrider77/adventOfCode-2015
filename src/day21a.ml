type item = {name : string; cost : int; damage : int; armor : int}
type player = {hit_points : int; damage : int; armor : int}

let weapons = [
  {name = "Dagger"; cost = 8; damage = 4; armor = 0};
  {name = "Shortsword"; cost = 10; damage = 5; armor = 0};
  {name = "Warhammer"; cost = 25; damage = 6;  armor = 0};
  {name = "Longsword"; cost = 40; damage = 7; armor = 0};
  {name = "Greataxe"; cost = 74; damage = 8; armor = 0};
]

let armors = [
  {name = "Leather"; cost = 13; damage = 0; armor = 1};
  {name = "Chainmail"; cost = 31; damage = 0; armor = 2};
  {name = "Splintmail"; cost = 53; damage = 0; armor = 3};
  {name = "Bandedmail"; cost = 75; damage = 0; armor = 4};
  {name = "Platemail"; cost = 102; damage = 0; armor = 5};
]

let rings = [
  {name = "Damage +1"; cost = 25; damage = 1; armor = 0};
  {name = "Damage +2"; cost = 50; damage = 2; armor = 0};
  {name = "Damage +3"; cost = 100; damage = 3; armor = 0};
  {name = "Defense +1"; cost = 20; damage = 0; armor = 1};
  {name = "Defense +2"; cost = 40; damage = 0; armor = 2};
  {name = "Defense +3"; cost = 80; damage = 0; armor = 3};
]


let parse_input = function
  | [h; d; a] ->
      let parse line = Scanf.sscanf line "%s@: %d" (fun _ v -> v) in
      {hit_points = parse h; damage = parse d; armor = parse a}
  | _ -> failwith "Malformed input."


let rec subsequences = function
  | [] -> [[]]
  | x :: xs ->
      let subseqs = subsequences xs in
      subseqs @ List.(map (cons x) subseqs)


let generate_players (hit_points : int) : (player * int) list =
  let create_player equipments =
    let (d, a, total_cost) = List.fold_left
      (fun (d, a, c) ({damage; armor; cost; _} : item) -> (damage + d, armor + a, cost + c)) (0, 0, 0) equipments in
    ({hit_points; damage = d; armor = a}, total_cost) in
  let ms = List.filter (fun xs -> List.length xs <= 1) @@ subsequences armors in
  let rs = List.filter (fun xs -> List.length xs <= 2) @@ subsequences rings in
  let eqs1 = List.concat_map (fun a -> List.map (fun w -> w :: a) weapons) ms in
  let equipments = List.concat_map (fun e -> List.map (fun r -> e @ r) rs) eqs1 in
  List.map create_player equipments


let does_player_win (player : player) (boss : player) : bool =
  let players_damage = max 1 (player.damage - boss.armor) in
  let bosses_damage = max 1 (boss.damage - player.armor) in
  let rec loop ({hit_points = hp; _} as p) ({hit_points = hb; _} as b) =
    let hb' = hb - players_damage in
    let hp' = hp - bosses_damage in
    if hb' <= 0 then true
    else if hp' <= 0 then false
    else loop {p with hit_points = hp'} {b with hit_points = hb'}
  in loop player boss


let find_cheapest_win (hit_points : int) (boss : player) : int =
  let players = generate_players hit_points in
  List.fold_left (fun acc (p, cost) -> if does_player_win p boss then min cost acc else acc) max_int players


let () =
  let lines = In_channel.input_lines stdin in
  let hit_points = 100 in
  let boss = parse_input lines in
  let result = find_cheapest_win hit_points boss in
  print_int result; print_newline ();
