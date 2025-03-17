type character = {hit_point : int; armor : int; damage : int; mana : int}
type spell = {name : string; cost : int; damage : int; armor : int; heal : int; mana : int; timer : int}
type state = {turn : int; player : character; boss : character; active_spells : spell list; mana_spent : int}

let all_spells = [
  {name = "Magic Missile"; cost = 53; damage = 4; armor = 0; heal = 0; mana = 0; timer = 1};
  {name = "Drain"; cost = 73; damage = 2; armor = 0; heal = 2; mana = 0; timer = 1};
  {name = "Shield"; cost = 113; damage = 0; armor = 7; heal = 0; mana = 0; timer = 6};
  {name = "Poison"; cost = 173; damage = 3; armor = 0; heal = 0; mana = 0; timer = 6};
  {name = "Recharge"; cost = 229; damage = 0; armor = 0; heal = 0; mana = 101; timer = 5};
]


let parse_input = function
  | [h; d] ->
      let parse line = Scanf.sscanf line "%s@: %d" (fun _ v -> v) in
      {hit_point = parse h; armor = 0; damage = parse d; mana = 0}
  | _ -> failwith "Malformed input."


let apply_spells (player : character) (boss : character) (spells : spell list) : character * character * (spell list) =
  let mana = List.fold_left (fun acc {mana; _} -> acc + mana) 0 spells in
  let damage = List.fold_left (fun acc {damage; _} -> acc + damage) 0 spells in
  let armor = List.fold_left (fun acc {armor; _} -> acc + armor) 0 spells in
  let heal = List.fold_left (fun acc {heal; _} -> acc + heal) 0 spells in
  let player' = {player with hit_point = player.hit_point + heal; mana = player.mana + mana; armor} in
  let boss' = {boss with hit_point = boss.hit_point - damage} in
  let spells' =
    List.filter_map (fun ({timer; _} as sp) -> if timer = 1 then None else Some {sp with timer = timer - 1}) spells in
  (player', boss', spells')


let play_one_turn ({turn; player; boss; active_spells; mana_spent} as state : state) : state list =
  let player', boss', remaining_spells = apply_spells player boss active_spells in
  if boss'.hit_point <= 0 then
    [{state with turn = turn + 1; player = {player' with armor = 0}; boss = boss'; active_spells = remaining_spells}]
  else if turn mod 2 = 0 then
    let is_spell_available {name; cost; _} =
      cost <= player'.mana && List.for_all (fun sp -> sp.name <> name) remaining_spells in
    match List.filter is_spell_available all_spells with
      | [] -> []
      | available_spells ->
          let cast_spell ({cost; _} as sp) =
            { turn = turn + 1
            ; player = {player' with mana = player'.mana - cost; armor = 0}
            ; boss = boss'
            ; active_spells = sp :: remaining_spells
            ; mana_spent = mana_spent + cost
            } in
          List.map cast_spell available_spells
  else
    let damage_by_boss = max (boss'.damage - player'.armor) 1 in
    let player' = {player' with hit_point = player'.hit_point - damage_by_boss; armor = 0} in
    [{state with turn = turn + 1; player = player'; boss = boss'; active_spells = remaining_spells}]


let get_cheapest_win (player : character) (boss : character) : int =
  let queue = Queue.create () in
  Queue.add {turn = 0; player = player; boss = boss; active_spells = []; mana_spent = 0} queue;
  let rec loop minimum_mana_spent = match Queue.take_opt queue with
    | None -> minimum_mana_spent
    | Some ({player = {hit_point = hp_p; _}; boss = {hit_point = hp_b; _}; mana_spent; _} as state) ->
        if hp_p <= 0 then loop minimum_mana_spent
        else if hp_b <= 0 then loop (min mana_spent minimum_mana_spent)
        else match play_one_turn state with
          | [] -> loop minimum_mana_spent
          | next_states ->
              next_states
                |> List.filter (fun {mana_spent; _} -> mana_spent < minimum_mana_spent)
                |> List.to_seq
                |> Queue.add_seq queue;
              loop minimum_mana_spent
  in loop max_int


let () =
  let lines = In_channel.input_lines stdin in
  let boss = parse_input lines in
  let player = {hit_point = 50; armor = 0; damage = 0; mana = 500} in
  let result = get_cheapest_win player boss in
  print_int result; print_newline ();
