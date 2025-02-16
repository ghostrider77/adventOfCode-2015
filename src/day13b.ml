module Distances = Map.Make(
  struct
    type t = string * string
    let compare = Stdlib.compare
  end)

module StringSet = Set.Make(String)


let rec permutations (xs : 'a list) : 'a list list =
  let rec interleave x = function
    | [] -> [[x]]
    | (y :: ys) ->
        let tl = List.map (fun z -> y :: z) (interleave x ys) in
        (x :: y :: ys) :: tl in
  match xs with
    | [] -> [[]]
    | (x :: xss) -> List.concat_map (interleave x) (permutations xss)


let parse_input (lines : string list) : int Distances.t =
  let interpret_values p1 action happiness p2 = match action with
    | "gain" -> ((p1, p2), happiness)
    | "lose" -> ((p1, p2), -happiness)
    | _ -> failwith "malformed input." in
  let parse line = Scanf.sscanf line "%s would %s %d happiness units by sitting next to %s@." interpret_values in
  lines |> List.map parse |> Distances.of_list


let add_another_person (distances : int Distances.t) : int Distances.t =
  let person = "aoc2015" in
  let people = Distances.fold (fun (p1, _) _ acc -> StringSet.add p1 acc) distances StringSet.empty in
  let ds = List.concat_map (fun p -> [((p, person), 0); ((person, p), 0)]) @@ StringSet.to_list people in
  Distances.add_seq (List.to_seq ds) distances


let find_optimal_arrangement (distances : int Distances.t) : int =
  let distances = add_another_person distances in
  let people = Distances.fold (fun (p1, _) _ acc -> StringSet.add p1 acc) distances StringSet.empty in
  match StringSet.to_list people with
    | [] -> 0
    | p :: ps ->
        let perms = permutations ps in
        let rec calc_happiness = function
          | [] -> 0
          | [pn] -> Distances.find (pn, p) distances + Distances.find (p, pn) distances
          | a :: b :: rest ->
              Distances.find (a, b) distances + Distances.find (b, a) distances + calc_happiness (b :: rest) in
        List.fold_left (fun acc perm -> max acc (calc_happiness (p :: perm))) min_int perms


let () =
  let lines = In_channel.input_lines stdin in
  let distances = parse_input lines in
  let result = find_optimal_arrangement distances in
  print_int result; print_newline ();
