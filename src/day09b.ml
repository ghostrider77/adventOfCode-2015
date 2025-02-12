module Distances = Map.Make(
  struct
    type t = string * string
    let compare = Stdlib.compare
  end
)

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


let parse_input (lines: string list) : int Distances.t =
  let parse line = Scanf.sscanf line "%s to %s = %d" (fun a b dist -> [((a, b), dist); ((b, a), dist)]) in
  lines |> List.map parse |> List.flatten |> Distances.of_list


let find_longest_path (distances : int Distances.t) : int =
  let cities = Distances.fold (fun (a, b) _ acc -> StringSet.(acc |> add a |> add b)) distances StringSet.empty in
  let perms = permutations (StringSet.to_list cities) in
  let rec calc_path_distance = function
    | ([] | [_]) -> 0
    | a :: b :: rest -> (Distances.find (a, b) distances) + calc_path_distance (b :: rest) in
  List.fold_left (fun acc p -> max acc (calc_path_distance p)) 0 perms


let () =
  let lines = In_channel.input_lines stdin in
  let distances = parse_input lines in
  let result = find_longest_path distances in
  print_int result; print_newline ();
