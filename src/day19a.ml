module StringMap = Map.Make(String)
module StringSet = Set.Make(String)


let parse_input (lines: string list) : (string * string list StringMap.t) =
  let parse line = Scanf.sscanf line "%s => %s" (fun a b -> (a, b)) in
  let replacements = List.(lines |> take_while ((<>) "") |> map parse) in
  let n = List.length replacements in
  let molecule = List.nth lines (n + 1) in
  let rules = List.fold_left (fun acc (a, b) -> StringMap.add_to_list a b acc) StringMap.empty replacements in
  (molecule, rules)


let replace_pattern (molecule : string) (pattern : string) (replacements : string list) : StringSet.t =
  let n = String.length molecule in
  let k = String.length pattern in
  let ixs = Seq.init (n - k + 1) Fun.id in
  let extend acc ix =
    let substring = String.sub molecule ix k in
    if substring = pattern then
      let prefix = String.sub molecule 0 ix in
      let suffix = String.sub molecule (ix + k) (n - ix - k) in
      let molecules = List.(replacements |> map (fun r -> prefix ^ r ^ suffix) |> to_seq) in
      StringSet.add_seq molecules acc
    else acc in
  Seq.fold_left extend StringSet.empty ixs


let find_all_possible_replacements (molecule : string) (rules : string list StringMap.t) : int =
  let process pattern replacements acc =
    let molecules = replace_pattern molecule pattern replacements in
    StringSet.union molecules acc in
  StringSet.cardinal @@ StringMap.fold process rules StringSet.empty


let () =
  let lines = In_channel.input_lines stdin in
  let molecule, rules = parse_input lines in
  let result = find_all_possible_replacements molecule rules in
  print_int result; print_newline ();
