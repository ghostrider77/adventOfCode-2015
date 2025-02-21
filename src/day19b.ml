let parse_molecule (lines: string list) : string =
  match List.drop_while ((<>) "") lines with
    | [""; molecule] -> molecule
    | _ -> failwith "Malfomed input."


let molecule_fabrication (molecule : string) : int =
  let n = String.length molecule in
  let count_occurrences pattern =
    let k = String.length pattern in
    let ixs = Seq.init (n - k + 1) Fun.id in
    Seq.fold_left (fun acc ix -> if pattern = String.sub molecule ix k then acc + 1 else acc) 0 ixs in
  let c1 = count_occurrences "Rn" in
  let c2 = count_occurrences "Ar" in
  let c3 = count_occurrences "Y" in
  let nr_upper_chars = String.fold_left (fun acc c -> if Char.uppercase_ascii c = c then acc + 1 else acc) 0 molecule in
  nr_upper_chars - c1 - c2 - 2*c3 - 1


let () =
  let lines = In_channel.input_lines stdin in
  let molecule = parse_molecule lines in
  let result = molecule_fabrication molecule in
  print_int result; print_newline ();
