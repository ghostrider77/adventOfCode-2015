type coord = {x : int; y : int}
type direction = North | East | South | West

module CoordSet = Set.Make(
  struct
    type t = coord
    let compare = Stdlib.compare
  end)


let direction_of_char = function
  | '^' -> North
  | '>' -> East
  | 'v' -> South
  | '<' -> West
  | _ -> failwith "Unknown direction."


let parse_input (line : string) : direction list =
  line |> String.to_seq |> Seq.map direction_of_char |> List.of_seq


let deliver_presents (directions : direction list) : int =
  let rec loop acc {x; y} = function
    | [] -> CoordSet.cardinal acc
    | d :: ds ->
        let next_coord = match d with
          | North -> {x = x - 1; y}
          | East -> {x; y = y + 1}
          | South -> {x = x + 1; y}
          | West -> {x; y = y - 1} in
        loop (CoordSet.add next_coord acc) next_coord ds
  in loop (CoordSet.singleton {x = 0; y = 0}) {x = 0; y = 0} directions


let () =
  let line = read_line () in
  let directions = parse_input line in
  let result = deliver_presents directions in
  print_int result; print_newline ();
