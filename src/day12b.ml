let filtered_sum_of_numbers (json : Yojson.Basic.t) : int =
  let is_value_red = function
    | (_, `String v) -> v = "red"
    | _ -> false in
  let rec filtered_sum = function
    | `Int n -> n
    | `List objs -> List.fold_left (fun acc obj -> acc + filtered_sum obj) 0 objs
    | `Assoc items ->
        if List.exists is_value_red items then 0
        else List.fold_left (fun acc (_, v) -> acc + filtered_sum v) 0 items
    | _ -> 0 in
  filtered_sum json


let () =
  let json = Yojson.Basic.from_channel stdin in
  let result = filtered_sum_of_numbers json in
  print_int result; print_newline ();
