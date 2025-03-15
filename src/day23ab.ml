type registers = {a : int; b : int}
type instruction =
  | Hlf of {register : string}
  | Tpl of {register : string}
  | Inc of {register : string}
  | Jmp of {offset : int}
  | Jie of {register : string; offset : int}
  | Jio of {register : string; offset : int}

type state = {position : int; registers : registers}


let read_instructions (lines : string list) : instruction array =
  let parse line =
    let filtered = line |> String.to_seq |> Seq.filter ((<>) ',') |> String.of_seq in
    match String.split_on_char ' ' filtered with
      | ["hlf"; register] -> Hlf {register}
      | ["tpl"; register] -> Tpl {register}
      | ["inc"; register] -> Inc {register}
      | ["jmp"; offset] -> Jmp {offset = int_of_string offset}
      | ["jie"; register; offset] -> Jie {register; offset = int_of_string offset}
      | ["jio"; register; offset] -> Jio {register; offset = int_of_string offset}
      | _ -> failwith "Cannot parse instruction." in
  Array.of_list @@ List.map parse lines


let execute ({position; registers = {a; b}} as state : state) = function
  | Hlf {register = "a"} -> {position = position + 1; registers = {a = a / 2; b}}
  | Hlf {register = "b"} -> {position = position + 1; registers = {a; b = b / 2}}
  | Tpl {register = "a"} -> {position = position + 1; registers = {a = 3 * a; b}}
  | Tpl {register = "b"} -> {position = position + 1; registers = {a; b = 3 * b}}
  | Inc {register = "a"} -> {position = position + 1; registers = {a = a + 1; b}}
  | Inc {register = "b"} -> {position = position + 1; registers = {a; b = b + 1}}
  | Jmp {offset} -> {state with position = position + offset}
  | Jie {register = "a"; offset} -> {state with position = position + (if a mod 2 = 0 then offset else 1)}
  | Jie {register = "b"; offset} -> {state with position = position + (if b mod 2 = 0 then offset else 1)}
  | Jio {register = "a"; offset} -> {state with position = position + (if a = 1 then offset else 1)}
  | Jio {register = "b"; offset} -> {state with position = position + (if b = 1 then offset else 1)}
  | _ -> failwith "Instruction is not understood."


let run_code (instructions : instruction array) (initial_value : int) : int =
  let n = Array.length instructions in
  let rec loop ({position; registers = {b; _}} as state) =
    if position < 0 || position >= n then b
    else
      let instruction = instructions.(position) in
      let state' = execute state instruction in
      loop state' in
  loop {position = 0; registers = {a = initial_value; b = 0}}


let () =
  let lines = In_channel.input_lines stdin in
  let instructions = read_instructions lines in
  let initial_value = 1 in
  let result = run_code instructions initial_value in
  print_int result; print_newline ();
