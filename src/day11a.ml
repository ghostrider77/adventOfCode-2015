module IntSet = Set.Make(Int)

let password_length = 8

let forbidden = List.map Char.code ['i'; 'o'; 'l']


let is_valid (password : int array) : bool =
  let condition1 () =
    let rec loop ix =
      if ix = password_length - 2 then false
      else
        let elem = password.(ix) in
        (password.(ix + 1) = elem + 1 && password.(ix + 2) = elem + 2) || loop (ix + 1)
    in loop 0 in
  let condition2 () = List.for_all (fun code -> not (Array.mem code password)) forbidden in
  let condition3 () =
    let rec loop acc ix =
      if ix = password_length - 1 then IntSet.cardinal acc >= 2
      else
        let c1 = password.(ix) in
        let c2 = password.(ix + 1) in
        let acc' = if c1 = c2 then IntSet.add c1 acc else acc in
        loop acc' (ix + 1) in
    loop IntSet.empty 0 in
  condition1 () && condition2 () && condition3 ()


let find_valid_password (current_password : string) : string =
  let password = current_password |> String.to_seq |> Seq.map Char.code |> Array.of_seq in
  let code_a = Char.code 'a' in
  let code_z = Char.code 'z' in
  let next () =
    let rec loop ix =
      if ix < 0 then ()
      else
        let code = password.(ix) in
        if code = code_z then
          (password.(ix) <- code_a;
          loop (ix - 1);)
        else
          password.(ix) <- code + 1;
          () in
    loop (password_length - 1) in
  while not (is_valid password) do
    next ();
  done;
  password |> Array.map (Fun.compose (String.make 1) Char.chr) |> Array.to_list |> String.concat ""


let () =
  let initial_password = "hxbxwxba" in
  let result = initial_password |> find_valid_password in
  print_endline result;
