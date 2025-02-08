let find_seed (key : string) : int =
  let rec loop n =
    let s = key ^ (string_of_int n) in
    let digest = Digest.MD5.string s in
    if String.starts_with ~prefix:"000000" (Digest.MD5.to_hex digest) then n
    else loop (n + 1)
  in loop 1


let () =
  let secret_key = "bgvyzdsv" in
  let result = find_seed secret_key in
  print_int result; print_newline ();
