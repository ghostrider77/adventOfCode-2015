let vowels = ['a'; 'e'; 'i'; 'o'; 'u']

let forbidden_words = ["ab"; "cd"; "pq"; "xy"]


let contains_three_vowels (s : string) : bool =
  let nr_vowels = String.fold_left (fun acc c -> if List.mem c vowels then acc + 1 else acc) 0 s
  in nr_vowels >= 3


let contains_double (s : string) : bool =
  let n = String.length s in
  let rec loop ix =
    if ix = n - 1 then false
    else
      let c1 = s.[ix] in
      let c2 = s.[ix+1] in
      c1 = c2 || loop (ix + 1)
  in loop 0


let allowed (s : string) : bool =
  let n = String.length s in
  let rec loop ix =
    if ix = n - 1 then true
    else
      let substring = String.sub s ix 2 in
      if List.mem substring forbidden_words then false else loop (ix + 1)
    in loop 0


let is_nice (str : string) : bool =
  List.for_all ((|>) str) [contains_three_vowels; contains_double; allowed]


let get_nice_words (words : string list) : int =
  List.fold_left (fun acc word -> if is_nice word then acc + 1 else acc) 0 words


let () =
  let lines = In_channel.input_lines stdin in
  let result = get_nice_words lines in
  print_int result; print_newline ();
