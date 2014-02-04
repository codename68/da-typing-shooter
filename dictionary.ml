open CamomileLibrary

type t = {
  words: (int * UChar.t, UTF8.t) Hashtbl.t;
  word_count: (int * UChar.t, int) Hashtbl.t;
  letters: UChar.t list;
}

let dict_file = "/usr/share/dict/french"

let add dict count word =
  let c = UTF8.get word 0 in
  let size = UTF8.length word in
  Hashtbl.add dict (size, c) word;
  begin
    try
      let x = Hashtbl.find count (size, c) in
      Hashtbl.replace count (size, c) (x + 1);
    with
    | Not_found ->
      Hashtbl.add count (size, c) 1
  end;
  c
  
let init () =
  let dict = Hashtbl.create 1000 in
  let count = Hashtbl.create 1000 in
  let ch = open_in dict_file in
  let letters = ref [] in
  try
    while true do
      let w = input_line ch in
      let c = add dict count w in
      if not (List.mem c !letters) then
        letters := c :: !letters
    done;
    failwith "omega"
  with End_of_file ->
    {
      words = dict;
      word_count = count;
      letters = !letters;
    }

let count dict size c =
  try
    Hashtbl.find dict.word_count (size, c)
  with
  | Not_found -> 0

exception Break of string

let get_random_word dict size avoid_letters =
  let letters =
    List.filter (fun c -> not (List.mem c avoid_letters)) dict.letters
  in
  let counts = List.map (fun c -> c, count dict size c) letters in
  let sum = List.fold_left (fun acc (_, count) -> acc + count) 0 counts in
  if sum <= 0 then
    None
  else
    let index = ref (Random.int sum) in
    try
      List.iter
        (fun (c, count) ->
           if !index >= count then
             index := !index - count
           else
             let l = Hashtbl.find_all dict.words (size, c) in
             let word = List.nth l !index in
             raise (Break word))
        counts;
      None
    with Break w -> Some w
  
