let dict_file = ref "/usr/share/dict/french"

let font_file = ref "/usr/share/fonts/truetype/didot/GFSDidot.otf"

let spec = let open Arg in
  [
    "--dict-file", Set_string dict_file, " set the dictionary file to use";
    "--font-file", Set_string font_file, " set the font file to use";
  ]

let init () =
  Arg.parse spec (fun _ -> ()) "Da typing shooter"
