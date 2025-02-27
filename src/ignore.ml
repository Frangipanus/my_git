open Str
(*Le path = la localisation du repo *)

let base_to_ignore = List.map regexp_string ["."; ".."; ".bit"; "_ILP_tree"; "mygit.exe"] 

let bite_to_ignore (path : string) : regexp list =
  (List.map regexp) (Utils.read_lines (path^".biteignore"))

let to_ignore (path : string) : regexp list =
  base_to_ignore @ (bite_to_ignore path)

let ignore_file (f : string) (path : string) : bool =
  List.exists (fun r -> string_match r f 0) (to_ignore path)
