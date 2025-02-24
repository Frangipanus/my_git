
open Structure

let exec_command (c : command) : unit =
  match c with
  | Init s -> C_init.c_init s
  | Cat (otype, obj) -> Object_manager.cat_file otype obj
  | _ -> print_string "pas implé"; exit 2
