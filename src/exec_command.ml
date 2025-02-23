open C_init
open Structure

let exec_command (c : command) : unit =
  match c with
  | Init s -> c_init s
  | _ -> print_string "pas implé"; exit 2
