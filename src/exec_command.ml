
open Structure

let exec_command (c : command) : unit =
  match c with
  | Init s ->
     Printf.printf "Dépôt bite initialisé dans %s" s; 
     C_init.c_init s
  | Cat (otype, obj) ->
     Object_manager.cat_file otype obj
  | Hash (objtype, file, todir) ->
     let h =
       (if todir then Object_manager.hash_object_directory
        else  Object_manager.hash_object_stdout) in
     (h objtype file)
  | Log -> git_log ()
  | _ -> print_string "pas implé"; exit 2
