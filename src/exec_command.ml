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
  | Commit m ->
     print_string
       (Object_manager.bite_commit
       m
       "auteur non implémenté"
       "commiteur non implémenté")
  | Log -> Object_manager.git_log ()
  |Chekout(s) -> Object_manager.checkout s
  |Branch_list -> Object_manager.branch_list "."
  |Branch_checkout(s) -> Object_manager.branch_checkout s 
  |Branch_create(s) -> Object_manager.branch_create s
  |Merge(s) -> Object_manager.merge s
  | _ -> print_string "pas implé"; exit 2
