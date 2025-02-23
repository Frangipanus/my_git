(*open Parse*)
(*open Exec_command*)
open Unix
(*Fonction pour trouver la racine du repo a partir d'un path*)
exception Not_A_Repo


let () = Object_manager.compress_file "test.txt" "test2.zip"
let () = Object_manager.comp_obj "." "Hello world" "Paulo"
let acc = Object_manager.sha1_hash ("Paulo"^"Hello world")
let zebi = Object_manager.decomp_obj "." acc
let () = Printf.printf "%s\n" zebi