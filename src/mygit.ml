(*open Parse*)
(*open Exec_command*)
open Unix
(*Fonction pour trouver la racine du repo a partir d'un path*)
exception Not_A_Repo


let () = Object_manager.compress_file "test.txt" "test2.zip"

let nul = string_of_int (0x00)
let acc = Object_manager.sha1_hash ("Paulo"^nul^"Hello world")

let () = Object_manager.comp_obj "." "Hello world" ("Paulo"^nul)
let zebi = Object_manager.decomp_obj "." acc
let () = Object_manager.cat_file "blob" acc
let () = Printf.printf "%s\n" zebi