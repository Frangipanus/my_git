(*open Parse*)
(*open Exec_command*)
(* open Unix *)
(*Fonction pour trouver la racine du repo a partir d'un path*)
exception Not_A_Repo


let com = "897888be1f9b1115c5e641ee0b9b02aadf9072d8"
let () = Object_manager.checkout com