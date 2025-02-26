(*open Parse*)
(*open Exec_command*)
(* open Unix *)
(*Fonction pour trouver la racine du repo a partir d'un path*)
exception Not_A_Repo


let com = "25cb3132aff375226c08a7a2ae0186c94b68b309"
let () = Object_manager.checkout com