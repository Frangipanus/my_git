(*open Parse*)
(*open Exec_command*)
(* open Unix *)
(*Fonction pour trouver la racine du repo a partir d'un path*)
exception Not_A_Repo


let acc = Object_manager.read_lines "test.txt"
let () = List.iter (fun x -> Printf.printf "%s\n" x) acc 
