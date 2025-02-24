(*open Parse*)
(*open Exec_command*)
(* open Unix *)
(*Fonction pour trouver la racine du repo a partir d'un path*)
exception Not_A_Repo


let acc = Object_manager.parse_commit 
"tree 29ff16c9c14e2652b22f8b78bb08a5a07930c147
parent 206941306e8a8af65b66eaaaea388a7ae24d49a0
author Thibault Polge <thibault@thb.lt> 1527025023 +0200
committer Thibault Polge <thibault@thb.lt> 1527025044 +0200

Create first draft"

let () = List.iter (fun (a,b) -> Printf.printf "%s -> %s\n" a b) acc
let () = Printf.printf "%s\n" (Object_manager.write_commit acc)