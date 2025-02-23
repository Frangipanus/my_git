(*open Parse*)
(*open Exec_command*)
open Unix
(*Fonction pour trouver la racine du repo a partir d'un path*)
exception Not_A_Repo
let equal_node n1 n2 = 
  (n1.st_dev = n2.st_dev) && (n1.st_ino = n2.st_ino)

let rec find_repo (path : string) = 
  if C_init.has_bite path then path else begin 
  let acc = path^"/.." in 
  let node1 = Unix.stat path in 
  let node2 = Unix.stat acc in
  if equal_node node1 node2 then (Printf.printf "%s\n" path;raise Not_A_Repo)
  else begin 
    find_repo acc
  end

end

   

let () = Printf.printf "%b\n" (C_init.has_bite ".")
let path = find_repo "."
let () = Printf.printf "%s\n" path


