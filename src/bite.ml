(*
  CHANTIER POUR LES TESTS
  open Parse *)
(* open Exec_command *)
(* (\* open Unix *\) *)
(* (\*Fonction pour trouver la racine du repo a partir d'un path*\) *)
(* exception Not_A_Repo *)


(* let com = "7e164939c6aec32691845151c7e8f4389da26513" *)
(* let () = Object_manager.checkout com *)


(*Vrai code de mygit *)

open Parse
open Exec_command
let com = parse_bite ()
let () = exec_command com
