open Structure

let help_msg = "bite init [chemin]"

let print_w_help (s : string) : unit =
  let s = "Erreur : "^s^"\n"^"Usage :"^help_msg in print_string s

let parse_init (l : string list) : command =
  match l with
  | [path] -> Init path
  | [] -> Init "."
  | _ -> (print_w_help "Mauvaise nombre d'arguments"; exit 1)

let parse_bite () : command =
  let args_as_l = List.tl @@ Array.to_list @@ Sys.argv in
  match args_as_l with
  | h  :: tl ->
     (
       match h with
       | "init" -> parse_init tl 
       | _ -> print_w_help ("Commande "^h^" inconnue"); exit 1
     )
  | _ -> print_w_help "Aucune commande entrée"; exit 1

