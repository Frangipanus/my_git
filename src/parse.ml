open Structure

let help_msg = "\n\tbite init [chemin] \\
                \n\tbite commit [-m message]\\
                \n\tbite cat-file <type d'objet> <objet>\\
                \n\tbite hash-object [-w] [-t type d'objet] <fichier> 
                "

let print_w_help (s : string) : unit =
  let s = "Erreur : "^s^"\n"^"Usage : "^help_msg in print_string s

let parse_init (l : string list) : command =
  match l with
  | [path] -> Init path
  | [] -> Init "."
  | _ -> (print_w_help "Mauvais nombre d'arguments"; exit 1)

(*Implémenter des regexp ici*)
let parse_add (l : string list) : command = Add l

let parse_remove (l : string list) : command = Remove l

let parse_commit (l : string list) : command =
  match l with
  | "-m" :: [msg] -> Commit msg
  | [] -> Commit ""
  | _ -> (print_w_help  "Mauvais nombre d'arguments"; exit 1)

let parse_catfile (l : string list) : command =
  match l with
  | [otype; obj] -> Cat (otype, obj)
  | _ -> (print_w_help "Mauvais nombre d'arguments"; exit 1)

let parse_hash (l : string list) : command =
  let getstore (l : string list) : bool * (string list) =
    (List.mem "-w", List.filter ((=) "-w") in     
  let get_type (l : string list) : string * (string list) =
    let indt = List.find_index (fun x -> x = "-t") in
    (match indt with
     | None -> ("blob", l)
     | Some i ->         
        if (List.length l) - 1 <> i then
          (List.nth l (i+1), List.filteri (fun j _ -> j <> i && j <> (i+1)))  
        else
          (print_w_help "-t : type d'objet non spécifié"; exit 1)) in
  let get_fname (l : string list) : string =
    if List.length l <> 1 then
      (print_w_help  "Mauvais nombre d'arguments"; exit 1)
    else
      List.hd l
  in
  let s, l = getstore l in
  let t, l = get_type l in
  let n = get_fname l in
  (t, n, s)   
           
let parse_bite () : command =
  let args_as_l = List.tl @@ Array.to_list @@ Sys.argv in
  match args_as_l with
  | h  :: tl ->
     (match h with
        | "init" -> parse_init tl 
       | "add" -> parse_add tl
       | "cat-file" -> parse_catfile tl
       | "hash-object" -> parse_hash tl 
       | _ -> print_w_help ("Commande "^h^" inconnue"); exit 1)
  | _ -> print_w_help "Aucune commande entrée"; exit 1

