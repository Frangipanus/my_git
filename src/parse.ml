open Structure

let help_msg = "
                Backup Integration Tool Engine
                \n\tbite init [chemin] \
                \n\tbite commit [-m message]\
                \n\tbite cat-file <type d'objet> <objet>\
                \n\tbite hash-object [-w] [-t type d'objet] <fichier>
                \n\tcheckout <sha>
                \n\tbranch_create <name>
                \n\tbranch_list
                \n\tbranch_checkout <name>
                \n\tmerge <branch>
                \n\tlink <url>
                \n\tpush
                \n\tbackup <url>
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
    (List.mem "-w" l, List.filter ((=) "-w") l) in     
  let get_type (l : string list) : string * (string list) =
    let indt = List.find_index (fun x -> x = "-t") l in
    (match indt with
     | None -> ("blob", l)
     | Some i ->         
        if (List.length l) - 1 <> i then
          (List.nth l (i+1), List.filteri (fun j _ -> j <> i && j <> (i+1)) l)  
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
  Hash (t, n, s)   
          
let parse_checkout args = Checkout (List.hd args)

let parse_branch_create args = Branch_create (List.hd args)

let parse_branch_checkout args = Branch_checkout (List.hd args)
 
let parse_merge args =  Merge (List.hd args)

let parse_link args = Link (List.hd args) 

let parse_backup args = Backup (List.hd args)

let parse_push = Push 

let parse_bite () : command =
  let args_as_l = List.tl @@ Array.to_list @@ Sys.argv in
  match args_as_l with
  | h  :: tl ->
     (match h with
      | "log" -> Log
      | "init" -> parse_init tl 
      | "add" -> parse_add tl
      | "cat-file" -> parse_catfile tl
      | "hash-object" -> parse_hash tl
      | "commit" -> parse_commit tl 
      |"checkout" -> parse_checkout tl
      | "branch_list" -> Branch_list
      | "branch_create" -> parse_branch_create tl
      | "branch_checkout" -> parse_branch_checkout tl 
      | "merge" -> parse_merge tl
      | "remove" -> Remove(List.hd tl)
      | "link" -> parse_link tl
      | "backup" -> parse_backup tl
      | "push" -> parse_push
      | _ -> print_w_help ("Commande "^h^" inconnue"); exit 1)
  | _ -> print_w_help "Aucune commande entrée"; exit 1

