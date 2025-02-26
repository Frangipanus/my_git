open Utils
open Unix

(**************************Object managing**********************************)

let log = ref [] (*Historique de commits*)

(*prend le sha et le bite_path et revoie un objet en string avec le header
Raise Not_An_Object si l'objet avec ce sha n'existe pas*)

exception Not_An_Object
exception Not_An_Object2

let decomp_obj (bite_path : string) (sha : string) : string =
  let path = bite_path^"/.bite/objects/" in
  let path = path^(String.make 1 sha.[0])^(String.make 1 sha.[1]) in      
  if Sys.file_exists path then 
    (let suf = "/"^(String.sub sha 2 (String.length sha -2)) in
     let pathsuf = path^suf in 
     if Sys.file_exists pathsuf then (
       let path_ilp = path^"/_ILP" in 
       (decompress_file pathsuf path_ilp);
       let acc = read_whole_file path_ilp in
       unlink path_ilp; acc (*Là on a l'objet décompressé*))
     else
       raise Not_An_Object)
  else 
    raise Not_An_Object2

(*renvoie le sha1 de l'object*)
let comp_obj (obj_path : string) (obj : string) (header : string) : string =
  (*obj_path est le DOSSIER ou est l'objet, penser à mettre la taille mais ca c'est pour plus tard tkt marius ca sera rapide*)
  let file = header^obj in 
    let hashed = sha1_hash file in 
    try
      let accu = (find_repo obj_path) in
      let path = accu^"/.bite/objects/" in 
      let path = path ^(String.make 1 hashed.[0])^(String.make 1 hashed.[1]) in
      let hpath =  (path^"/"^(String.sub hashed 2 (String.length hashed -2))) in 
      let obj_path_ilp = obj_path^"/_ILP" in  
      if Sys.file_exists path then (
        let oc = open_out obj_path_ilp in
        (* create or truncate file, return channel *)
        Printf.fprintf oc "%s" file;
        (* write something *)
        close_out oc;
        compress_file obj_path_ilp hpath;
        unlink obj_path_ilp;
        hashed)
      else
        (mkdir path 0o755;
         let oc = open_out obj_path_ilp in
         (* create or truncate file, return channel *)
         Printf.fprintf oc "%s" file;
         (* write something *)
         close_out oc;
         compress_file obj_path_ilp hpath;
         unlink obj_path_ilp;
         hashed)
    with 
    | Not_A_Repo -> failwith  "Path is not in a bite repo"
    | err -> raise err

let cat_file (objtype : string) (obj_ident : string) : unit = 
    let acc = decomp_obj (find_repo ".")  (obj_ident) in 
    let lst = String.split_on_char (space).[0] acc in 
    match objtype, lst with 
    |"blob", _ :: q :: [] -> Printf.printf "%s\n" q; (*en vrai faut enelver le header je ferai ca quand je comprednrai si il est dans obj*)
    |_ -> failwith "Affichage d'autre objet que les blobs pas implémenté"

(*Les deux versions de hash-object, c'est le parsing qui dit laquelle on appelle. le type par default est blob si non précisé*)
let hash_object_stdout (objtype : string) (file : string) : unit = 
    let file_str = read_whole_file file in 
    let length = String.length file_str in 
    let acc = objtype^space^(string_of_int length)^nul^file_str in 
    let res = sha1_hash acc in 
    Printf.printf "%s\n" res

let hash_object_directory (objtype : string) (file : string) : unit = 
  let path_to_file = "./"^file in 
  let lst1 = String.split_on_char ('/') path_to_file in 
    let rec aux lst res = 
      match lst with 
      |h::q-> aux q (res^"/"^h) 
      |_ -> res 
    in 
    let path = aux lst1 "." in 
    let file_str = read_whole_file file in 
    let length = String.length file_str in 
    let head = objtype^space^(string_of_int length)^nul in
    let _ = comp_obj path file_str head  in ()


let parse_commit (commit : string) : (string*string) list=  (*On prend en entrée un string décompresser et on renvoie un type commit*)
  let lines = String.split_on_char '\n' commit in 
  let rec aux line  (res : (string*string) list) (a,b)= 
    match line with 
        |[] -> res, []
        |h::q ->
          if (String.length h) = 0 then
            (res, line)
          else (if h.[0] <> ' ' then
                  (let acc = (String.split_on_char ' ' h) in
                   let acc2 = toutsauflepremier acc "" in
                   aux q (res@[(a,b)]) (List.hd acc, acc2))
                else
                  (aux q res (a,b^(String.sub h 1 (String.length h -1)))))
    in
    let (key_value , message) = aux lines [] ("", "") in 
    let message_vrai = toutsauflepremier (("ratio")::(message)) "" in 
    List.tl(key_value@["message", message_vrai])

let write_commit (commit : (string*string) list) : string  = 
    let rec aux lst acc = 
        match lst with 
        | (a,b) :: q :: t -> aux (q::t) (acc^a^" "^b^"\n")
        |(_,b) :: [] -> (acc ^"\n"^b)
        |[] -> failwith "not a commit"
    in aux commit ""

let rec print_shit (lst : (string * string) list) : unit = 
    match lst with 
    | (a,b) :: q  when not(String.equal a "message") ->
       (Printf.printf "%s %s\n" a b; print_shit q)
    | ("message", b) :: q -> (Printf.printf "%s\n" b; print_shit q)
    |_ -> ()

let read_object (path : string) (sha : string) : string * string = (*Renvoie le header et l'object*)
    let bite_path = find_repo path in 
    let object_with_header = decomp_obj bite_path sha in     
    let lst = String.split_on_char (nul.[0]) object_with_header in 
    (List.hd lst,toutsauflepremier lst nul )

let read_header (head : string) : string*string = (*Head n'a pas le null a la fin*)
    let lst1 = String.split_on_char (space.[0]) head in 
    let types = List.hd lst1 in 
    let size = toutsauflepremier lst1 nul in 
    (types, size)

let rec treat_obj (path : string) (sha : string) : unit =
  (*Path est l'endroit ou c'est mis. Ca peut etre un file ou un dossier*) 
  if (Sys.file_exists path && Sys.is_directory path) then (
    let head, obj = read_object path sha in 
    let types, _ = read_header head in
    match types with 
    |"blob" -> failwith "blob is not a directoru"
    |"tree" -> treat_tree obj path 
    |_ -> failwith "not yet")
  else
    (
      let lst = String.split_on_char '/' path in 
      let head, obj = read_object (toutsaufledernier lst "/") sha in 
      let _, size = read_header head in
      treat_blob obj path
    )
and treat_tree (tree : string) (path : string) : unit  = (*tree est l'arbres*) (*tree = mode shaNULpath *)
  if ((not(C_init.has_bite path))) then
    (Printf.printf "no problem2\n";
     if Sys.file_exists path then
       rmrf path; mkdir path (0o755)) else ();
    Printf.printf "tree is \n%s\n path is %s\n" tree path;
    let content = String.split_on_char '\n' tree in (*Une liste des triplet de la forme (autorisation, sha, path)*)
    
    List.iter (fun x -> if String.length x = 0 then () else (
                let lst = String.split_on_char (space.[0]) x in 
              let auto = List.hd lst in  
              let acc = toutsauflepremier lst space in 
              let lst2 = String.split_on_char (nul.[0]) acc in 
              let sha = List.hd lst2 in 
              Printf.printf "supposed sha = %s path  = %s \n " sha (toutsauflepremier lst2 nul) ;
              let path2 = path^"/"^(toutsauflepremier lst2 nul) in 
              treat_obj path2 sha)) content

and treat_blob blob path= 
    Printf.printf "here wtf???%s\n" path;
    let oc = open_out path in 
    Printf.fprintf oc "%s" blob; 
    close_out oc


let checkout sha1 = 
    let bitepath = find_repo "." in 
    let acc = opendir bitepath in 
    try while true do 
        let fichier = readdir acc in 
        if (not(Sys.is_directory fichier) && (not(String.equal fichier "mygit.exe"))) then (unlink (bitepath^"/"^fichier))
        done
    with
        |End_of_file -> (
    let head, obj = read_object "." sha1 in 
    let types, size = read_header head in

    Printf.printf "Commencing checking of %s\n" sha1;
    assert (String.equal types "commit");
    let lst_assoc = parse_commit obj in 
    
    List.iter (fun (a,b) -> if (String.equal a "tree") then treat_obj "." b else ()) lst_assoc
        )



(* Faire une fonction qui ramasse ce qu'on ignore ou non dans un .biteignore *)
let to_ignore = ["."; ".."; ".bit"; "_ILP_tree"; "mygit.exe"] 

let ignore_file (f : string) : bool = List.mem f to_ignore

let rec bite_commit (message : string) (author : string) (commitor : string) =
    let path = find_repo "." in 
    let acc = opendir path in 
    let tree_ici = open_out (path^"/"^"_ILP_tree") in 
    try while true do 
        let file = readdir acc in 
        Printf.printf "TREATING %s\n" file;
        if not (ignore_file file) then
          (Printf.printf "did pass\n";
           let sah = (if Sys.is_directory (path^"/"^file)
                      then "0000"^space^(treat_dir (path^"/"^file))^nul^(file) 
                      else "0000"^space^(treat_blob (path^"/"^file))^nul^(file))
           in Printf.fprintf tree_ici "%s\n" sah; Printf.printf "sah = %s\n" sah
          )
        done 
    with 
    |End_of_file ->
      (close_out tree_ici;
       let path_ilptree = (path^"/"^"_ILP_tree") in 
       let tree_txt = read_whole_file path_ilptree in
       let path_stree =
         ("tree"^space^(string_of_int (String.length tree_txt))^nul) in 
       let sah_tree = comp_obj path tree_txt  path_stree in 
       unlink path_ilptree;
       let commited = [|("tree", sah_tree); ("Author: ", author);
                        ("Commitor: ", commitor);
                        ("message", message)|] in 
       let text_commit = write_commit (Array.to_list commited) in 
       let sha = comp_obj
                   path
                   text_commit
                   ("commit"^space^(string_of_int (String.length text_commit))^nul) in 
       log := sha::(!log); sha
      )
and treat_dir path = 
  let acc = opendir path in
  let path_ilptree = (path^"/"^"_ILP_tree") in 
  let tree_ici = open_out path_ilptree in 
  try while true do 
        let file = readdir acc in 
        Printf.printf "TREATING %s\n" file;
        if not (ignore_file file) then 
          (Printf.printf "did pass\n";
           let sah = (if Sys.is_directory (path^"/"^file)
                      then "0000"^space^(treat_dir (path^"/"^file))^nul^(file) 
                      else "0000"^space^(treat_blob (path^"/"^file))^nul^(file))
           in Printf.fprintf tree_ici "%s\n" sah; Printf.printf "sah = %s\n" sah
          )
      done 
  with 
  |End_of_file ->
    (close_out tree_ici;
     let tree_txt = read_whole_file path_ilptree in 
     let sah_tree =
       comp_obj
         path
         tree_txt
         ("tree"^space^(string_of_int (String.length tree_txt))^nul) in 
     unlink path_ilptree;
     sah_tree)

and treat_blob path = 
    let text = read_whole_file path in 
    comp_obj
      (toutsaufledernier (String.split_on_char '/' path) "/")
      text
      ("blob"^space^(string_of_int (String.length text))^nul)
 
let git_log = 
    let rec aux lst = match lst with 
      |[] -> ()
      |(sha)::q ->
        (Printf.printf "commit %s\n" sha;
         print_shit (parse_commit (snd (read_object "." sha)));
         Printf.printf "\n"; aux q)
    in aux (!log)  
