
open Unix


(****************Fonctions utilitaires sur les texte et repo***********************************)
let rec rmrf path = match Sys.is_directory path with
  | true ->
    Sys.readdir path |>
    Array.iter (fun name -> rmrf (Filename.concat path name));
    Unix.rmdir path
  | false -> Sys.remove path


exception Not_A_Repo

let decode_hex_string hex_string =
    let byte_string = Hex.to_cstruct (`Hex hex_string) in
    let decoded_string = Cstruct.to_string byte_string in
    decoded_string


let nul = decode_hex_string "01"
let space = decode_hex_string "20"
let sha1_hash input =
    let hash = Digestif.SHA1.digest_string input in
    Digestif.SHA1.to_hex hash


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

let read_whole_file filename =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let read_lines (file_name : string) : string list =
    In_channel.with_open_text file_name In_channel.input_lines




let buffer_size = 4096
let compress_file source dest =
    let gz_file = Gzip.open_out ~level:9 dest in
    let buffer = Bytes.make buffer_size '*' in
    let ic = In_channel.open_bin source in
    let rec aux () =
      let len = In_channel.input ic buffer 0 buffer_size in
      if len <> 0 then
        begin
          Gzip.output gz_file buffer 0 len;
          aux ()
        end
    in
    aux ();
    Gzip.close_out gz_file;
    In_channel.close ic
let decompress_file source dest =
  let gz_file = Gzip.open_in source in
  let buffer = Bytes.make buffer_size '*' in
  let oc = open_out_bin dest in
  let rec aux () =
    let len = Gzip.input gz_file buffer 0 buffer_size in
    if len <> 0 then
      begin
        output oc buffer 0 len;
        aux ()
      end
  in
  aux ();
 

  Gzip.close_in gz_file;
  close_out oc

  let toutsaufledernier lst sep= 
    let rec aux lst1 res = 

        match lst1 with 
            |[h] ->  res 
            |[] -> res 
            |h::q::[] ->res^h
            |h::t::q -> aux (t::q) (res^h^sep)
            |_ -> failwith "ta mere"
            
    in aux lst ""

(**************************Object managing**********************************)

(*prend le sha et le bite_path et revoie un objet en string avec le header
Raise Not_An_Object si l'objet avec ce sha n'existe pas*)

exception Not_An_Object
exception Not_An_Object2

let decomp_obj bite_path sha = 
    let path = bite_path^"/.bite/objects/"^(String.make 1 sha.[0])^(String.make 1 sha.[1]) in 

     
    if Sys.file_exists path then 
        (   
            
            if Sys.file_exists (path^"/"^(String.sub sha 2 (String.length sha -2))) then (
            let file_path = (path^"/"^(String.sub sha 2 (String.length sha -2))) in 
            (decompress_file file_path (path^"/_ILP"));
            let acc = read_whole_file (path^"/_ILP")  in unlink (path^"/_ILP")  ;acc(*La on a l'objet decompresser*))
        else
            raise Not_An_Object)
    else 
        raise Not_An_Object2
(*renvoie le sha1 de l'object*)
let comp_obj (obj_path : string) (obj : string) (header : string) =(*obj_path est le DOSSIER ou est l'objet, fauda mettre la taille mais ca c'est pour plus tard tkt marius ca sera rapide*)
    let file = header^obj in 
    let hashed = sha1_hash file in 

    try let accu = (find_repo obj_path) in let path =accu^"/.bite/objects/"^(String.make 1 hashed.[0])^(String.make 1 hashed.[1]) in
    if Sys.file_exists path then (
        let oc = open_out (obj_path^"/_ILP") in
        (* create or truncate file, return channel *)
        Printf.fprintf oc "%s" file;
        (* write something *)
        close_out oc;
        compress_file (obj_path^"/_ILP") (path^"/"^(String.sub hashed 2 (String.length hashed -2)));
        unlink (obj_path^"/_ILP");
        hashed)
    else
        (mkdir path 0o755; let oc = open_out (obj_path^"/_ILP") in
        (* create or truncate file, return channel *)
        Printf.fprintf oc "%s" file;
        (* write something *)
        close_out oc;
        compress_file (obj_path^"/_ILP") (path^"/"^(String.sub hashed 2 (String.length hashed -2)));
        unlink (obj_path^"/_ILP");
        hashed)
    with 
        |Not_A_Repo ->failwith  "Path is not in a bite repo"
        
let cat_file types obj_ident = 
    let acc = decomp_obj (find_repo ".")  (obj_ident) in 
    let lst = String.split_on_char (space).[0] acc in 
    match types, lst with 
    |"blob", _::q::[] -> Printf.printf "%s\n" q; (*en vrai faut enelver le header je ferai ca quand je comprednrai si il est dans obj*)
    |_ -> failwith "pas implem"

(*Les deux versions de hash-object, c'est le parsing qui dit laquelle on appelle. le type par default est blob si non précisé*)
let hash_object_stdout types file = 
    let file_str = read_whole_file file in 
    let length = String.length file_str in 
    let acc = types^space^(string_of_int length)^nul^file_str in 
    let res = sha1_hash acc in 
    Printf.printf "%s\n" res

let hash_object_directory types file = 
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
    let head = types^space^(string_of_int length)^nul in
    let _ = comp_obj path file_str head  in ()

let toutsauflepremier lst sep= 
    let rec aux  lst1 res = (
        
        match lst1 with 
        |[]-> res
        |[h] -> res^h
        |h ::q -> aux  q (res^h^sep))
    in match lst with 
        |[] -> ""
        |[_] -> ""
        |_::q -> aux  q ""
 

let parse_commit (commit : string) : (string*string) list=  (*On prend en entrée un string décompresser et on renvoie un type commit*)
    let lines = String.split_on_char '\n' commit in 
    let rec aux line  (res : (string*string) list) (a,b)= 
        match line with 
        |[] -> res, []
        |h::q ->  if (String.length h) =0 then (res, line) else ( 
            if h.[0] != ' ' then (let acc = (String.split_on_char ' ' h) in let acc2 = toutsauflepremier acc "" in 
                                    aux q (res@[(a,b)]) (List.hd acc, acc2))
                                else
                                    (aux q res (a,b^(String.sub h 1 (String.length h -1)))))
    in
    let (key_value , message) = aux lines [] ("", "") in 
    let message_vrai = toutsauflepremier (("ratio")::(message)) "" in 
    List.tl(key_value@["message", message_vrai])


let get_branch ()= 
    let bitepath = find_repo "." in
    let branch = read_whole_file (bitepath^"/.bite/branch") in 
    branch
let commit_list branch =
  let bitepath = find_repo "." in 
  let commits = read_lines (bitepath^"/.bite/branches/"^branch^"/list") in 
  let rec aux lst res = 
    match lst with
    |[] -> res 
    |h1::h2::t -> aux t (h1::res)
  in 
  aux commits []
let write_commit commit = 
    let rec aux lst acc = 
        match lst with 
        | (a,b)::q::t -> aux (q::t) (acc^a^" "^b^"\n")
        |(_,b)::[] -> (acc ^"\n"^b)
        |[] -> failwith "not a commit"
    in aux commit ""

let rec print_shit lst = 
    match lst with 
    | (a,b)::q  when not(String.equal a "message")-> (Printf.printf "%s %s\n" a b; print_shit q)
    | ("message", b)::q -> (Printf.printf "%s\n" b; print_shit q)
    |_ -> ()

let read_object path sha = (*Renvoie le header et l'object*)
    let bite_path = find_repo path in 
    let object_with_header = decomp_obj bite_path sha in     
    let lst = String.split_on_char (nul.[0]) object_with_header in 

    (List.hd lst,toutsauflepremier lst nul )

let read_header head = (*Head n'a pas le null a la fin*)
    let lst1 = String.split_on_char (space.[0]) head in 
    let types = List.hd lst1 in 
    let size = toutsauflepremier lst1 nul in 
    (types, size)

let rec treat_obj path sha = (*Path est l'endroit ou c'est mis. Ca peut etre un file ou un dossier*)
    if (Sys.file_exists path && Sys.is_directory path) then (
            let head, obj = read_object path sha in 
            let types, size = read_header head in
            Printf.printf "type = %s\n" types;
            match types with 
            |"blob" -> failwith "blob is not a directoru"
            |"tree" -> treat_tree obj path 
            |_ -> failwith "not yet")
    else
        (
        let lst = String.split_on_char '/' path in 
        let head, obj = read_object (toutsaufledernier lst "/") sha in 
        let types, size = read_header head in
        match types with 
          | "blob" -> treat_blob obj path
          | "tree" -> treat_tree obj path 
          |_ -> failwith "not yet"
        )


and treat_tree tree path  = (*tree est l'arbres*) (*tree = mode shaNULpath *)
Printf.printf "tree is commencing";
    if ((not(C_init.has_bite path))) then ( Printf.printf "no problem2\n";if Sys.file_exists path then rmrf path; mkdir path (0o755)) else ();
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

let get_last_commit () = 
  let bitepath = find_repo "." in 
  read_whole_file (bitepath^"/.bite/HEAD")

let checkout sha1 = 
    let bitepath = find_repo "." in 
    let acc = opendir bitepath in 
    try while true do 
        let fichier = readdir acc in
        Printf.printf "%s\n" fichier;
        if (not(Sys.is_directory fichier) && (not(String.equal fichier "mygit.exe")) && (not(String.equal ".biteignore" fichier))) then (Printf.printf "remove 2\n";let accu = bitepath^"/"^fichier in Printf.printf "path is %s %s\n" bitepath accu ;unlink accu) else(
        if (Sys.is_directory fichier) && (not(String.equal ".biteignore" fichier)) && (not(String.equal ".bite" fichier)) &&(not(String.equal "." fichier) && (not(String.equal ".." fichier))) then ( Printf.printf "removed\n";rmrf (bitepath^"/"^fichier)))
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

    
let rec bite_commit message author commitor =
    let path = find_repo "." in 
    let acc = opendir path in 
    let tree_ici = open_out (path^"/"^"_ILP_tree") in 
    try while true do 
        let file = readdir acc in 
        Printf.printf "TREATING %s\n" file;
        if ((String.equal file ".") || (String.equal file "..") ||(String.equal file ".bite") || (String.equal file "_ILP_tree") ||(String.equal file "mygit.exe")) then () 
        else ( Printf.printf "did pass\n";
            let sah = (if Sys.is_directory (path^"/"^file)
                         then  "0000"^space^(treat_dir (path^"/"^file))^nul^(file) 
                         else   "0000"^space^(treat_blob (path^"/"^file))^nul^(file) )
            in Printf.fprintf tree_ici "%s\n" sah; Printf.printf "sah = %s\n" sah
        )
    done 
    with 
    |End_of_file -> (close_out tree_ici; let tree_txt = read_whole_file (path^"/"^"_ILP_tree") in 
                    let sah_tree = comp_obj path tree_txt ("tree"^space^(string_of_int (String.length tree_txt))^nul) in 
                    unlink (path^"/"^"_ILP_tree");
                    let commited = [|("tree", sah_tree); ("Author: ", author); ("Commitor: ", commitor); ("message", message)|] in 
                    let text_commit = write_commit (Array.to_list commited) in 
                    let sha = comp_obj path text_commit ("commit"^space^(string_of_int (String.length text_commit))^nul) in 
                    let branch = get_branch () in 
                    let already = commit_list branch in 
                    if (List.mem sha already) && (String.equal sha (get_last_commit ()) ) then (Printf.printf "Deja a jour.\n"; exit(0));
                    let oc = open_out (path^"/.bite/HEAD") in Printf.fprintf oc "%s" sha;
                    let branch_cur = get_branch () in 
                    let oc2 = open_out (path^"/.bite/branches/"^branch_cur^"/HEAD") in 
                    Printf.fprintf oc2 "%s" sha;
                    let str_acc = read_whole_file (path^"/.bite/branches/"^branch_cur^"/list") in 
                    let to_print = (if (String.length str_acc) >0 then sha^"\n"^message^"\n"^str_acc else sha^"\n"^message  )in 
                    let oc = open_out (path^"/.bite/branches/"^branch_cur^"/list") in 
                    Printf.fprintf oc "%s" to_print;
                    sha
                     )

and treat_dir path = 
    let acc = opendir path in 
    let tree_ici = open_out (path^"/"^"_ILP_tree") in 
    try while true do 
        let file = readdir acc in 
        Printf.printf "TREATING %s\n" file;
        if ((String.equal file ".") || (String.equal file "..") || (String.equal file "_ILP_tree") ||(String.equal file "mygit.exe")) then () 
        else ( Printf.printf "did pass\n";
            let sah = (if Sys.is_directory (path^"/"^file)
                         then  "0000"^space^(treat_dir (path^"/"^file))^nul^(file) 
                         else   "0000"^space^(treat_blob (path^"/"^file))^nul^(file) )
            in Printf.fprintf tree_ici "%s\n" sah; Printf.printf "sah = %s\n" sah
        )
    done 
    with 
    |End_of_file -> (close_out tree_ici; let tree_txt = read_whole_file (path^"/"^"_ILP_tree") in 
                    let sah_tree = comp_obj path tree_txt ("tree"^space^(string_of_int (String.length tree_txt))^nul) in 
                    unlink (path^"/"^"_ILP_tree");
                    sah_tree
                     )

and treat_blob path = 
    let text = read_whole_file path in 
    comp_obj (toutsaufledernier (String.split_on_char '/' path) "/") text ("blob"^space^(string_of_int (String.length text))^nul)
    

let branch_list path = 
  let bitepath = find_repo path in 
  let acc = opendir (bitepath^"/.bite/branches") in 
  try while true do 
    let fichier = readdir acc in 
    if (not(String.equal "." fichier) && not(String.equal ".." fichier)) then 
    Printf.printf "%s\n" fichier
  done 
  with
  |End_of_file -> () 

let get_branch_list path = 
  let bitepath = find_repo path in 
  let acc = opendir (bitepath^"/.bite/branches") in 
  let rec aux dir res = 
    try let fichier = readdir acc in 
    if (not(String.equal "." fichier) && not(String.equal ".." fichier)) then 
      aux dir (fichier::res)
  else
    aux dir res
    with 
    |End_of_file -> res 
  in 
  aux acc []


let git_log ()= 
    let branch = get_branch () in
    let path = find_repo "." in  
    let commits = read_lines (path^"/.bite/branches/"^branch^"/list") in 
    let rec aux lst = 
      match lst with 
      |[] -> ()
      |h1::h2::t -> (Printf.printf "Commit hash: %s\nCommit message:\n %s \n" h1 h2; aux t)
    in 
    aux commits 

let branch_create name = 
  let lst = get_branch_list "." in
  if (List.mem name lst) then (Printf.printf "%s is already a branch.\n" name)
  else (
    mkdir (".bite/branches/"^name) 0o770;
    let _ = openfile (".bite/branches/"^name^"/HEAD") [O_CREAT] 0o770 in
    let _ = openfile (".bite/branches/"^name^"/list") [O_CREAT] 0o770 in 
    Printf.printf "Branch %s was succesfully created.\n" name    
  )

let branch_checkout name = 
  let lst = get_branch_list "." in
  let bitepath = find_repo "." in  
  if (List.mem name lst) then (
    let oc = open_out (bitepath^"/.bite/branch") in 
    Printf.fprintf  oc "%s" name;
    Printf.printf "Switched to branch %s.\n" name
  )
  else
    (Printf.printf "Branch %s does not exist. You can create it with command branch_create.\n" name)



