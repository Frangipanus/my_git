open Unix


(****************Fonctions utilitaires sur les texte et repo***********************************)
exception Not_A_Repo

let decode_hex_string hex_string =
    let byte_string = Hex.to_cstruct (`Hex hex_string) in
    let decoded_string = Cstruct.to_string byte_string in
    decoded_string


let nul = decode_hex_string "00"
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
    Printf.printf "%s %s\n" source dest;
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


(**************************Object managing**********************************)
let log = ref [] (*Commit History.*)

(*prend le sha et le bite_path et revoie un objet en string avec le header
Raise Not_An_Object si l'objet avec ce sha n'existe pas*)

exception Not_An_Object

let decomp_obj bite_path sha = 
    let path = bite_path^"/.bite/objects/"^(String.make 1 sha.[0])^(String.make 1 sha.[1]) in 
    Printf.printf "hash= %s" sha;
    Printf.printf "%s = path\n" path; 
    if Sys.file_exists path then 
        (if Sys.file_exists (path^"/"^(String.sub sha 2 (String.length sha -2))) then (
            let file_path = (path^"/"^(String.sub sha 2 (String.length sha -2))) in 
            (decompress_file file_path (path^"/_ILP"));
            let acc = read_whole_file (path^"/_ILP")  in acc(*La on a l'objet decompresser*))
        else
            raise Not_An_Object)
    else 
        raise Not_An_Object
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
        compress_file (obj_path^"/_ILP") (path^"/"^(String.sub hashed 2 (String.length hashed -2))));
        hashed
    else
        (mkdir path 0o777;compress_file obj_path (path^"/"^(String.sub hashed 2 (String.length hashed -2))));
        hashed
    with 
        |Not_A_Repo ->Printf.printf "%s\n" "Path is not in a bite repo"
        
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

let toutsauflepremier lst = 
    let rec aux  lst1 res = 
        match lst1 with 
        |[]-> res
        |h ::q -> aux  q (res^h)
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
            if h.[0] != ' ' then (let acc = (String.split_on_char ' ' h) in let acc2 = toutsauflepremier acc in 
                                    aux q (res@[(a,b)]) (List.hd acc, acc2))
                                else
                                    (aux q res (a,b^(String.sub h 1 (String.length h -1)))))
    in
    let (key_value , message) = aux lines [] ("", "") in 
    let message_vrai = toutsauflepremier (("ratio")::(message)) in 
    List.tl(key_value@["message", message_vrai])

let write_commit commit = 
    let rec aux lst acc = 
        match lst with 
        | (a,b)::q::t -> aux (q::t) (acc^a^" "^b^"\n")
        |(_,b)::[] -> (acc ^"\n"^b)
        |[] -> failwith "not a commit"
    in aux commit ""

let rec print_shit lst = 
    match lst with 
    | (a,b)::q  when a = "auhor"-> (Printf.printf "Author: %s\n" b; print_shit q)
    | ("commbiter", b)::q -> (Printf.printf "Commitor: %s\n" b; print_shit q)
    | ("message", b)::q -> (Printf.printf "%s\n" b; print_shit q)
    |_ -> ()
 
let git_log = 
    let rec aux lst = match lst with 
        |[] -> ()
        |(sha, comm)::q -> (Printf.printf "commit %s\n" sha; print_shit comm; Printf.printf "\n"; aux q)
    in
    aux (!log)  

let read_object path sha = (*Renvoie le header et l'object*)
    let bite_path = find_repo path in 
    let object_with_header = decomp_obj bite_path sha in 
    let lst = String.split_on_char (nul.[0]) object_with_header in 
    (List.hd lst,toutsauflepremier lst )

let read_header head = (*Head n'a pas le null a la fin*)
    let lst1 = String.split_on_char (space.[0]) head in 
    let types = List.hd lst1 in 
    let size = toutsauflepremier lst1 in 
    (types, size)

let rec treat_obj path sha = (*Path est l'endroit ou c'est mis. Ca peut etre un file ou un dossier*)
    let head, obj = read_object path sha in 
    let types, size = read_header head in
    match types with 
    |"blob" -> treat_blob obj path
    |"tree" -> treat_tree obj path 
    |_ -> failwith "not yet"


and treat_tree tree path  = (*tree est l'arbres*)
    let content = String.split_on_char '\n' tree in (*Une liste des triplet de la forme (autorisation, sha, path)*)
    List.iter (fun x -> let lst = String.split_on_char (space.[0]) x in 
              let auto = List.hd lst in  
              let acc = toutsauflepremier lst in 
              let lst2 = String.split_on_char (nul.[0]) acc in 
              let sha = List.hd lst2 in 
              let path2 = path^"/"^(toutsauflepremier lst2) in 
              treat_obj path sha) content

and treat_blob blob path= 
    let oc = open_out path in 
    Printf.fprintf oc "%s" blob; 
    close_out oc

let checkout sha1 = 
    let head, obj = read_object "." sha1 in 
    let types, size = read_header head in
    assert (String.equal types "commit");
    let lst_assoc = parse_commit obj in 
    List.iter (fun (a,b) -> if (String.equal a "tree") then treat_obj "." sha1 else ()) lst_assoc

    
    
let rec bite_commit () =
    let path = find_repo "." in 
    let acc = opendir path in 
    try while true do 
        let file = readdir acc in 
        if ((String.equal file ".") || (String.equal file "..") ||(String.equal file ".bite")) then () 
        else (
            if Sys.is_directory (path^file) then treat_dir (path^file) else treat_blob (path^file)
        )
    done 
    with 
    |End_of_file -> ()

and treat_dir path = ()

and treat_blob path = ()

    
