open Unix


(****************Fonctions utilitaires sur les texte et repo***********************************)

let rec rmrf (path : string) : unit =
  if Sys.is_directory path then 
    (Sys.readdir path |>
       Array.iter (fun name -> rmrf (Filename.concat path name));
     Unix.rmdir path)
  else
    Sys.remove path

exception Not_A_Repo

let decode_hex_string (hex_string : string) : string =
  Cstruct.to_string @@ Hex.to_cstruct (`Hex hex_string)

let nul = decode_hex_string "01"
let space = decode_hex_string "20"

let sha1_hash (input : string) : string =
    Digestif.SHA1.to_hex @@ Digestif.SHA1.digest_string input

let equal_node n1 n2 =  (n1.st_dev = n2.st_dev) && (n1.st_ino = n2.st_ino)
  
let rec find_repo (path : string) : string = 
  if C_init.has_bite path then
    path
  else begin 
      let acc = path^"/.." in 
      let node1 = Unix.stat path in 
      let node2 = Unix.stat acc in
      if equal_node node1 node2 then
        (Printf.printf "%s\n" path; raise Not_A_Repo)
      else begin 
          find_repo acc
        end      
    end

let read_whole_file (filename : string) : string =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let read_lines (file_name : string) : string list =
    In_channel.with_open_text file_name In_channel.input_lines

let buffer_size = 4096
let compress_file (source : string) (dest : string) : unit =
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

let decompress_file (source : string) (dest : string) : unit =
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

let map_all_except_last  (f : 'a -> 'a) (l : 'a list) : 'a list = 
  let last_ind = (List.length l) - 1 in 
  List.mapi (fun i x -> if i <> last_ind then f x else x) l 

let all_except_last (l : 'a list) : 'a list =
  List.rev @@ List.tl @@ List.rev l
    
let toutsaufledernier (lst : string list) (sep : string) : string =
  if List.length lst = 0 then
    sep
  else
    (List.fold_left (^) "") @@
    (map_all_except_last (fun s -> s^sep)) @@
    all_except_last lst

let toutsauflepremier (lst : string list) (sep : string) : string =
  (List.fold_left (^) "") @@ (map_all_except_last (fun x -> x^sep)) @@ List.tl lst
