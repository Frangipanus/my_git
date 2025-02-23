let sha1_hash input =
    let hash = Digestif.SHA1.digest_string input in
    Digestif.SHA1.to_hex hash





let buffer_size = 4096
let compress_file source dest =
  let gz_file = Gzip.open_out ~level:9 dest in
  let buffer = Bytes.make buffer_size '*' in
  let ic = open_in_bin source in
  let rec aux () =
    let len = input ic buffer 0 buffer_size in
    if len <> 0 then
      begin
        Gzip.output gz_file buffer 0 len;
        aux ()
      end
  in
  aux ();
  Gzip.close_out gz_file;
  close_in ic

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

let read_whole_file filename =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s


(*prend le sha et le bite_path et revoie un objet en string avec le header
Raise Not_An_Object si l'objet avec ce sha n'existe pas*)
exception Not_An_Object
let decomp_obj bite_path sha = 
    let path = bite_path^"/.bite/objects/"^sha[0]^sha[1] in 
    if Sys.file_exists path then 
        (if Sys.file_exists path^(String.sub sha 2 (String.length sha)) then 
            (decompress_file (path^(String.sub sha 2 (String.length sha)) (path^"/_ILP"));
            let acc = read_whole_file (path^"/_ILP") (*La on a l'objet decompresser*))
        else
            raise Not_An_Object)
    else 
        raise Not_An_Object

