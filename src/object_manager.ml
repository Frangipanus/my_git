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

let test = sha1_hash "nigger"