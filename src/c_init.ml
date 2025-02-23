
let has_bite (path : string) =
  let bitepath = path^".bite" in
  if Sys.file_exists bitepath then Sys.is_directory bitepath else false
  
let c_init (path : string) : unit = 
  chdir path; 
  if has_bite path then
    (Printf.eprintf "Echec: %s contient déja un dossier .bite" path; exit 1)
  else 
    mkdir ".bite" 0o770;
    mkdir ".bite/objects" 0o770;
    mkdir ".bite/refs" 0o770;
    let _ = openfile ".bite/HEAD" [O_CREAT] 0o770 in
    let _ = openfile ".bite/config" [O_CREAT] 0o770 in
    let _ = openfile ".bite/description" [O_CREAT] 0o770 in
    ()



  
