open Unix

exception Found
let has_git path = 
  let dossier = Unix.opendir path in 
  try while true do 
    try let acc = Unix.readdir dossier in 
      if acc = ".bite" then raise Found
    with 
      
      |End_of_file -> raise Not_found
  done;
    false
  with
    |Found -> true
    |Not_found -> false

let init path = 
  if has_git path then Printf.printf "Echec: le répertoire a déja un dossier .git. Le supprimer et retenter.";
  Unix.mkdir ".bite" 0o777;
  Unix.mkdir ".bite/objects" 0o777;
  Unix.mkdir ".bite/refs" 0o777;
  let _ = Unix.openfile ".bite/HEAD" ([Unix.O_CREAT]) 0o777 in 
  let _ = Unix.openfile ".bite/config" ([Unix.O_CREAT]) 0o777 in 
  let _ = Unix.openfile ".bite/description" ([Unix.O_CREAT]) 0o777 in 
  ()



  