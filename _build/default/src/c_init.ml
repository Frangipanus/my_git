open Unix

exception Found
let has_git path = 
  let dossier = Unix.opendir path in 
  try while true do 
    try let acc = Unix.readdir dossier in 
      if acc = ".git" then raise Found
    with 
      
      |End_of_file -> raise Not_found
  done;
    false
  with
    |Found -> true
    |Not_found -> false

let init path = 
  if has_git path then 