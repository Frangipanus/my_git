open Structure 
open Unix

exception Found
let has_git path = 
  let dossier = Unix.opendir path in 
  let dones = ref true in
  try while true do 
    try let acc = Unix.readdir dossier in 
      if acc = ".git" then raise Found
    with 
      
      |End_of_file -> true
  done;
    false
  with
    | |Found -> false

let init path = 
  
