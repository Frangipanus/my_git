(* à changer : pour stocker le type des configurations*) 
type conf = A 

type repo = { path : string; conf : conf}

type command =
  | Init of string 
  | Add of string list
  | Remove of string list
  | Cat of string * string
  | Commit of string 
