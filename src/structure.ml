(* à changer : pour stocker le type des configurations*) 
type conf = A 

type repo = { path : string; conf : conf}

type command =
  | Init of string 
  | Add of string list
  | Cat of string * string
  | Hash of string * string * bool (* type, fichier, true si on stocke dans le repo, false sinon *) 
  | Commit of string
  | Log 
  |Chekout of string
  |Branch_list
  | Branch_create of string 
  |Branch_checkout of string
  |Merge of string
  |Remove of string
