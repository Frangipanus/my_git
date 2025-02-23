(* à changer : pour stocker le type des configurations*) 
type conf = A 

type repo = { path : string; conf : conf}

type command = Init of string | Autre
