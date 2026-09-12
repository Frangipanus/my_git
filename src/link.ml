open Utils

let link (serv : string) : unit =
  let bitepath = find_repo "." in
  let oc = open_out (bitepath^"/.bite/"^"config") in
  Printf.fprintf oc  "distant_server:'%s'" serv;
  close_out oc
