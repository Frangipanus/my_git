(** Module Hashtbl (dictionnaires) :
        https://ocaml.org/api/Hashtbl.html
*)
(** Module String (chaînes de caractères) :
        https://ocaml.org/api/String.html
*)

(** Fonctions d'affichage **)

(* Pour afficher le contenu des tables d'associations *)
let print_values c i =
  print_char c;
  print_string " -> ";
  print_int i;
  print_newline ()


let print_huffman c s =
  print_char c;
  print_string " -> ";
  print_string s;
  print_newline ()


let display print_func hash_tbl =
  print_newline ();
  print_string "----------";
  print_newline ();
  Hashtbl.iter print_func hash_tbl;
  print_string "----------"


(** Implémentation d'une file de priorité **)

type huffman =
  | F of char 
  | N of huffman * huffman


(* File de priorité par tas-min *)
type 'a tas = {mutable free_idx : int; tbl : 'a array}
type couple = {weight : int; tree : huffman}

let empty_couple = {weight = -1; tree = F ' '}

(* ************************************************** *)
(* File de priorité à partir d'un tas-min *)
let swap a i j =
  let x = a.(i) in a.(i) <- a.(j) ; a.(j) <- x


(* Constructeur d'un tas *)
let create_heap size =
  {free_idx = 0; tbl = Array.make size empty_couple}

let is_empty h = h.free_idx = 0
let is_full h = h.free_idx = (Array.length h.tbl)

(* Remontée de l'élément d'indice k pour reconstituer la structure de
   tas *)
let rec sift_up h = function
  | 0 -> ()
  | j ->
     let i = (j - 1) / 2 in
     if h.tbl.(i).weight > h.tbl.(j).weight then begin
       swap h.tbl i j;
       sift_up h i
       end


(* Descente d'un élément d'indice k *)
let rec sift_down h = function
  | i when 2 * i + 1 >= h.free_idx - 1 -> ()
  | i ->
     let j = if (2 * i + 2 = h.free_idx - 1) || (h.tbl.(2 * i + 1).weight < h.tbl.(2 * i + 2).weight) then
               2 * i + 1
             else
               2 * i + 2
     in
     if h.tbl.(i).weight >= h.tbl.(j).weight then begin
         swap h.tbl i j;
         sift_down h j
       end


(* Insertion aux feuilles d'une valeur dans un tas puis reconstitution
   du tas *)
let insert h value =
  if is_full h then
    failwith "insertion impossible -> tas plein"
  else
    let i = ref h.free_idx in
    begin
      h.free_idx <- !i + 1;
      h.tbl.(!i) <- value;
      sift_up h !i
    end


(* Supression du minimum par permutation avec le dernier élément puis
   sift_down du nouveau premier élément *)
let take h =
  let n = h.free_idx in
  let mini = h.tbl.(0) in
  h.tbl.(0) <- h.tbl.(n-1);
  h.tbl.(n-1) <- empty_couple;
  sift_down h 0;
  h.free_idx <- (h.free_idx-1);
  mini


(** Fonctions à implémenter **)

(* Question 1 : Dictionnaire des nombres d'occurrences *)
let nb_occ s =
  let table = Hashtbl.create 1 in 
  for i = 0 to (String.length s) -1 do 
    try 
      let x = Hashtbl.find table s.[i] in 
      Hashtbl.replace table s.[i] (x+1)
    with
      Not_found -> Hashtbl.add table s.[i] 1
  done;
  table
 


let list_to_string l = 
  String.concat "" l



(* Question 2 : Arbre de Huffman *)
let make_huffman_tree s =
  let table = nb_occ s in
  let tas = create_heap (Hashtbl.length table) in 
  let x = ref empty_couple in
  Hashtbl.iter (fun c i -> insert tas {weight = i; tree = F c}) table;
  while not(is_empty tas) do 
    x := take tas;
    if not(is_empty tas) then begin
      let y = take tas in 
      let z = {weight = !x.weight + y.weight; tree = N (!x.tree, y.tree)} in 
      insert tas z
    end
  done;
  !x.tree
  
  




(* Question 3 : Dictionnaire des codes de Huffman *)
let make_dic_huffman arbre =
  let table = Hashtbl.create 1 in
  let rec aux arbre code = 
    match arbre with 
    | F c -> Hashtbl.add table c code
    | N (g, d) -> aux g (code ^ "0"); aux d (code ^ "1")
  in
  aux arbre "";
  table
  
(* Construction des tables avec ordre inversé pour les couples *)
let make_dic_huffman_rev arbre =
  let table = Hashtbl.create 1 in
  let rec aux arbre code = 
    match arbre with 
    | F c -> Hashtbl.add table code (String.make 1 c)
    | N (g, d) -> aux g (code ^ "0"); aux d (code ^ "1")
  in 
  aux arbre "";
  table

(* Question 4 : Encodage *)
let huffman_encode s arbre =
 let table = make_dic_huffman arbre in
 let res = ref [] in
 for i = 0 to (String.length s) -1 do 
   res := Hashtbl.find table s.[i] :: !res 
 done;
 list_to_string (List.rev !res)
  


(* Question 5 : Décodage *)


let huffman_decode s_code table nb_caracteres =
  (*La table est a l'envers*)
 let res = ref [] in
 let ind = ref 0 in 
 let nb_lettre = ref 0 in 
 while !ind < (String.length s_code) && !nb_lettre < nb_caracteres   do 
  let code = ref "" in
  while !ind < (String.length s_code) && not(Hashtbl.mem table !code) do 
    code := !code ^ (String.make 1 s_code.[!ind]);
    ind := !ind + 1
  done;
  if (Hashtbl.mem table !code) then  begin
  res := (Hashtbl.find table !code)::(!res); (*PB*)
  nb_lettre := !nb_lettre + 1
  end
  done;
  list_to_string (List.rev !res)
   


(* Question 6*)

let pow x n = 
  let res = ref 1 in
  for i = 1 to n do 
    res := !res * x;
  done;
  !res

let binary_to_int (s : string): int =
  let res = ref 0 in
  for i = 0 to (String.length s) -1 do 
    
    res := !res + (int_of_string (String.make 1 s.[i])) * (pow 2 (String.length s - i - 1));
    
  done;
  !res

let int_to_binary n = 
  let n = ref n in
  let res = ref "" in
  while !n > 0 do 
    res := (string_of_int (!n mod 2)) ^ !res;
    n := !n / 2
  done;
  while (String.length !res) mod 8 <> 0 do 
    res := "0"^ !res
  done;
  !res

    


let bits_vers_string s_code =
  let s_code = ref s_code in
  s_code := !s_code^(String.make (8 -((String.length !s_code) mod 8)) '0'); (*PB?*)
  let res = ref [] in
  let i = ref 0 in 
  while !i < (String.length !s_code) do 
    let s = String.sub !s_code !i 8 in
    let c = Char.chr (binary_to_int s) in 
    res := (String.make 1 c)::(!res); (*PB*)
    i:= !i + 8
  done;
  list_to_string(List.rev !res)



(* Question 7*)
let string_vers_bits (chaine: string) =
	let res = ref [] in
	for i = 0 to String.length chaine - 1 do
		let code = Char.code chaine.[i] in
		let acc = ref "" in
		let rec convert (n: int) (i: int) =
			if n = 0 && i = 0 then
				()
			else begin
				convert (n / 2) (i - 1);
				acc := !acc ^ (string_of_int (n mod 2))
			end
		in
		convert code 8;
		res := !acc :: !res;
	done;
	String.concat "" (List.rev !res)

(* Question 8*)

let nb_line filename = 
  let ic = open_in filename in
  let nb = ref 0 in
  try
    while true do 
      ignore(input_line ic);
      nb := !nb + 1
    done;
    !nb
  with
    End_of_file -> close_in ic; !nb

let readfile filename =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let compresser filename filename_out = 
    let file = filename_out in 
    Printf.printf "   [+] On compresse: %s\n" filename;
    let str = readfile filename in
    let h_tree = make_huffman_tree str in
    let dic_huffman = make_dic_huffman h_tree in
    let s_code = huffman_encode str h_tree in
    let s_code_2 = bits_vers_string s_code in
    let oc = open_out file  in
    let nb_distinc = Hashtbl.length dic_huffman in
    let nb_char = String.length str in
    Printf.fprintf oc "%d %d\n" nb_char nb_distinc;
    Hashtbl.iter (fun cha code -> Printf.fprintf oc "%c %s\n" cha code) dic_huffman;
    Printf.fprintf oc "%s"  s_code_2;
    Printf.printf "   [+] Success\n";
    Printf.printf "   [+] Fichier Compressé: %s\n" file;
    close_out oc
    
  let decompresser filename filename_out = 
    let file = filename_out in 
    Printf.printf "   [+] On décompresse: %s\n" filename;
    let str = readfile filename in
    let ind = ref 0 in 
    let acc = ref "" in
    let nb_char = ref 0 in
    while str.[!ind] <> '\n' do 
      
      if str.[!ind] = ' ' then begin
        nb_char := int_of_string !acc;
        acc := ""
        end
      else
        acc := !acc ^ (String.make 1 str.[!ind]); (*PB*)
      incr ind
    done;
    incr ind;
    let nb_diff = int_of_string !acc in 
    let table = Hashtbl.create nb_diff in 
    for i = 0 to nb_diff - 1 do
      
      let letter = (String.make 1 str.[!ind]) in 
      incr ind;
      incr ind;
      let code = ref "" in 
      while str.[!ind]<> '\n' do
        code := !code ^ (String.make 1 str.[!ind]); (*PB*)
        incr ind
      done;
      incr ind;
      Hashtbl.add table !code letter
    done;
    let to_decode = String.sub str (!ind) (String.length str - !ind) in 
    
    let bits = string_vers_bits to_decode in 
    
    let message = huffman_decode bits table !nb_char in 
    
    let oc = open_out file  in
    
    Printf.fprintf oc "%s"  message;
    close_out oc;
    Printf.printf "   [+] Success\n";
    Printf.printf "   [+] Fichier Décompressé: %s\n" file
  

(* ************************************************** *)
(* let s = "scienceinformatique" *)
(*
let s = "abracadabra"


let dic_nb_occ = nb_occ s
let () = display print_values dic_nb_occ

let h_tree =  make_huffman_tree s

let dic_huffman = make_dic_huffman h_tree
let () = display print_huffman dic_huffman

let s_code = huffman_encode s h_tree
let () = Printf.printf "Encodage : %s\n" (s_code)
let table = make_dic_huffman_rev h_tree 
let () = Printf.printf "Décodage : %s\n" (huffman_decode s_code table (String.length s) )

let () = Printf.printf "Encodage en caractères : %s\n" (bits_vers_string s_code)
let () = Printf.printf "Décodage des caractères : %s\n" (string_vers_bits (bits_vers_string s_code))
let test = string_vers_bits (bits_vers_string s_code)
let () = Printf.printf "Décodage : %s\n" (huffman_decode test table (String.length s) )*)
