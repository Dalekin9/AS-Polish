
(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(*****************************************************************************)
(** Syntaxe abstraite Polish (types imposés, ne pas changer sauf extensions) *)

(** Position : numéro de ligne dans le fichier, débutant à 1 *)
type position = int

(** Nom de variable *)
type name = string

(** Opérateurs arithmétiques : + - * / % *)
type op = Add | Sub | Mul | Div | Mod

(** Expressions arithmétiques *)
type expr =
  | Num of int
  | Var of name
  | Op of op * expr * expr

(** Opérateurs de comparaisons *)
type comp =
| Eq (* = *)
| Ne (* Not equal, <> *)
| Lt (* Less than, < *)
| Le (* Less or equal, <= *)
| Gt (* Greater than, > *)
| Ge (* Greater or equal, >= *)

(** Condition : comparaison entre deux expressions *)
type cond = expr * comp * expr

(** Instructions *)
type instr =
  | Set of name * expr
  | Read of name
  | Print of expr
  | If of cond * block * block
  | While of cond * block
and block = (position * instr) list

(** Un programme Polish est un bloc d'instructions *)
type program = block


(***********************************************************************)


(* decouper le fichier en ligne de string *)
let rec convert_file_in_string_list (pos:position) (file:in_channel) =
  (pos, input_line file)::(convert_file_in_string_list (pos + 1) file)
(*
let file = open_in filename in
  let rec read_line lines file =
  try
    let line = input_line file in
      read_line (lines@[id_line_splitter (get_indent line)]) file
   with e ->
      if e = End_of_file then lines else failwith "unknown error" in
let lines = read_line [] file 
*)


(* decouper la liste de string du fichier en liste de liste de mots *)
let rec convert_string_list_in_string_list_list (string_list) (pos_string_list_list) =
  match string_list with 
  | [] -> pos_string_list_list
  | (x,y)::l -> 
    let chars = String.split_on_char ' ' y in 
    (*let chars_whithout_spaces = List.filter (fun x -> x != " ") chars in *)
    convert_string_list_in_string_list_list l ((x, chars)::pos_string_list_list)
    

(* determine si un string est un operateur*)
let name_is_op (var:name) =
  match var with 
  | "*" -> true
  | "+" -> true
  | "-" -> true 
  | "/" -> true
  | "%" -> true
  | _ -> false 


let name_is_comp (var:name) =
  match var with
  | "=" -> true  
  | "<>" -> true
  | "<" -> true
  | "<=" -> true
  | ">" -> true
  | ">=" -> true
  | _ -> false


(* separe une liste de mot en 2 liste de mots et retourne une liste de liste de mots*)
let separate_expr_into_2_expr (list:name list) (finale:name list list)=
match list with
| [] -> finale
| x::y::l -> 
  if (name_is_op x) then (*+....*)

    let rec sep_liste (right:int) (left:int) (liste:name list) (nv_list:name list) (finale:name list list)= 
      if right == 0 && left == 0  then
        nv_list::liste::finale
      else 
        match liste with
        | [] -> nv_list::liste::finale
        | x::l -> 
          if name_is_op x then
            if right == 0 then
              sep_liste 1 left l (x::nv_list) finale
            else 
              sep_liste right (left + 1) l (x::nv_list) finale
          else
            if right == 0 then
              sep_liste 0 (left-1) l (x::nv_list) finale
            else 
              sep_liste (right-1) left l (x::nv_list) finale

    in sep_liste 1 1 (y::l) [] []
    (*[x]::[y]*)
  else (*3...*)
    if (name_is_op y) then (*3+...*)
      [x]::(y::l)::finale
    else 
      if (l != []) then
        (*(x::y)::l::finale*)
        failwith "separate error"
      else 
        [x]::[y]::finale
| _ -> failwith "separate error fin"
    
(* transforme une liste de mots en expression*)
let rec convert_expr (list:name list) =
match list with 
| "+"::l -> 
  let ll = List.filter (fun x -> x != " ") l in
  let db_list = separate_expr_into_2_expr ll [] in
  let exp1 = List.nth db_list 0 in
  let exp2 = List.nth db_list 1 in 
  Op(Add, convert_expr exp1, convert_expr exp2)
| "-"::l -> 
  let db_list = separate_expr_into_2_expr l [] in
  let exp1 = List.nth db_list 0 in
  let exp2 = List.nth db_list 1 in 
  Op(Sub, convert_expr exp1, convert_expr exp2)
| "*"::l ->
  let db_list = separate_expr_into_2_expr l [] in
  let exp1 = List.nth db_list 0 in
  let exp2 = List.nth db_list 1 in 
  Op(Mul, convert_expr exp1, convert_expr exp2)
| "/"::l ->
  let db_list = separate_expr_into_2_expr l [] in
  let exp1 = List.nth db_list 0 in
  let exp2 = List.nth db_list 1 in 
  Op(Div, convert_expr exp1, convert_expr exp2)
| "%"::l ->
  let db_list = separate_expr_into_2_expr l [] in
  let exp1 = List.nth db_list 0 in
  let exp2 = List.nth db_list 1 in 
  Op(Mod, convert_expr exp1, convert_expr exp2)
| _ -> failwith "convert_expr error"

(*transforme une liste de liste de mot*pos en liste d'expression*pos*)
let rec create_block (liste: (position*name list) list) (block)=
  match liste with
  | [] -> block
  | (p,l)::ll ->
    create_block ll ( (p , convert_expr l)::block)


let rec separate_cond (list:name list) (ope:name) (f_exp) (l_exp) = 
  match list with
  | [] -> (ope, f_exp, l_exp)
  | x::l ->
    if name_is_comp x then
      (x, f_exp, l_exp)
    else
      separate_cond l ope (x::f_exp) l_exp

(*create condition*)
let create_condition (list:name list) = 
  let liste = separate_cond list "" [] [] in
  match liste with
  | (x,y,z) ->
    match x with
    | "=" -> (convert_expr y, Eq , convert_expr z)
    | "<>" -> (convert_expr y, Ne , convert_expr z)
    | "<" -> (convert_expr y, Lt , convert_expr z)
    | "<=" -> (convert_expr y, Le , convert_expr z)
    | ">" -> (convert_expr y, Gt , convert_expr z)
    | ">=" -> (convert_expr y, Ge , convert_expr z)
    | _ -> failwith "error in create condition"


(* convertit une ligne READ*)
let rec convert_read (list:string list) =
  match list with 
  | [] -> failwith "No var for Read line"
  | " "::l -> convert_read l
  | x::_ -> Read(x)


(* convertit une ligne PRINT*)
let rec convert_print (list:string list) =
  match list with 
  | [] -> failwith "No var for Print line"
  | ""::l -> convert_print l
  | " "::l -> convert_print l
  | x::l -> 
    let exp = convert_expr (x::l) in 
    Print(exp)
  
(* convertit une ligne IF*)
let rec convert_if (list:string list) = 
  failwith "a"


let rec get_profondeur (list:name list) (prof) =
  match list with
  | [] -> prof
  | " "::ll -> get_profondeur ll (prof + 1)
  | _ -> prof

let rec constr_list (prem_list:(position * name list) list)(nv_liste:(position *name list) list) (prof) = 
  match prem_list with 
  | [] -> (nv_liste,prem_list)
  | (a,b)::c ->  
    if (get_profondeur b 0 == prof) then
        (nv_liste,prem_list)
    else
      constr_list (c) ((a,b)::nv_liste) prof  

let get_name_list (l) =
  match l with
  | (x,y) -> x

let get_big_list (l) =
  match l with
  | (x,y) -> y


(* convertit une liste de (pos, list) list en (pos, instr ) list*)
let rec convert_list_in_ocaml (pos_string_list_list:(position * name list) list) (pos_ocaml_list_list:block)=
  match pos_string_list_list with 
  | [] -> pos_ocaml_list_list
  | (x,y)::l -> 
    (*let list_ocaml = convert_line_in_ocaml y  in 
    convert_list_in_ocaml l ((x , list_ocaml)::pos_ocaml_list_list)*)

    let rec parcours_y (y:name list) = 
      match y with
      | [] -> failwith "Empty line"
      | ""::l -> parcours_y l
      | " "::l -> parcours_y l
      | "READ"::ll -> 
        let e = convert_read ll in
        convert_list_in_ocaml l ( (x, e)::pos_ocaml_list_list)
      | "PRINT"::ll-> 
        let e = convert_print ll in 
        convert_list_in_ocaml l ( (x, e)::pos_ocaml_list_list)
      | "IF"::ll ->
        let prof = get_profondeur y 0 in
        let db_list = constr_list ( (x,y)::l) [] prof in
        let list_now = get_big_list db_list in
        let first_block = get_name_list db_list in 
        match list_now with
        | (a,b)::c -> 
          match b with
          | "ELSE"::lll -> 
            let db_list2 = constr_list ( c ) [] prof in
            let list_now = get_big_list db_list in
            let second_block = get_name_list db_list in 
            let e = If(create_condition y, (convert_list_in_ocaml first_block []), (convert_list_in_ocaml second_block [])) in 
            convert_list_in_ocaml l ( (x, e)::pos_ocaml_list_list)
          | _ ->
            let e = If(create_condition y, (convert_list_in_ocaml first_block []), []) in 
            convert_list_in_ocaml l ( (x, e)::pos_ocaml_list_list)
      | "WHILE"::ll ->
        let prof = get_profondeur y 0 in 
        let db_list = constr_list ( (x,y)::l) [] prof in
        let list_now = get_big_list db_list in
        let first_block = get_name_list db_list in 
        let e = While(create_condition y, (convert_list_in_ocaml first_block [])) in 
        convert_list_in_ocaml l ( (x, e)::pos_ocaml_list_list)
      | _-> failwith "error in parcours_y" 

    in parcours_y y


let read_polish (filename:string) =
(*
decouper le fichier en ligne de string
pour chaque ligne de string, la decouper en liste de string/char
pour chaque liste de string/char, convertir en code ocaml
*)

  let contenu = convert_file_in_string_list 0 (open_in filename) in 
  let list_list_mot = convert_string_list_in_string_list_list contenu [] in
  convert_list_in_ocaml list_list_mot



(***************************************************
****************************************************
*****************PRINT POLISH***********************
****************************************************
****************************************************)

(*Cherche le block d'insctruction la position pos*)
let rec search_block (pos:position) (b:block) =
  match b with
  | [] -> failwith "NO"
  | (x,y)::l ->
    if (x = pos) then
      y
    else
      search_block pos l


(*Retourne le nombre total d'instruction du programme*)
let rec max_instr (b:block) (max:int)= 
  match b with
  | [] -> max
  | (x,y)::l ->
    if (x > max) then
      max_instr b x
    else
      max_instr b max


(*Retourne la position de la plus petite instruction du programme*)
let rec min_instr (b:block) (min:int)= 
  match b with
  | [] -> min
  | (x,y)::l ->
    if (x < min) then
      min_instr b x
    else
      min_instr b min

let print_op (op:op) =
  match op with 
  | Add -> print_string " + "
  | Sub -> print_string " - "
  | Mul -> print_string " * "
  | Div -> print_string " / "
  | Mod -> print_string " % "


let rec print_expr (exp:expr) =
  match exp with 
  | Num (x) -> print_int x;
  | Var (x) -> print_string x;
  | Op  (op, exp1, exp2) ->
    print_op op;
    print_expr (exp1);
    print_expr exp2


let print_comp (comp:comp) =
  match comp with
  | Eq -> print_string " = "
  | Ne -> print_string " <> "
  | Lt -> print_string " < "
  | Le -> print_string " <= "
  | Gt -> print_string " > " 
  | Ge -> print_string " >= "


let print_cond (cond:cond) =
  match cond with 
  | (x,y,z) ->
    print_comp y;
    print_expr x;
    print_expr z


let print_indentation (indent:int) =
  for i = 0 to indent do
    print_string "  "
  done

let rec print_block (block:block) (indent:int)= 

  let min_instr = min_instr block 0 in
  let max_instr = max_instr block 0 in
  for i = min_instr to max_instr do 
    print_indentation indent;
    let inst = search_block i block in
    
    match inst with
    | Set(x,y) -> 
      print_string x;
      print_string " := ";
      print_expr y;
      print_string "\n"
    | Read(x) -> 
      print_string "READ ";
      print_string x;
      print_string "\n"
    | Print(x) ->
      print_string "PRINT ";
      print_expr x;
      print_string "\n"
    | If(c,b1,b2) ->
      print_string "IF ";
      print_cond c;
      print_string "\n";
      print_block b1 (indent + 1);
      print_string "ELSE ";
      print_string "\n";
      print_block b2 (indent + 1);
      print_string "\n"
    | While(c,b) ->
      print_string "WHILE ";
      print_cond c;
      print_string "\n";
      print_block b (indent + 1);
      print_string "\n"

  done


(*fct aux pr recherche de la 1er (et suite) instruction
pr chaque instruction
print l'expression en fonction de ce qu'elle contient
*)
let print_polish (p:(position*instr)list) =   
  print_block p 0

let eval_polish (p:program) : unit = failwith "TODO"

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> print_polish ( read_polish file )
  | [|_;"--eval";file|] -> eval_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
