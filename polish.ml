
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





(***************************************************
****************************************************
*****************PRINT POLISH***********************
****************************************************
****************************************************)

(*Cherche le block d'insctruction la position pos*)
let rec search_block (pos:position) (b:(position * instr)list) =
  match b with
  | [] -> (*failwith "NO"*) None
  | (x,y)::l ->
    if x == pos then
      Some y
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
  | Add -> print_string "+"
  | Sub -> print_string "-"
  | Mul -> print_string "*"
  | Div -> print_string "/"
  | Mod -> print_string "%"


let rec print_expr (exp:expr) =
  match exp with 
  | Num (x) -> 
    print_string " ";
    print_int x;
  | Var (x) -> 
    print_string " ";
    print_string x;
  | Op  (op, exp1, exp2) ->
    print_string " ";
    print_op op;
    print_expr exp1;
    print_expr exp2


let print_comp (comp:comp) =
  match comp with
  | Eq -> print_string "="
  | Ne -> print_string "<>"
  | Lt -> print_string "<"
  | Le -> print_string "<="
  | Gt -> print_string ">" 
  | Ge -> print_string ">="


let print_cond (cond:cond) =
  match cond with 
  | (x,y,z) ->
    print_expr x;
    print_string " ";
    print_comp y;
    print_expr z


let rec print_indentation (indent:int) (pos:int) (txt:string) =
    if (pos != indent) then
      print_indentation indent (pos+1) (" "^txt)
    else 
      txt
        
let rec max_pos (block) (pos:position) = 
  match block with
  | [] -> pos
  | (x,y)::l ->
    if x > pos then
      max_pos l x
    else 
      max_pos l pos

let rec print_block (block) (indent:int) (pos) = 

    if (pos <= (max_pos block 0)) then
      print_string (print_indentation indent 0 "");
      let inst = search_block pos block in
      match inst with
      | None -> print_string "";
      | Some Set(x,y) -> 
        print_string x;
        print_string " :=";
        print_expr y;
        print_string "\n";
        print_block block indent (pos+1)
      | Some Read(x) -> 
        print_string "READ ";
        print_string x;
        print_string "\n";
        print_block block indent (pos+1)
      | Some Print(x) ->
        print_string "PRINT";
        print_expr x;
        print_string "\n";
        print_block block indent (pos+1)
      | Some If(c,b1,b2) ->
        print_string "IF";
        print_cond c;
        print_string "\n";
        print_block b1 (indent + 2) (pos+1);
        if (b2 != []) then
          print_string "ELSE \n";
          print_block b2 (indent + 2) (pos+ List.length b1 + 2);
        print_block block indent (pos+ List.length b1 + 1 + List.length b2 +1)
      | Some While(c,b) ->
        print_string "WHILE";
        print_cond c;
        print_string "\n";
        print_block b (indent + 2) (pos+1);
        print_block block indent (pos + List.length b + 1 )


(*fct aux pr recherche de la 1er (et suite) instruction
pr chaque instruction
print l'expression en fonction de ce qu'elle contient
*)


(***************************************************
****************************************************
******************READ POLISH***********************
****************************************************
****************************************************)



(* decouper le fichier en ligne de string *)
let rec convert_file_in_string_list (pos:position) (file:in_channel) =
  (pos, input_line file)::(convert_file_in_string_list (pos + 1) file)

let lecture (filename) = 
let file = open_in filename in
  let rec read_line (lines) (file) (pos:position) =
  try
    let line = input_line file in
      read_line (lines@[pos, (line)]) file (pos+1)
   with e ->
      if e = End_of_file then 
        lines 
      else 
        failwith "unknown error"
  in
  read_line [] file 0
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

let decoupe_list (list:name) = 
  let rec parcours (liste:name) (pos) (ret)= 
    if (pos < String.length liste) then
      parcours liste (pos+1) ((String.make 1 (String.get liste pos))::ret)
    else
      ret
  in
  parcours list 0 []


(* decouper la liste de string du fichier en liste de liste de mots *)
let rec convert_string_list_in_string_list_list (string_list) (pos_string_list_list) =
  match string_list with 
  | [] -> pos_string_list_list
  | (x,y)::l -> 
    let c = List.rev (decoupe_list y) in
    let chars = String.split_on_char ' ' y in 
    (*let chars_whithout_spaces = List.filter (fun x -> x != " ") chars in *)
    convert_string_list_in_string_list_list l ((x, c)::pos_string_list_list)
    

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
        [x]::(y::l)::finale
      else 
        [x]::[y]::finale
| _ -> failwith "separate error fin"
    
(* transforme une liste de mots en expression*)
let rec convert_expr (list:name list) =
  match list with 
  | [] -> failwith "error vide"
  | ""::l -> convert_expr l
  | " "::l -> convert_expr l
  | ":"::l -> convert_expr l
  | "="::l -> convert_expr l
  | ":="::l -> convert_expr l
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
  | x::l -> 
    try
      Num(int_of_string x)
    with e ->
      Var(x)

let rec separate_cond (list:name list) (f_exp:name list) (l_exp:name list) = 
  match list with
  | x::y::l ->
    if name_is_comp (x) then
      (x, f_exp, (y::l))
    else
      separate_cond (y::l) (x::f_exp) l_exp
  | _ -> failwith "proutage"

(*create condition*)
let create_condition (list:name list) = 
  let liste = separate_cond list [] [] in
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
  | ""::l -> convert_read l
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

let rec get_profondeur (list:name) (prof:position) =
  if (prof < String.length list) then
    if (String.get list prof == ' ') then
      get_profondeur list (prof+1)
    else
      prof
  else
    prof

let rec constr_list (prem_list)(nv_liste) (prof) = 
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

  (*
let test (y) (x) (l) =
  let prof = get_profondeur y 0 in
  let db_list = constr_list ( (x,y)::l) [] prof in
  let list_now = get_big_list db_list in
  let first_block = get_name_list db_list in 
  match list_now with
        | [] -> failwith "liste now vide"
        | (a,b)::c -> 
            if (String.get b prof) == "ELSE"  then
              let db_list2 = constr_list ( c ) [] prof in
              let list_now = get_big_list db_list in
              let second_block = get_name_list db_list in 
              [first_block]@[second_block]
            else
              [first_block]@[]
              *)
              

let is_Else (liste) = 
  let l = String.split_on_char ' ' liste in
  if (List.mem "ELSE" l) then
    true
  else
    false


let rec get_line_at_pos (pos_string_list_list:(position * name) list) (pos) =
  match pos_string_list_list with
  | [] -> 
    print_string (string_of_int pos);
    failwith "pas de ligne a la pos"
  | (x,y)::l ->
    if x == pos then
      y
    else
      get_line_at_pos l pos

let rec get_max_pos (liste) (pos) = 
  match liste with
  | [] -> pos
  | (x,y)::l ->
    if (x > pos ) then
      get_max_pos l (x)
    else
      get_max_pos l pos


let rec create_block (liste:(position*name)list) (pos_init:position) (pos_actu:position) (prof_init) (first) (f_list) (s_list)= 
  if (pos_actu > get_max_pos liste 0) then
    (pos_actu, f_list::s_list::[])
  else
    let y = get_line_at_pos liste pos_actu in
    if (get_profondeur y 0 > prof_init) then
      if first then
        create_block liste pos_init (pos_actu+1) prof_init first ((pos_actu,y)::f_list) s_list
      else
        create_block liste pos_init (pos_actu+1) prof_init first f_list ((pos_actu,y)::s_list)
    else
      if is_Else y then
        create_block liste pos_init (pos_actu+1) prof_init false f_list s_list
      else
        (pos_actu, f_list::s_list::[])


let rec get_liste_after_pos (liste_globale:(position * name list) list) (liste_fin:(position * name list) list) (pos:position) =
  match liste_globale with
  | [] -> liste_fin
  | (x,y)::l ->
    if x > pos then
      get_liste_after_pos liste_globale ( (x,y)::liste_fin) (pos)
    else
      get_liste_after_pos liste_globale (liste_fin) (pos)


let cas_if (liste_blocks: position*(position * name)list list) = 
  match liste_blocks with
  | (x,y) ->
    match y with 
      | a::b::l ->
        a::b::[]
      | _ -> failwith "rien ds le if"

let cas_if_pos (liste_blocks: position*(position * name)list list) = 
  match liste_blocks with
  | (x,y) -> x

let rec affiche_list (list) =
  match list with
  | (x,y)::l -> 
    print_int x;
    print_string y;
    affiche_list l
  | _-> print_string ""

(* convertit une liste de (pos, list) list en (pos, instr ) list*)
let rec convert_list_in_ocaml (pos_string_list_list:(position * name) list) (list_fin) (pos) =
  if (pos <= get_max_pos pos_string_list_list 0) then
    let lis = get_line_at_pos pos_string_list_list pos in
    let prof = get_profondeur lis 0 in
    let liste = String.split_on_char ' ' lis in 
    let rec parcours_de_la_liste (liste) = 
      match liste with
        | ""::l -> parcours_de_la_liste l
        | " "::l -> parcours_de_la_liste l
        | "READ"::l ->
          convert_list_in_ocaml pos_string_list_list ((pos, convert_read l)::list_fin) (pos+1)
        | "PRINT"::l ->
          convert_list_in_ocaml pos_string_list_list ((pos, convert_print l)::list_fin) (pos+1)
        | "IF"::l ->
          let co = create_condition l in 
          let liste_blocks = create_block pos_string_list_list pos (pos+1) prof true [] [] in 
          let cas_if = cas_if liste_blocks in
          let b_a = convert_list_in_ocaml (List.nth cas_if 0) [] (pos+1) in
          let b_b = convert_list_in_ocaml (List.nth cas_if 1) [] (pos+2+List.length b_a) in 
          let e = If(co, b_a, b_b) in 
          convert_list_in_ocaml pos_string_list_list ((pos,e)::(List.append(List.append b_a b_b) list_fin)) (cas_if_pos liste_blocks)
          (*
          block1 = (pos * instr) list 
          block2 = (pos * instr) list
          If( co, block1, block2)
          add block1 et block2 a la liste de fin
          recursion a la pos d'apres le block2
          *)
        | "WHILE"::l ->
          let co = create_condition l in 
          let liste_blocks = create_block pos_string_list_list pos (pos+1) prof true [] [] in 
          let cas_if = cas_if liste_blocks in
          let b_a = convert_list_in_ocaml (List.nth cas_if 0) [] (pos+1) in
          let e = While(co,b_a) in
          convert_list_in_ocaml pos_string_list_list ((pos,e)::(List.append b_a list_fin)) (cas_if_pos liste_blocks)
        | x::l ->
          convert_list_in_ocaml pos_string_list_list ((pos, Set(x, convert_expr l))::list_fin) (pos+1)
        | _ -> failwith "rien d'autre"

      in parcours_de_la_liste liste
  else
    list_fin

      
    (*
    match liste with 
    | (x,y) -> 
      (*let list_ocaml = convert_line_in_ocaml y  in 
      convert_list_in_ocaml l ((x , list_ocaml)::pos_ocaml_list_list)*)
      print_string (string_of_int x);
      let rec parcours_y (y:name list) = 
        match y with
        | [] -> failwith "Empty line"
        | ""::ll -> parcours_y ll
        | " "::ll -> parcours_y ll
        | "COMMENT"::ll -> convert_list_in_ocaml l pos_ocaml_list_list
        | "READ"::ll -> 
          let e = convert_read ll in
          convert_list_in_ocaml l ( (x, e)::pos_ocaml_list_list)
        | "PRINT"::ll-> 
          let e = convert_print ll in 
          convert_list_in_ocaml l ( (x, e)::pos_ocaml_list_list)
        | "IF"::ll ->
          let res = test y x l in
          let e = If(create_condition y, (convert_list_in_ocaml (List.nth res 1) [] ), (convert_list_in_ocaml (List.nth res 2) [])) in 
          convert_list_in_ocaml l ( (x, e)::pos_ocaml_list_list) 
        | "WHILE"::ll ->
          let prof = get_profondeur y 0 in 
          let db_list = constr_list ( (x,y)::l) [] prof in
          let list_now = get_big_list db_list in
          let first_block = get_name_list db_list in 
          let e = While(create_condition y, (convert_list_in_ocaml first_block [])) in 
          convert_list_in_ocaml l ( (x, e)::pos_ocaml_list_list)
        | v::ll -> 
          let e = Set(v, convert_expr ll) in 
          convert_list_in_ocaml l ( (x, e)::pos_ocaml_list_list)

      in parcours_y y
      *)


let read_polish (filename:string) =
(*
decouper le fichier en ligne de string
pour chaque ligne de string, la decouper en liste de string/char
pour chaque liste de string/char, convertir en code ocaml
*)

  let contenu = lecture filename in 
  (*let list_list_mot = convert_string_list_in_string_list_list contenu [] in*)
  convert_list_in_ocaml contenu [] 0




let print_polish (p:program) =  
  print_block p 0 0

let eval_polish (p:program) : unit = failwith "TODO"

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> 
    let pro = read_polish file in 
    let p = (0, While( (Num(4),Eq,Num(5)),( (1, Print(Num(7)))::(2, Read("b"))::(3, Read("d"))::[] )))::(4, Read("g"))::[] in
    print_polish pro
  | [|_;"--eval";file|] -> 
    let pro = read_polish file in 
    eval_polish pro
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
