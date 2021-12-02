(* decouper le fichier en ligne de string *)
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
  else 
    if (name_is_op y) then
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
  | [] -> failwith "Expression vide."
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
  | _ -> failwith "Impossible de separer la condition en 2"

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
    | _ -> failwith "Erreur dans la creation de condition"


(* convertit une ligne READ*)
let rec convert_read (list:string list) =
  match list with 
  | [] -> failwith "Pas de variable pour le READ"
  | ""::l -> convert_read l
  | " "::l -> convert_read l
  | x::_ -> Read(x)


(* convertit une ligne PRINT*)
let rec convert_print (list:string list) =
  match list with 
  | [] -> failwith "Pas de variable pour le PRINT"
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

(* convertit une liste de (pos, list) list en (pos, instr ) list*)
let rec convert_list_in_ocaml (pos_string_list_list:(position * name) list) (list_fin) (pos) (prof_base)=
  if (pos <= get_max_pos pos_string_list_list 0) then
    let lis = get_line_at_pos pos_string_list_list pos in
    let prof = get_profondeur lis 0 in
    if (prof != prof_base) then
      failwith "Pas d'indentation correcte."
    else
    let liste = String.split_on_char ' ' lis in 
    let rec parcours_de_la_liste (liste) = 
      match liste with
        | "READ"::l ->
          convert_list_in_ocaml pos_string_list_list ((pos, convert_read l)::list_fin) (pos+1) (prof_base)
        | "PRINT"::l ->
          convert_list_in_ocaml pos_string_list_list ((pos, convert_print l)::list_fin) (pos+1) (prof_base)
        | "IF"::l ->
          let co = create_condition l in 
          let liste_blocks = create_block pos_string_list_list pos (pos+1) prof true [] [] in 
          let cas_if = cas_if liste_blocks in
          let b_a = convert_list_in_ocaml (List.nth cas_if 0) [] (pos+1) (prof_base + 2) in
          let b_b = convert_list_in_ocaml (List.nth cas_if 1) [] (pos+2+List.length b_a) (prof_base + 2) in 
          let e = If(co, b_a, b_b) in 
          convert_list_in_ocaml pos_string_list_list ((pos,e)::(List.append(List.append b_a b_b) list_fin)) (cas_if_pos liste_blocks) (prof_base)
        | "WHILE"::l ->
          let co = create_condition l in 
          let liste_blocks = create_block pos_string_list_list pos (pos+1) prof true [] [] in 
          let cas_if = cas_if liste_blocks in
          let b_a = convert_list_in_ocaml (List.nth cas_if 0) [] (pos+1) (prof_base+2) in
          let e = While(co,b_a) in
          convert_list_in_ocaml pos_string_list_list ((pos,e)::(List.append b_a list_fin)) (cas_if_pos liste_blocks) (prof_base)
        | "COMMENT"::l ->
          convert_list_in_ocaml pos_string_list_list (list_fin) (pos+1) (prof_base)
        | x::l ->
          convert_list_in_ocaml pos_string_list_list ((pos, Set(x, convert_expr l))::list_fin) (pos+1) (prof_base)
        | _ -> failwith "La ligne ne commence pas avec quelque chose de correct."
      in parcours_de_la_liste liste
  else
    list_fin

let read_polish (filename:string) =

  let contenu = lecture filename in 
  convert_list_in_ocaml contenu [] 0 0
