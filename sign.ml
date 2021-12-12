
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

type sign = Neg | Zero | Pos | Error 

let rec search_block (pos:position) (b:(position * instr)list) =
  match b with
  | [] -> failwith "NO"
  | (x,y)::l ->
    if x = pos then
      y
    else
      search_block pos l


let max_pos (p:program) (pos:position) = 
  pos


  (************************************************)
  (************************************************)
  (************************************************)
  (************************************************)
  (************************************************)

(*determine si exp est dans la liste*)
let rec is_in_list (liste : (expr * sign list)list) (exp:expr) : bool =
  match liste with
  | [] -> false
  | (e,_)::l ->
    if (e = exp) then
      true
    else
    is_in_list l exp

(*fusionne les deux liste de signe en fonction de l'addition*)
let join_add l1 l2 : sign list =
  if (l1 = Pos::[]) then
    if (l2 = Pos::[] || l2 = Zero::[]) then
      [Pos]
    else if (l2 = Pos::Zero::[] || l1 = Zero::Pos::[] ) then
      Pos::Zero::[]
    else
      Pos::Zero::Neg::[]
  else if (l1 = Pos::Zero::[] || l1 = Zero::Pos::[] ) then
    if (l2 = Zero::[]) then
      [Zero]
    else if (l2 = Pos::[]) then
      [Pos]
    else if (l2 = Pos::Zero::[] || l1 = Zero::Pos::[] ) then
      Pos::Zero::[]
    else
      Pos::Zero::Neg::[]
  else if (l1 = Zero::[]) then
    if (l2 = Zero::[]) then
      [Zero]
    else if (l2 = Pos::[]) then
      [Pos]
    else if (l2 = Pos::Zero::[] || l1 = Zero::Pos::[] ) then
      Pos::Zero::[]
    else if (l2 = Neg::[]) then
      Neg::[]
    else
      Zero::Neg::[]
  else
    Zero::Neg::[]

(*fusionne les deux liste de signe en fonction de la soustraction*)
let join_sub l1 l2 : sign list =
  if (l1 = Pos::[]) then
    if (l2 = Pos::[] || l2 = Zero::[]) then
      [Pos]
    else if (l2 = Pos::Zero::[] || l1 = Zero::Pos::[] ) then
      Pos::Zero::[]
    else
      Pos::Zero::Neg::[]
  else if (l1 = Pos::Zero::[] || l1 = Zero::Pos::[] ) then
    if (l2 = Zero::[]) then
      [Zero]
    else if (l2 = Pos::[]) then
      [Pos]
    else if (l2 = Pos::Zero::[] || l1 = Zero::Pos::[] ) then
      Pos::Zero::[]
    else
      Pos::Zero::Neg::[]
  else if (l1 = Zero::[]) then
    if (l2 = Zero::[]) then
      [Zero]
    else if (l2 = Pos::[]) then
      [Pos]
    else if (l2 = Pos::Zero::[] || l1 = Zero::Pos::[] ) then
      Pos::Zero::[]
    else if (l2 = Neg::[]) then
      Neg::[]
    else
      Zero::Neg::[]
  else
    Zero::Neg::[]


let is_possible_Eq (l1) (l2) : bool =
    false

(*determine les signes possible d'une expression*)
let rec sign_expr (exp:expr) (liste) : (sign list) =
  match exp with
  | Num(i) -> 
    if i = 0 then
      [Zero]
    else if i < 0 then
      [Neg]
    else
      [Pos]
  | Var(n) ->
    (* si existe dans liste -> retourne sa valeur
    sinon pos neg zero*)
    Pos::Neg::Zero::[]
  | Op(o,e1,e2) ->
    let l1 = sign_expr e1 liste in
    let l2 = sign_expr e2 liste in
    match o with
    | Add -> join_add l1 l2
    | Sub -> join_sub l1 l2
    | Mul -> failwith "TODO"
    | Div -> failwith "TODO"
    | Mod -> failwith "TODO"

(*determine les signes d'une condition et met potentiellement a jour les valeurs*)
let sign_cond (c:comp) (e1:expr) (e2:expr) (liste) : bool =
  let l1 = sign_expr e1 liste in
  let l2 = sign_expr e2 liste in 
  match c with
  | Eq -> 
    if is_possible_Eq l1 l2 then
      true
    else
      false
  | Ne (* Not equal, <> *) -> failwith "TODO"
  | Lt (* Less than, < *) -> failwith "TODO"
  | Le (* Less or equal, <= *) -> failwith "TODO"
  | Gt (* Greater than, > *) -> failwith "TODO"
  | Ge (* Greater or equal, >= *) -> failwith "TODO"

let cond_inverse (c:comp) : comp =
  match c with 
  | Eq -> Ne
  | Ne -> Eq
  | Lt -> Ge
  | Le -> Gt
  | Gt -> Le
  | Ge -> Lt

let rec prop_sign (p:program) (pos:position) (liste : (name * sign list)list) : ((name * sign list)list) =
  if (pos <= max_pos p 0) then
    (
      let inst = search_block pos p in
      match inst with
      | Set(n,e) ->
        let l = sign_expr e liste in
        prop_sign p (pos+1) ( (n,l)::liste)
      | Read(n) ->
        let l = Pos::Zero::Neg::[] in
        prop_sign p (pos+1) ( (n,l)::liste)
      | Print(e) ->
        prop_sign p (pos+1) liste
      | If((e1,c,e2),b1,b2) ->
        let lc1 = sign_cond c e1 e2 liste in
        let cc = cond_inverse c in 
        let lc2 = sign_cond cc e1 e2 liste in
        if lc1 = false then (*cas impossible ?*)
          if lc2 = false then
            if b2 = [] then
              prop_sign p (max_pos b1 0 +1) liste
            else
              prop_sign p (max_pos b2 0 +1) liste
          else
            if b2 = [] then
              prop_sign p (max_pos b1 0 +1) liste
            else
            (
              (*update liste avec cond*)
              let liste_fin = prop_sign b2 (max_pos b1 0 +1) liste in
              prop_sign p (max_pos b2 0 +1) liste_fin
            )
        else
          if lc2 = false then
            (
              (*update liste avec cond*)
              let liste_fin = prop_sign b1 (pos +1) liste in
              if b2 = [] then
                prop_sign p (max_pos b1 0 +1) liste_fin
              else
                prop_sign p (max_pos b2 0 +1) liste_fin
            )
          else
            (
              (*update liste avec cond*)
              let liste_fin = prop_sign b1 (pos +1) liste in
              if b2 = [] then
                prop_sign p (max_pos b1 0 +1) liste_fin
              else
                (
                  let liste_fin2 = prop_sign b2 (max_pos b1 0 +1) liste in
                  (*join liste_fin 1 et 2*)
                  prop_sign p (max_pos b2 0 +1) liste_fin
                )
            )
      | While(c,b) ->
        (*update liste avec cond*)
        let liste_fin = prop_sign b (pos+1) liste in
        (*join liste et liste_fin*)
        (*join resultat et inverse de condition*)
        prop_sign p (max_pos b 0 +1) liste
    )
  else
    liste

let rec print_list_sign (liste: sign list) : unit =
  match liste with
  | [] -> print_string ""
  | Zero::l ->
    print_string "0";
    print_list_sign l
  | Neg::l ->
    print_string "-";
    print_list_sign l
  | Pos::l ->
    print_string "+";
    print_list_sign l
  | Error::l ->
    print_string "!";
    print_list_sign l

let rec print_list (liste:(name * sign list)list) (pos:position) : unit =
  match liste with
  | [] -> print_string ""
  | (e,l)::ll ->
    print_string (e^" ");
    print_list_sign l;
    print_string "\n"
    

let sign (p:program) : unit = 
  let liste = prop_sign p 0 [] in
  print_list liste 0