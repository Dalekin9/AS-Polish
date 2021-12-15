open Syntaxe
open Functions
  
(*determine si exp est dans la liste*)
let rec is_in_list (liste) (exp) =
  match liste with
  | [] -> false
  | (e,_)::l ->
    if (e = exp) then
      true
    else
    is_in_list l exp

(*fusionne les deux liste de signe en fonction de l'addition*)
let join_add l1 l2=
  if ( (List.exists (fun x -> x = Pos) l1 && List.exists (fun x -> x = Neg) l2 ) ||
       (List.exists (fun x -> x = Neg) l1 && List.exists (fun x -> x = Pos) l2 ) ) then
      Pos::Zero::Neg::[]
  else if ( l1 = Pos::[] && l2 = (Pos::Zero::[]) || l1 = Pos::Zero[] && l2 = (Pos:[]) )  then
    Pos::[]
  else if ( l1 = Pos::Zero[] && l2 = (Pos::Zero::[])  ) then
    Pos::Zero::[]
  else if (  l1 = Neg::[] && l2 = (Zero::Neg::[]) || l1 = Zero::Neg[] && l2 = (Neg:[]) ) then
    Neg::[]
  else if ( l1 = Zero::Neg[] && l2 = (Zero::Neg::[]) then
    Zero::Neg::[]
  else if ( l1 = Zero::[] && l2 = (Zero::[]) then
    Zero::[]
  else
    failwith "Erreur dans la jonction des listes de signs dans l'addition"

(*fusionne les deux liste de signe en fonction de la soustraction*)
let join_sub l1 l2 =
 if ( (List.exists (fun x -> x = Pos) l1 && List.exists (fun x -> x = Neg) l2 ) ||
       (List.exists (fun x -> x = Neg) l1 && List.exists (fun x -> x = Pos) l2 ) ) then
      Pos::Zero::Neg::[]
  else if ( l1 = Pos::[] && l2 = (Pos::Zero::[]) || l1 = Pos::Zero[] && l2 = (Pos:[]) )  then
    Pos::[]
  else if ( l1 = Pos::Zero[] && l2 = (Pos::Zero::[])  ) then
    Pos::Zero::[]
  else if (  l1 = Neg::[] && l2 = (Zero::Neg::[]) || l1 = Zero::Neg[] && l2 = (Neg:[]) ) then
    Neg::[]
  else if ( l1 = Zero::Neg[] && l2 = (Zero::Neg::[]) then
    Zero::Neg::[]
  else if ( l1 = Zero::[] && l2 = (Zero::[]) then
    Zero::[]
  else
    failwith "Erreur dans la jonction des listes de signs dans la soustraction"

(*fusionne les deux liste de signe en fonction de la multiplication*)
let join_mul l1 l2 =
  if ( (List.exists (fun x -> x = Pos) l1 && List.exists (fun x -> x = Neg) l2 ) ||
    (List.exists (fun x -> x = Neg) l1 && List.exists (fun x -> x = Pos) l2 ) ) then
   Pos::Zero::Neg::[]
  else if ( l1 = Pos::[] && l2 = (Pos::Zero::[]) || l1 = Pos::Zero[] && l2 = (Pos:[]) )  then
    Pos::[]
  else if ( l1 = Pos::Zero[] && l2 = (Pos::Zero::[])  ) then
    Pos::Zero::[]
  else if (  l1 = Neg::[] && l2 = (Zero::Neg::[]) || l1 = Zero::Neg[] && l2 = (Neg:[]) ) then
    Neg::[]
  else if ( l1 = Zero::Neg[] && l2 = (Zero::Neg::[]) then
    Zero::Neg::[]
  else if ( l1 = Zero::[] && l2 = (Zero::[]) then
    Zero::[]
  else
    failwith "Erreur dans la jonction des listes de signs dans la multiplication"

(*fusionne les deux liste de signe en fonction de la division*)
let join_div l1 l2 =
  if ( (List.exists (fun x -> x = Pos) l1 && List.exists (fun x -> x = Neg) l2 ) ||
    (List.exists (fun x -> x = Neg) l1 && List.exists (fun x -> x = Pos) l2 ) ) then
    Pos::Zero::Neg::[]
  else if ( l1 = Pos::[] && l2 = (Pos::Zero::[]) || l1 = Pos::Zero[] && l2 = (Pos:[]) )  then
    Pos::[]
  else if ( l1 = Pos::Zero[] && l2 = (Pos::Zero::[])  ) then
    Pos::Zero::[]
  else if (  l1 = Neg::[] && l2 = (Zero::Neg::[]) || l1 = Zero::Neg[] && l2 = (Neg:[]) ) then
    Neg::[]
  else if ( l1 = Zero::Neg[] && l2 = (Zero::Neg::[]) then
    Zero::Neg::[]
  else if ( l1 = Zero::[] && l2 = (Zero::[]) then
    Zero::[]
  else
    failwith "Erreur dans la jonction des listes de signs dans la division"

(*fusionne les deux liste de signe en fonction du modulo*)
let join_mod l1 l2 =
  if ( (List.exists (fun x -> x = Pos) l1 && List.exists (fun x -> x = Neg) l2 ) ||
    (List.exists (fun x -> x = Neg) l1 && List.exists (fun x -> x = Pos) l2 ) ) then
    Pos::Zero::Neg::[]
  else if ( l1 = Pos::[] && l2 = (Pos::Zero::[]) || l1 = Pos::Zero[] && l2 = (Pos:[]) )  then
    Pos::[]
  else if ( l1 = Pos::Zero[] && l2 = (Pos::Zero::[])  ) then
    Pos::Zero::[]
  else if (  l1 = Neg::[] && l2 = (Zero::Neg::[]) || l1 = Zero::Neg[] && l2 = (Neg:[]) ) then
    Neg::[]
  else if ( l1 = Zero::Neg[] && l2 = (Zero::Neg::[]) then
    Zero::Neg::[]
  else if ( l1 = Zero::[] && l2 = (Zero::[]) then
    Zero::[]
  else
    failwith "Erreur dans la jonction des listes de signs dans le modulo"


let is_possible_Eq (l1) (l2) : bool =
    if l1 = l2 then
      true
    else false

(*determine les signes possible d'une expression*)
let rec sign_expr (exp) (liste) =
  match exp with
  | Num(i) -> 
    if i = 0 then
      [Zero]
    else if i < 0 then
      [Neg]
    else
      [Pos]
  | Var(n) ->
    (try SignTable.find n liste with Not_found -> (Pos::Neg::Zero::[]))
  | Op(o,e1,e2) ->
    let l1 = sign_expr e1 liste in
    let l2 = sign_expr e2 liste in
    match o with
    | Add -> join_add l1 l2
    | Sub -> join_sub l1 l2
    | Mul -> join_mul l1 l2
    | Div -> join_div l1 l2
    | Mod -> join_mod l1 l2

(*determine les signes d'une condition et met potentiellement a jour les valeurs*)
let sign_cond (c) (e1) (e2) (liste) =
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

(*retourner le comparateur inverse*)
let cond_inverse (c)=
  match c with 
  | Eq -> Ne
  | Ne -> Eq
  | Lt -> Ge
  | Le -> Gt
  | Gt -> Le
  | Ge -> Lt

(*: ([SignTable]::[int list])*)
let rec prop_sign (p) (pos) (signs) (errors) =
  if (pos <= max_pos p 0) then
    (
      let inst = search_block pos p in
      match inst with
      | Set(n,e) ->
        let l = sign_expr e liste in
        if Map.exist (fun x -> x = n) then
          (
            let sig = Map.remove n signs in
            prop_sign p (pos+1) (Map.add n l sign) errors
          )
        else
          prop_sign p (pos+1) (Map.add n l signs) errors

      | Read(n) ->
        let l = sign_expr e liste in
        if Map.exist (fun x -> x = n) then
          (
            let sig = Map.remove n signs in
            prop_sign p (pos+1) (Map.add n l sign) errors
          )
        else
          prop_sign p (pos+1) (Map.add n l signs) errors
      | Print(e) ->
        prop_sign p (pos+1) signs errors
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


(*print une liste de signe*)
let rec print_list_sign (liste) =
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

(*print la liste de signes possible pour chaque variable*)
let rec print_list (liste) =
  Map.iter (fun x y -> 
    (print_string (e^" ");
    print_list_sign y;
    print_string "\n") liste


(*fonction principale*)
let sign_polish (p) : unit = 
  let signs = SignTable.empty in
  let errors = ErrorTable.empty in 
  let liste = prop_sign p 0 [] in
  print_list List.nth liste 0
  List.iter (fun x -> 
    (print_string x;
      print_string " : Division by 0\n") (List.nth liste 1)