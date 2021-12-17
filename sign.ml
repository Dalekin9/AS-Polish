open Syntaxe
open Functions

let is_var e =
  match e with
  | Var(_) -> true
  | _ -> false

let get_val e =
  match e with
  | Var(x) -> x
  | _ -> failwith " ce n'est pas une Var(x)"
  
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
  if ( List.exists (fun x -> x = Pos) l1 && List.exists (fun x -> x = Neg) l2 ) ||
       (List.exists (fun x -> x = Neg) l1 && List.exists (fun x -> x = Pos) l2 ) then
      Pos::Zero::Neg::[]
  else if (l1 = Pos::[] && l2 = Pos::Zero::[]) || (l1 = Pos::Zero::[] && l2 = Pos::[] )  then
    Pos::[]
  else if l1 = Pos::Zero::[] && l2 = Pos::Zero::[]  then
    Pos::Zero::[]
  else if (l1 = Neg::[] && l2 = Zero::Neg::[]) || (l1 = Zero::Neg::[] && l2 = Neg::[] ) then
    Neg::[]
  else if l1 = Zero::Neg::[] && l2 = Zero::Neg::[] then
    Zero::Neg::[]
  else if l1 = Zero::[] && l2 = Zero::[] then
    Zero::[]
  else
    failwith "Erreur dans la jonction des listes de signs dans l'addition"

(*fusionne les deux liste de signe en fonction de la soustraction*)
let join_sub l1 l2 =
 if List.exists (fun x -> x = Pos) l1 && List.exists (fun x -> x = Pos) l2 then
    Pos::Zero::Neg::[]
  else if ( l1 = Neg::[] && (l2 = Pos::Zero::[] || l2 = Pos::[]) )  then
    Neg::[]
  else if ( l1 = Zero::Neg::[] && l2 = (Pos::Zero::[])  ) then
    Zero::Neg::[]
  else if (  l1 = Pos::[] && (l2 = (Zero::[]) || l2 = Zero::Neg::[] || l2 = Neg::[] ) ) then
    Pos::[]
  else if  l1 = Pos::Zero::[] && l2 = (Zero::Neg::[]) then
    Pos::Zero::[]
  else if l1 = Zero::[] && l2 = (Zero::[]) then
    Zero::[]
  else
    failwith "Erreur dans la jonction des listes de signs dans la soustraction"

(*fusionne les deux liste de signe en fonction de la multiplication*)
let join_mul l1 l2 =
  if List.exists (fun x -> x = Zero) l2 || List.exists (fun x -> x = Zero) l1 then
   Zero::[]
  else if l1 = Pos::[] && l2 = Pos::[]  then
    Pos::[]
  else if l1 = Neg::[] && l2 = Neg::[] then
    Pos::[]
  else if (l1 = Pos::[] && l2 = Neg::[]) || (l2 = Pos::[] && l1 = Neg::[]) then
    Neg::[]
  else
    failwith "Erreur dans la jonction des listes de signs dans la multiplication"

(*fusionne les deux liste de signe en fonction de la division*)
let join_div l1 l2 =
  if List.exists (fun x -> x = Zero) l2  then (
    if List.exists (fun x -> x = Zero) l1  then
      Zero::Error::[]
    else if (l1 = Pos::[] && l2 = Pos::[]) || (l1 = Neg::[] && l2 = Neg::[])  then
      Pos::Error::[]
    else if (l1 = Neg::[] && l2 = Pos::[]) || (l1 = Pos::[] && l2 = Neg::[]) then
      Neg::Error::[]
    else
      Pos::Neg::Error::[] )
  else
    (
      if List.exists (fun x -> x = Zero) l1  then
        Zero::[]
      else if (l1 = Pos::[] && l2 = Pos::[]) || (l1 = Neg::[] && l2 = Neg::[])  then
        Pos::[]
      else if (l1 = Neg::[] && l2 = Pos::[]) || (l1 = Pos::[] && l2 = Neg::[]) then
        Neg::[]
      else
        Pos::Neg::[]
    )

(*fusionne les deux liste de signe en fonction du modulo*)
let join_mod l1 l2 =
  if List.exists (fun x -> x = Zero) l2  then (
    if List.exists (fun x -> x = Zero) l1  then
      Zero::Error::[]
    else if (l1 = Pos::[] || l1 = Neg::[]) && l2 = Pos::[]  then
      Pos::Error::[]
    else if (l1 = Pos::[] || l1 = Neg::[]) && l2 = Neg::[] then
      Neg::Error::[]
    else
      Pos::Neg::Error::[])
  else (
    if List.exists (fun x -> x = Zero) l1  then
      Zero::[]
    else if (l1 = Pos::[] || l1 = Neg::[]) && l2 = Pos::[]  then
      Pos::[]
    else if (l1 = Pos::[] || l1 = Neg::[]) && l2 = Neg::[] then
      Neg::[]
    else
      Pos::Neg::[])

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

let is_possible_Eq (l1) (l2) =
  l1 = l2

let is_possible_Ne l1 l2 = 
  l1 != l2

let is_possible_Lt l1 l2 =
  if List.exists (fun x -> x = Zero) l1 || List.exists (fun x -> x = Pos) l1 then
    l2 = Pos::[]
  else if List.exists (fun x -> x = Neg) l1 then
    true
  else
    failwith "erreur is possible < (Lt)"   

let is_possible_Le l1 l2 =
  if List.exists (fun x -> x = Pos) l1 then
    if l2 != Pos::[] then
      false
    else
      true
  else if List.exists (fun x -> x = Zero) l1 then
    if l2 != Pos::[] || l2 != Pos::Zero::[] then
      false
    else
      true
  else if List.exists (fun x -> x = Neg) l1 then
    true
  else
    failwith "erreur is possible <= (Le)"   

let is_possible_Gt l1 l2 =
  if List.exists (fun x -> x = Pos) l1 then
    true
  else if List.exists (fun x -> x = Zero) l1 || l1 = Neg::[]then
    l2 = Neg::[]
  else
    failwith "erreur is possible > (Gt)"    

let is_possible_Ge l1 l2 =
  if List.exists (fun x -> x = Pos) l1 then
    true
  else if List.exists (fun x -> x = Zero) l1 then
    if l2 != Neg::[] || l2 != Zero::Neg::[] then
      false
    else
      true
  else if l1 = Neg::[] then
    if l2 != Neg::[] then
      false
    else
      true
  else
    failwith "erreur is possible >= (Ge)"    
    
(*determine si une condition est possible vis a vis de l'environnemnt de ses expressions*)
let cond_is_possible c e1 e2 liste =
  let l1 = sign_expr e1 liste in
  let l2 = sign_expr e2 liste in 
  match c with
  | Eq -> is_possible_Eq l1 l2 
  | Ne -> is_possible_Ne l1 l2 
  | Lt -> is_possible_Lt l1 l2 
  | Le -> is_possible_Le l1 l2 
  | Gt -> is_possible_Gt l1 l2 
  | Ge -> is_possible_Ge l1 l2 

(*determine les signes d'une condition et met potentiellement a jour les valeurs
on suppose ici que la condition est possible*)
let sign_cond (c) (e1) (e2) (liste) =
  let l1 = sign_expr e1 liste in
  let l2 = sign_expr e2 liste in 
  match c with
  | Eq -> l2
  | Ne -> 
    (
      let rec funct l1 l2 =
        if l2 = [] then l1 
        else (
        if (List.exists (fun x -> x = List.hd l2) l1) then
          funct (List.filter (fun x -> x != List.hd l2) l1) (List.tl l2)
        else
          funct (l1) (List.tl l2) )
        in
        funct l1 l2 
    )
  | Lt -> (
        if List.exists (fun x -> x = Neg) l2 then
          Neg::[]
        else if List.exists (fun x -> x = Zero) l2 then
          Neg::[]
        else
          Pos::Zero::Neg::[]
  )
  | Le -> (
    if List.exists (fun x -> x = Neg) l2 then
      Neg::[]
    else if List.exists (fun x -> x = Zero) l2 then
      Zero::Neg::[]
    else
      Pos::Zero::Neg::[]
  )
  | Gt -> l2
  | Ge -> l2

(*retourner le comparateur inverse*)
let cond_inverse (c)=
  match c with 
  | Eq -> Ne
  | Ne -> Eq
  | Lt -> Ge
  | Le -> Gt
  | Gt -> Le
  | Ge -> Lt

let rec union_error l1 l2 =
  if (List.exists (fun x -> x = List.hd l1) l2) then
      union_error (List.tl l1) (l2)
  else
      union_error (List.tl l1) (List.cons (List.hd l1) l2)

let rec union_sign l1 l2 =
  if SignTable.is_empty l1 then l2 
  else
    (
    let va = SignTable.choose l1 in
    match va with
    | (k,d) ->
      if SignTable.mem k l2 then
        union_sign (SignTable.remove k l1) (SignTable.update k (union_error d (SignTable.find k l2) ) l2)
      else
        union_sign (SignTable.remove k l1) (SignTable.add k d l2)
    )


(*: ([SignTable]::[int list])*)
let rec prop_sign (p) (pos) (signs) (errors) =
  if (pos <= max_pos p 0) then
    (
      let inst = search_block pos p in
      match inst with

      | Set(n,e) ->
        let l = sign_expr e signs in
        if SignTable.exists (fun x y -> x = n) signs then
          (
            let sign = SignTable.remove n signs in
            prop_sign p (pos+1) (SignTable.add n l sign) (if List.exists (fun x -> x = Error) l then pos::errors else errors)
          )
        else
          prop_sign p (pos+1) (SignTable.add n l signs) (if List.exists (fun x -> x = Error) l then pos::errors else errors)

      | Read(n) ->
        let l = sign_expr (Var(n)) signs in
        if SignTable.exists (fun x y -> x = n) signs then
          (
            let sign = SignTable.remove n signs in
            prop_sign p (pos+1) (SignTable.add n l sign) errors
          )
        else
          prop_sign p (pos+1) (SignTable.add n l signs) errors

      | Print(e) ->
        prop_sign p (pos+1) signs errors

      | If((e1,c,e2),b1,b2) ->
        let lc1 = cond_is_possible c e1 e2 signs in
        let cond1 = (if lc1 then sign_cond c e1 e1 signs else []) in
        let cc = cond_inverse c in 
        let lc2 = cond_is_possible cc e1 e2 signs in
        let cond2 = (if lc2 then sign_cond cc e1 e1 signs else []) in
        let signs1 = (
            if (is_var e1) then(
                let value = get_val e1 in
                let sign = SignTable.remove value signs in
                SignTable.add value cond2 sign)
            else
              signs) in
        let signs2 = (
            if (is_var e1) then(
              let value = get_val e1 in
              let sign = SignTable.remove value signs in
              SignTable.add value cond2 sign)
            else
              signs ) in 
        if b2 = [] then(
            if lc1 = false then
                  prop_sign p (max_pos b1 0 +1) signs errors
            else (
                let liste_fin = prop_sign b1 (pos +1) signs1 (if List.exists (fun x -> x = Error) cond1 then pos::errors else errors) in
                match liste_fin with
                | (x,y) -> prop_sign p (max_pos b1 0 +1) x y))
        else (
            if lc1 = false then
              if lc2 = false then
                prop_sign p (max_pos b2 0 +1) signs errors
              else (
                  let liste_fin = prop_sign b2 (max_pos b1 0 +1) signs1 (if List.exists (fun x -> x = Error) cond1 then pos::errors else errors) in
                  match liste_fin with
                  | (x,y) -> prop_sign p (max_pos b2 0 +1) x y )
            else (
                let liste_fin = prop_sign b1 (pos +1) signs1 (if List.exists (fun x -> x = Error) cond1 then pos::errors else errors) in
                  match liste_fin with
                  | (x,y) ->  (
                      if lc2 = false then 
                        prop_sign p (max_pos b2 0 +1) x y
                      else
                        let liste_fin2 = prop_sign b2 (max_pos b1 0 +1) signs2 (if List.exists (fun x -> x = Error) cond2 then pos::errors else errors) in
                        match liste_fin2 with
                        | (xx,yy) -> prop_sign p (max_pos b2 0 +1) (union_sign x xx) (union_error y yy)
                    )
            )
        )
      | While((e1,c,e2),b) ->
        let rec parcours_while env1 =
          match env1 with
          | (x,y) ->
              ( 
                if cond_is_possible c e1 e2 x then
                  (
                    let signs1 = (
                      if (is_var e1) then(
                        let value = get_val e1 in
                        let sign = SignTable.remove value x in
                        SignTable.add value (sign_cond c e1 e2 x) sign)
                      else
                        x ) in
                    let envx = prop_sign b (pos+1) signs1 y in
                    match envx with
                    | (xx,yy) -> (
                      if (x = xx) then
                        env1
                      else 
                        parcours_while envx )
                  )
                else
                  env1
              )
        in
        let env_fin = parcours_while (signs,errors) in
        match env_fin with
        | (s,e) ->
          (
            let cc = cond_inverse c in
            let cond_inv = sign_cond cc e1 e1 s in
            let signs1 = (
            if (is_var e1) then(
                let value = get_val e1 in
                let sign = SignTable.remove value s in
                SignTable.add value cond_inv sign)
            else
              s) in
            prop_sign p (max_pos b 0 +1) s e
          )
    )
  else
    (signs,errors)


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
let print_list (liste) =
  SignTable.iter (fun x y -> 
    (print_string (x^" ");
    print_list_sign y;
    print_string "\n") ) liste


(*fonction principale*)
let sign_polish (p) : unit = 
  let signs = SignTable.empty in
  let liste = prop_sign p 0 signs [] in
  match liste with
  | (x,y) ->
    print_list x;
    if y = [] then print_string "safe\n"
    else  
    List.iter (fun z -> 
      (print_int z;
        print_string " : Division by 0\n") ) y