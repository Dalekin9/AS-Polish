open Syntaxe

(************************************)

let rec search_block (pos:position) (b:(position * instr)list) =
      match b with
      | [] -> (*failwith "NO"*) None
      | (x,y)::l ->
        if x = pos then
          Some y
        else
          search_block pos l

let rec max_pos (block) (pos:position) =
  match block with
  | [] -> pos
  | (x,y)::l ->
    if x > pos then
      max_pos l x
    else
      max_pos l pos

(***************************************)

(*retourne la valeur d'une Constante*)
let get_val e : int =
    match e with
    | Num(i) -> i
    | _ -> failwith "erreur get_val : appel sur autre que Const"

(*Applique l'operation sur e1 et e2*)
let apply_Op op e1 e2 : int =
    match op with
    | Add -> e1 + e2
    | Sub -> e1 - e2
    | Mul -> e1 * e2
    | Div -> e1 / e2
    | Mod -> e1 % e2

(*determiner si l'expression est une Constante*)
let is_Const (e:expr) : boolean =
    match e with
    | Num(i) -> true
    | _ -> false

(*simplifie une expression*)
let simpl_exp (e:expr) : expr =
    match e with
    | Num(i) -> Num(i)
    | Var(n) -> Var(n)
    | Op(o,e1,e2) ->
        if is_Const e1 && is_Const e2 then
            Num(apply_Op o (get_val e1) (get_val e2))
        else if is_Const e1 && (get_val e1) = 0 then
            match o with
            | Add -> e2
            | Sub -> e2
            | Mul -> Num(0)
            | Div -> Num(0)
            | Mod -> Num(0)
        else if is_Const e1 && (get_val e1) = 1 then
            match o with
            | Mul -> e2
            | _ -> Op(o,e1,e2)
        else if is_Const e2 && (get_val e2) = 0 then
            match o with
            | Add -> e1
            | Sub -> e1
            | Mul -> Num(0)
            | Div -> Op(o,e1,e2)
            | Mod -> Op(o,e1,e2)
        else if is_Const e2 && (get_val e2) = 1 then
            match o with
            | Mul -> e1
            | _ -> Op(o,e1,e2)

let simpl_cond (c) : cond =
    match c with
    | (e1, com, e2) -> cond( (simpl_exp e1), com, (simpl_exp e2) )

let rec simpl_const (p:program) (pos) (p2:program)=
    if (pos > max_pos p) then
        let inst = search_block pos p in
        match inst with
        | Set(n,e) -> simpl p (pos+1) ( (pos, Set(n, (simpl_exp e)))::p2)
        | Read(n) -> simpl p (pos+1) ( (pos, Read(n))::p2 )
        | Print(e) -> simpl p (pos+1) ( (pos, Print(simpl_exp e))::p2 )
        | If(c,b1,b2) ->
            let nb1 = simpl_const b1 (pos+1) [] in
            let nb2 = simpl_const b2 (max_pos nb1 + 1) [] in
            let e = If( (simpl_cond c), nb1, nb2) in
            if (nb2 = []) then
                simpl p (max_pos nb1 +1) ( (pos, e)::p2 )
            else
                simpl p (max_pos nb2 +1) ( (pos, e)::p2 )
        | While(c,b) ->
            let nb = simpl_const b (pos+1) [] in
            let e = While( (simpl_cond c), nb) in
            simpl p (max_pos nb +1) ( (pos, e)::p2 )
    else
        p2

let simpl_polish (p:program) : program =
    let prog_after_const = simpl_const p 0 []

