open Syntaxe

(************************************)

let rec search_block (pos:position) (b:(position * instr)list) =
      match b with
      | [] -> (*failwith "NO"*) 
      print_string "ici";
      None
      | (x,y)::l ->
        if x = pos then
            (print_string "la";
          Some y)
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
let apply_Op (o:op) (e1:int) (e2:int) : int =
    match o with
    | Add -> e1 + e2
    | Sub -> e1 - e2
    | Mul -> e1 * e2
    | Div -> e1 / e2
    | Mod -> e1 mod e2

let apply_comp (comp) (e1:int) (e2:int) : bool =
    match comp with
    | Eq -> e1 = e2
    | Ne -> e1 <> e2
    | Lt -> e1 < e2
    | Le -> e1 <= e2
    | Gt -> e1 > e2
    | Ge -> e1 >= e2

(*determiner si l'expression est une Constante*)
let is_Const (e:expr) : bool =
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
            | Sub -> Op(o,e1,e2)
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
        else
            Op(o,e1,e2)

let simpl_cond (c) : cond =
    match c with
    | (e1, com, e2) -> ( (simpl_exp e1),com , (simpl_exp e2) )

let rec simpl_const (p:program) (pos:position) (p2:program)=
    print_int pos;
    print_int (max_pos p 0);
    print_string "\n";
    if (pos <= (max_pos p 0)) then
        (print_string "fesse";
        let inst = search_block pos p in
        print_string "inst";
        match inst with
        | None -> 
            print_int pos;
            print_string "\n";
            failwith "none"
        | Some Set(n,e) -> 
            print_int pos;
            print_string "\n";
            simpl_const p (pos+1) ( (pos, Set(n, (simpl_exp e)))::p2)
        | Some Read(n) -> 
            print_int pos;
            print_string "\n";
            simpl_const p (pos+1) ( (pos, Read(n))::p2 )
        | Some Print(e) -> 
            print_int pos;
            print_string "\n";
            simpl_const p (pos+1) ( (pos, Print(simpl_exp e))::p2 )
        | Some If(c,b1,b2) ->
            print_int pos;
            print_string "\n";
            let nb1 = simpl_const b1 (pos+1) [] in
            let nb2 = simpl_const b2 (max_pos nb1 0 + 1) [] in
            let e = If( (simpl_cond c), nb1, nb2) in
            if (nb2 = []) then
                simpl_const p (max_pos nb1 0 +1) ( (pos, e)::p2 )
            else
                simpl_const p (max_pos nb2 0 +1) ( (pos, e)::p2 )
        | Some While(c,b) ->
            print_int pos;
            print_string "\n";
            let nb = simpl_const b (pos+1) [] in
            let e = While( (simpl_cond c), nb) in
            simpl_const p (max_pos nb 0 +1) ( (pos, e)::p2 )
        )
    else
        p2

let cond_with_const (cond) : bool =
    match cond with
    | (e1, com, e2) -> 
        if is_Const e1 && is_Const e2 then
            true
        else false

let cond_is_true (cond) : bool =
match cond with
| (e1, com, e2) -> 
    apply_comp com (get_val e1) (get_val e2)

let rec simpl_block (p:program) (pos:position) (pos_instr:position) (p2:program): program =
    if (pos <= max_pos p 0) then
        let inst = search_block pos p in
        match inst with
        | None ->
            failwith "none"
        | Some Set(n,e) -> 
            simpl_block p (pos+1) (pos_instr +1) ( (pos_instr,Set(n,e))::p2 )
        | Some Read(n) -> 
            simpl_block p (pos+1) (pos_instr +1) ( (pos_instr,Read(n))::p2 )
        | Some Print(e) -> 
            simpl_block p (pos+1) (pos_instr +1) ( (pos_instr,Print(e))::p2 )
        | Some If(c,b1,b2) ->
            if cond_with_const c then 
                if cond_is_true c then
                    let p3 = simpl_block b1 (pos+1) (pos_instr) [] in
                    if (b2 = []) then
                        simpl_block p (max_pos b1 0 +1) (max_pos p3 0 +1) (List.append p3 p2)
                    else
                        simpl_block p (max_pos b2 0 +1) (max_pos p3 0 +1) (List.append p3 p2)
                else
                    if b2 = [] then
                        simpl_block p (max_pos b1 0 +1) (pos_instr) p2
                    else
                        let p3 = simpl_block b2 (max_pos b1 0 +1) (pos_instr) [] in
                        simpl_block p (max_pos b2 0 +1) (max_pos p3 0 +1) (List.append p3 p2)
            else
                let p3 = simpl_block b1 (pos+1) (pos_instr+1) [] in
                let p4 = simpl_block b1 (max_pos b1 0 +1) (max_pos p3 0 +1) [] in
                let e = If(c,p3,p4) in
                if (b2 = []) then
                    simpl_block p (max_pos b1 0+1) (max_pos p3 0 +1) ((pos_instr,e)::p2)
                else
                    simpl_block p (max_pos b2 0+1) (max_pos p4 0 +1) ((pos_instr,e)::p2)
        | Some While(c,b) ->
            if cond_with_const c then
                if cond_is_true c then
                    let nb = simpl_block b (pos+1) (pos_instr + 1) [] in 
                    simpl_block p (max_pos b 0 +1) (max_pos nb 0 +1) ( (pos_instr,While(c,nb))::p2 )
                else
                    simpl_block p (max_pos b 0 +1) pos_instr p2
            else
                simpl_block p (max_pos b 0 +1) pos_instr p2
            
    else 
        p2
        
let simpl_polish (p:program) : program =
    let p2 = simpl_const p 0 [] in
    simpl_block p2 0 0 []
    