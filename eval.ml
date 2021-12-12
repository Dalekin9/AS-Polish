open Syntaxe

let rec next_pos (pos:position) (current: int) (p:program) =
  match p with
  | [] -> (current)
  | (x,y)::l -> 
                if current < x then
                  next_pos pos current l
                else
                    if current > x && x > pos then
                      next_pos pos x l
                    else
                      next_pos pos current l
      

(*Cherche le block d'insctruction la position pos*)
let rec search_block (pos:position) (b:(position * instr)list) =
  match b with
  | [] -> failwith "NO"
  | (x,y)::l ->
    if x = pos then
      y
    else
      search_block pos l


(*Retourne le nombre total d'instruction du programme*)
let rec max_instr (b:block) (max:int)= 
  match b with
  | [] -> max
  | (x,y)::l ->
    if (x > max) then
      max_instr l x
    else
      max_instr l max

let rec max_pos (block) (pos:position) = 
  match block with
  | [] -> pos
  | (x,y)::l ->
    if x > pos then
      max_pos l x
    else 
      max_pos l pos


(*Retourne la position de la plus petite instruction du programme*)
let rec min_instr (b:(position * instr)list) (min)= 
  match b with
  | [] -> min
  | (x,y)::l ->
    if (x <= min) then
      min_instr l x
    else
      min_instr l min

let eval_polish (p:program) : unit = 
let env = NameTable.empty in

(* Transforme une expression en sa valeur finale *)
let rec exprToVal (ex:expr) (envir:int NameTable.t)  =
  match ex with
  | Num(x) -> x

  | Var(x) -> (try NameTable.find x envir; with Not_found -> (print_string ("Variable "^x^" does not exist");
                                                             print_newline();
                                                             exit(1)))
  
  | Op(op,ex1,ex2) -> 
    match op with
    | Add -> (exprToVal ex1 envir) + (exprToVal ex2 envir) 
    | Sub -> (exprToVal ex1 envir) - (exprToVal ex2 envir)
    | Mul -> (exprToVal ex1 envir) * (exprToVal ex2 envir)

    | Div ->  if (exprToVal ex2 envir) = 0 then 
                (print_string "Division par 0";
                exit(1))
              else
                (exprToVal ex1 envir) / (exprToVal ex2 envir)

    | Mod ->  if (exprToVal ex2 envir) = 0 then 
                (print_string "Modulo par 0";
                exit(1))
              else
                (exprToVal ex1 envir) mod (exprToVal ex2 envir) 
in

(* Transforme une condition en sa valeur booléene *)  
let condToBool (condition:cond) (envir: int NameTable.t) = 
  match condition with
  | (ex1, compar, ex2) -> 
                          match compar with
                          | Eq -> (exprToVal ex1 envir) = (exprToVal ex2 envir)
                          | Ne -> (exprToVal ex1 envir) <> (exprToVal ex2 envir)
                          | Lt -> (exprToVal ex1 envir) < (exprToVal ex2 envir)
                          | Le -> (exprToVal ex1 envir) <= (exprToVal ex2 envir)
                          | Gt -> (exprToVal ex1 envir) > (exprToVal ex2 envir)
                          | Ge -> (exprToVal ex1 envir) >= (exprToVal ex2 envir)
in

(* Parcours la liste de (position, instruction) et évalue chaque instruction *)
let rec eval (p:program) (envir: int NameTable.t) (pos: position) =

  (* Evalue l'instruction donnée en paramètre grâce à l'environnement donné *)
  let rec instr_eval (ins:instr) (envir:int NameTable.t) =
  
    match ins with
    | Set(x,y) -> NameTable.add x (exprToVal y envir) envir

    | Read(name) -> (print_string (name ^ "?");
                    let value = read_int_opt() in
                    match value with
                    |None -> instr_eval ins envir
                    | _ -> NameTable.add name (Option.get value) envir )
    
    | If(cond,b1,b2) ->  if (condToBool cond envir) then
                          eval b1 envir (pos + 1)
                        else
                          if b2 != [] then
                            eval b2 envir ((max_instr b1 0) + 1)
                          else
                            envir

    | Print(ex) -> (print_int (exprToVal ex envir);
                    print_newline();
                    envir)

    |While(cond,block) -> if (condToBool cond envir) then
                            let newenvir = eval block envir (pos + 1) in
                            instr_eval (While(cond,block)) newenvir
                          else
                            envir

  in
  
  if (pos <= (max_instr p 0)) then
    let newenvir = instr_eval (search_block pos p) envir in
    eval p newenvir (next_pos pos ((max_instr p 0) +1) p)
  else
    envir
  
in

let finalenv = eval p env (min_instr p (List.length p))  in
print_string "FIN";
print_newline();;