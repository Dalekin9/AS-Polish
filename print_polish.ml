open Syntaxe

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
let rec min_instr (b:(position * instr)list) (min)= 
  match b with
  | [] -> min
  | (x,y)::l ->
    if (x <= min) then
      min_instr l x
    else
      min_instr l min

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

let rec print_block (block) (indent:int) (pos) : unit= 

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

        let print_polish (p:(position * instr) list) =  
          let min = min_instr p (List.length p) in
          print_block p 0 min