
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

let read_polish (filename:string) : program = failwith "TODO"


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
let print_polish (p:program) =   
  print_block p 0

let eval_polish (p:program) : unit = failwith "TODO"

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> print_polish (read_polish file)
  | [|_;"--eval";file|] -> eval_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
