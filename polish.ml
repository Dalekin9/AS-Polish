
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

module NameTable = Map.Make(String)

(***********************************************************************)

let read_polish (filename:string) : program = failwith "TODO"

let print_polish (p:program) : unit = failwith "TODO"

let eval_polish (p:program) : unit = 
  let env = NameTable.empty in
  
  (* Transforme une expression en sa valeur finale *)
  let rec exprToVal (ex:expr) (envir:int NameTable.t)  =
    match ex with
    | Num(x) -> x

    | Var(x) -> NameTable.find x envir
    
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
  let rec eval (p:program) (envir: int NameTable.t) =

    (* Evalue l'instruction donnée en paramètre grâce à l'environnement donné *)
    let instr_eval (ins:instr) (envir:int NameTable.t) =
    match ins with
    | Set(x,y) -> NameTable.add x (exprToVal y envir) envir

    | Read(name) -> NameTable.add name (read_int()) envir
    
    | If(cond,b1,b2) -> if (condToBool cond envir) then
                          eval b1 envir
                        else
                          eval b2 envir

    | Print(ex) -> (print_int (exprToVal ex envir);
                    envir)

    |While(cond,block) -> if (condToBool cond envir) then
                            eval block envir
                          else
                            envir
    in
    
    match p with
    |[] -> print_string "FIN"
    |[(pos,ins)] -> let envir = instr_eval ins envir in
                    eval [] envir 
    |(pos,ins)::l -> let envir = instr_eval ins envir in
                      eval l envir 
  in

  eval p env

;;

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
