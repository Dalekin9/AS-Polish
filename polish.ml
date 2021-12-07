open Syntaxe

(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

let eval_polish (p:program) : unit = failwith "TODO"

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> 
    let pro = Read.read_polish file in 
    let p = (0, While( (Num(4),Eq,Num(5)),( (1, Print(Num(7)))::(2, Read("b"))::(3, Read("d"))::[] )))::(4, Read("g"))::[] in
    Print.print_polish pro
  | [|_;"--eval";file|] -> 
    let pro = Read.read_polish file in 
    eval_polish pro
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
