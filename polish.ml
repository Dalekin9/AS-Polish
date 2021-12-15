open Read
open Print
open Eval
let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "Entrez une ligne de commande valide.\n"

let main () =
  match Sys.argv with

  | [|_;"-reprint";file|] -> 
    let pro = read_polish file in 
    print_polish pro
  | [|_;"-eval";file|] -> 
    let pro = read_polish file in 
    eval_polish pro
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
