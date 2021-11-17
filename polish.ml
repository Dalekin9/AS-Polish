(** Projet Polish -- Analyse statique d'un mini-langage impératif *)
let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> 
    let pro = Read.read_polish file in 
    Print.print_polish pro
  | [|_;"-eval";file|] -> 
    let pro = Read.read_polish file in 
    Eval.eval_polish pro
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
