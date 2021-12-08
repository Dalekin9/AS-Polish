open Syntaxe

let simpl_polish (p:program) : unit = failwith "TODO"

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> 
    let pro = Read.read_polish file in 
    let pp = (0 , If ((Var("x"),Eq, Num(5)), 
                ((1, Print(Num(7)))::(2, Print(Num(3)))::[]),
                ((4, Read("n"))::[]) ))
            ::(5, Print(Num(3)))::[] in
    let p = (0, While( (Num(4),Eq,Num(5)),( (1, Print(Num(7)))::(2, Read("b"))::(3, Read("d"))::[] )))::(4, Read("g"))::[] in
    Print.print_polish pro
  | [|_;"--eval";file|] -> 
    let pro = Read.read_polish file in 
    failwith "eval"
  | [|_;"--simpl";file|] -> Print.print_polish (Simpl.simpl_polish (Read.read_polish file))
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
