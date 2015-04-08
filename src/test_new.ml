open Sigs
(* Message affichés par le parser de la ligne de commande *)
let usage_msg = "Usage: ./resol <options> input_file <output_file>"
let version = "SAT-solver v1. Remy Grunblatt & Simon Mauras"

(* Arguments (ligne de commande *)
let arg_debug = ref false
let arg_wl = ref false
let arg_cl = ref false
let arg_clinterac = ref false
let arg_input = ref ""
let arg_output = ref ""

let doc = [("-wl", Arg.Set arg_wl, "Use watched literals to compute satisfiability");
           ("-cl", Arg.Set arg_cl, "Use clause learning");
           ("-cl-interac", Arg.Set arg_clinterac, "Use interactive mode for clauses learning");
           ("-debug", Arg.Set arg_debug, "Print debug informations");
           ("-version", Arg.Unit (fun () -> print_endline version; exit 0), "Print version and exit")]

(* Fonction appelée par le parser de la ligne de commande *)
let add_file s =
  if !arg_input = ""
  then arg_input := s
  else if !arg_output = ""
  then arg_output := s
  else (prerr_string "Warning: File '"; prerr_string s; prerr_string "' ignored.\n") 

(* Parse l'entrée et renvoie une valeur de type Sigs.cnf *)
let parse lexbuf = Parser1.shell Lexer1.read lexbuf

let rec print_chars = function
  |[]->();
  |t::q->Printf.printf "%c" t; print_chars q;;

(* Liste les "labels" (les variables) avec doublons
   Par la suite, on élimine les doublons et on assigne à chaque nom de variable un numéro "cnf"
   (genre -1 ou +1, etc…) *)
let rec list_label formula l =
  match formula with
  |Atom(a) -> a::l
  |And(a, b) ->
    let l2 = list_label a l in
    list_label b l2
  |Or(a, b) ->
    let l2 = list_label a l in
    list_label b l2
  |Imp(a, b)    ->
    let l2 = list_label a l in
    list_label b l2
  |Not(a) -> list_label a l;;

(* Méthode rapide pour éliminer les doublons *)
let rec assign_var_label l =
  let ul = List.sort_uniq compare l in
  List.mapi (fun i elt -> (i+1, elt)) ul;;
(* Création par transformations de tseitin de notre clause de type cnf
   type cnf = int * int * clause list
*)

let rec label_to_var label labels=
  let (_, i) = List.find (fun x -> x=label) labels in
  i;;

let rec printt formula =
  match formula with
  |And(a, b) -> Printf.printf "("; printt a; Printf.printf " AND "; printt b; Printf.printf ")";
  |Or(a, b) -> Printf.printf "("; printt a; Printf.printf " OR "; printt b; Printf.printf ")";
  |Imp(a, b) -> Printf.printf "("; printt a; Printf.printf " => "; printt b; Printf.printf ")";
  |Not(a) -> Printf.printf " NOT ("; printt a; Printf.printf ")";
  |Atom(a) -> print_chars a;;

(* Fonction principale *)
let main () =
  Arg.parse doc add_file usage_msg;
  if !arg_input = ""
  then (prerr_string "No input file provided\n"; Arg.usage doc usage_msg)
  else begin
    try
      let input = open_in !arg_input in
      let output = if !arg_output <> ""
        then open_out !arg_output
        else stdout in

      let lexbuf = Lexing.from_channel input in
      let data = (parse lexbuf) in
      match data with
      |None -> Printf.printf "None\n";
      |Some v -> (printt v;)
    with
    | Sys_error s -> prerr_endline s (* no such file or directory, ... *)
  end

(* Executer main *)
let _ = main ()
