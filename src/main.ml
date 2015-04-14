module Tseitin_tseitin = Tseitin.Make(String)
module Tseitin_equality = Tseitin.Make(Sigs.Equality)
module Solver = Dpll.Make (Literal2.Make) (Formula.Make) (Theory_default.Make)
module Solver_wl = Dpll.Make (Literal2.Make) (Formula_wl.Make) (Theory_default.Make)

(* Message affichés par le parser de la ligne de commande *)
let usage_msg = "Usage: ./resol <options> input_file <output_file>"
let version = "SAT-solver v1. Remy Grunblatt & Simon Mauras"

(* Arguments (ligne de commande *)
type mode = Cnf_mode | Tseitin_mode | Equality_mode | Congruence_mode | Difference_mode
let arg_mode = ref Cnf_mode
let arg_debug = ref false
let arg_wl = ref false
let arg_cl = ref false
let arg_clinterac = ref false
let arg_input = ref ""
let arg_output = ref ""

(* Doc pour le parser de la ligne de commande *)
let doc = [("-wl", Arg.Set arg_wl, "Use watched literals to compute satisfiability");
           ("-cl", Arg.Set arg_cl, "Use clause learning");
           ("-cl-interac", Arg.Set arg_clinterac, "Use interactive mode for clauses learning");
           ("-debug", Arg.Set arg_debug, "Print debug informations");
           ("-cnf", Arg.Unit (fun () -> arg_mode := Cnf_mode), "Use cnf mode");
           ("-tseitin", Arg.Unit (fun () -> arg_mode := Tseitin_mode), "Use tseitin mode");
           ("-equality", Arg.Unit (fun () -> arg_mode := Equality_mode), "Use equality mode");
           ("-congruence", Arg.Unit (fun () -> arg_mode := Congruence_mode), "Use congruence mode");
           ("-difference", Arg.Unit (fun () -> arg_mode := Difference_mode), "Use difference mode");
           ("-version", Arg.Unit (fun () -> print_endline version; exit 0), "Print version and exit")]

(* Fonction appelée par le parser de la ligne de commande *)
let add_file s =
  if !arg_input = ""
  then arg_input := s
  else if !arg_output = ""
  then arg_output := s
  else (prerr_string "Warning: File '"; prerr_string s; prerr_string "' ignored.\n")


let affiche formula tab =
let rec affiche = function
  | Sigs.And(a, b) -> Printf.printf "("; affiche a; Printf.printf " AND "; affiche b; Printf.printf ")";
  | Sigs.Or(a, b) -> Printf.printf "("; affiche a; Printf.printf " OR "; affiche b; Printf.printf ")";
  | Sigs.Imp(a, b) -> Printf.printf "("; affiche a; Printf.printf " => "; affiche b; Printf.printf ")";
  | Sigs.Not(a) -> Printf.printf "NOT ("; affiche a; Printf.printf ")";
  | Sigs.Atom(a) -> Array.iteri (fun i x -> if x = Some a then Printf.printf "[%d]" i) tab in
  affiche formula

(* Parse l'entrée *)
let parse_cnf lexbuf = Checker.check stderr (Parser.formula Lexer.main lexbuf)
let parse_tseitin lexbuf = Parser_tseitin.main Lexer_tseitin.main lexbuf
let parse_equality lexbuf = Parser_equality.main Lexer_equality.main lexbuf
let parse_congruence lexbuf = Parser_congruence.main Lexer_congruence.main lexbuf
let parse_difference lexbuf = Parser_difference.main Lexer_difference.main lexbuf

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
      let data, tab = match !arg_mode with
        | Cnf_mode -> (parse_cnf lexbuf, Array.make 0 None)
        | Tseitin_mode -> Tseitin_tseitin.make (parse_tseitin lexbuf)
        (*| Equality_mode -> Tseitin_equality.make (parse_equality lexbuf)*)
        (*| Congruence_mode -> Tseitin_congruence.make (parse_congruence lexbuf)*)
        (*| Difference_mode -> Tseitin_difference.make (parse_difference lexbuf)*)
        | _ -> failwith "Not implemented yet !" in
      
      let s = if !arg_wl then (
          Solver_wl.setDebug !arg_debug;
          Solver_wl.setClauseLearning !arg_cl;
          Solver_wl.setClauseLearningInteractive !arg_clinterac;
          Solver_wl.solve data tab)
        else (
          Solver.setDebug !arg_debug;
          Solver.setClauseLearning !arg_cl;
          Solver.setClauseLearningInteractive !arg_clinterac;
          Solver.solve data tab) in

      match s with
      | None -> output_string output "s UNSATISFIABLE\n"
      | Some l -> output_string output "s SATISFIABLE\n";
        List.iter (Printf.fprintf output "%d ") l;
        Printf.fprintf output "0\n"
    with
    | Sys_error s -> prerr_endline s (* no such file or directory, ... *)
  end

(* Executer main *)
let _ = main ()
