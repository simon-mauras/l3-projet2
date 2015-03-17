module Formula = Formula.Make (Literal2.Make)
module Formula_wl = Formula_wl.Make (Literal2.Make)
module Solver = Dpll.Make(Formula)
module Solver_wl = Dpll.Make(Formula_wl)

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

(* Doc pour le parser de la ligne de commande *)
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
let parse lexbuf = Parser.formula Lexer.main lexbuf

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
      let data = Checker.check stderr (parse lexbuf) in

      let s = if !arg_wl then (
          let form = Formula_wl.make stderr data in
          Solver_wl.setDebug !arg_debug;
          Solver_wl.setClauseLearning !arg_cl;
          Solver_wl.setClauseLearningInteractive !arg_clinterac;
          Solver_wl.solve form)
        else (
          let form = Formula.make stderr data in
          Solver.setDebug !arg_debug;
          Solver.setClauseLearning !arg_cl;
          Solver.setClauseLearningInteractive !arg_clinterac;
          Solver.solve form) in

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
