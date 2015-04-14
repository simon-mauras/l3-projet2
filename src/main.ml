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

module type Mode_type =
  sig
    module T : Map.OrderedType
    module Theory : Sigs.Theory_type
    val parse : Lexing.lexbuf -> Sigs.cnf * T.t option array
    val print_solution : out_channel -> int list -> T.t option array -> unit
  end

module Mode_cnf =
  struct
    module T = String
    module Theory = Theory_default.Make
    let parse lexbuf = (Checker.check stderr (Parser.formula Lexer.main lexbuf), Array.make 0 None)
    let print_solution output l tab =
      List.iter (Printf.fprintf output "%d ") l;
      Printf.fprintf output "0\n"
  end

module Mode_tseitin =
  struct
    module T = String
    module Theory = Theory_default.Make
    module Tseitin = Tseitin.Make(T)
    let parse lexbuf = Tseitin.make (Parser_tseitin.main Lexer_tseitin.main lexbuf)
    let print_solution output l tab =
      List.iter (fun x ->
                 let i = abs x in
                 let value = if x > 0 then "true" else "false" in
                 match tab.(i) with
                 | None -> ()
                 | Some s -> Printf.fprintf output "%s = %s\n" s value) l;
  end

module Mode_equality =
  struct
    module T = Sigs.Equality
    module Theory = Theory_default.Make
    module Tseitin = Tseitin.Make(T)
    let parse lexbuf = Tseitin.make (Parser_equality.main Lexer_equality.main lexbuf)
    let print_solution output l tab =
      List.iter (Printf.fprintf output "%d ") l;
      Printf.fprintf output "0\n"
  end
  
module Mode_congruence =
  struct
    module T = Sigs.Congruence
    module Theory = Theory_default.Make
    module Tseitin = Tseitin.Make(T)
    let parse lexbuf = Tseitin.make (Parser_congruence.main Lexer_congruence.main lexbuf)
    let print_solution output l tab =
      List.iter (Printf.fprintf output "%d ") l;
      Printf.fprintf output "0\n"
  end
  
module Mode_difference =
  struct
    module T = Sigs.Difference
    module Theory = Theory_default.Make
    module Tseitin = Tseitin.Make(T)
    let parse lexbuf = Tseitin.make (Parser_difference.main Lexer_difference.main lexbuf)
    let print_solution output l tab =
      List.iter (Printf.fprintf output "%d ") l;
      Printf.fprintf output "0\n"
  end

module Main =
  functor (F : Sigs.Formula_type) ->
  functor (M : Mode_type) ->
  struct
    module Solver = Dpll.Make (Literal2.Make) (F) (M.Theory)
    let main input output =
      let lexbuf = Lexing.from_channel input in
      let data, tab = M.parse lexbuf in
      Solver.setDebug !arg_debug;
      Solver.setClauseLearning !arg_cl;
      Solver.setClauseLearningInteractive !arg_clinterac;
      match Solver.solve data tab with
      | None -> output_string output "s UNSATISFIABLE\n"
      | Some l -> output_string output "s SATISFIABLE\n";
                  M.print_solution output l tab
  end

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
      match !arg_wl, !arg_mode with
      | true, Cnf_mode         -> let module M = Main (Formula_wl.Make) (Mode_cnf) in M.main input output
      | true, Tseitin_mode     -> let module M = Main (Formula_wl.Make) (Mode_tseitin) in M.main input output
      | true, Equality_mode    -> let module M = Main (Formula_wl.Make) (Mode_equality) in M.main input output
      | true, Congruence_mode  -> let module M = Main (Formula_wl.Make) (Mode_congruence) in M.main input output
      | true, Difference_mode  -> let module M = Main (Formula_wl.Make) (Mode_difference) in M.main input output
      | false, Cnf_mode        -> let module M = Main (Formula.Make) (Mode_cnf) in M.main input output
      | false, Tseitin_mode    -> let module M = Main (Formula.Make) (Mode_tseitin) in M.main input output
      | false, Equality_mode   -> let module M = Main (Formula.Make) (Mode_equality) in M.main input output
      | false, Congruence_mode -> let module M = Main (Formula.Make) (Mode_congruence) in M.main input output
      | false, Difference_mode -> let module M = Main (Formula.Make) (Mode_difference) in M.main input output
    with
    | Sys_error s -> prerr_endline s (* no such file or directory, ... *)
  end

(* Executer main *)
let _ = main ()
