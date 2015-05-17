(** Module principal, appele au debut de l'execution *)

open Sigs

(** Message affiches par le parser de la ligne de commande *)
let usage_msg = "Usage: ./resol <options> input_file <output_file>"
let version = "SAT-solver v1. Remy Grunblatt & Simon Mauras"

(** Arguments (ligne de commande) *)
type heuristic = Dlis_heuristic | Moms_heuristic | Rand_heuristic | Vsids_heuristic
type mode = Cnf_mode | Tseitin_mode | Equality_mode | Congruence_mode | Difference_mode
let arg_heuristic = ref Rand_heuristic
let arg_mode = ref Cnf_mode
let arg_debug = ref false
let arg_wl = ref false
let arg_cl = ref false
let arg_clinterac = ref false
let arg_input = ref ""
let arg_output = ref ""

(** Doc pour le parser de la ligne de commande *)
let doc = [("-wl", Arg.Set arg_wl, "Use watched literals to compute satisfiability");
           ("-cl", Arg.Set arg_cl, "Use clause learning");
           ("-cl-interac", Arg.Set arg_clinterac, "Use interactive mode for clauses learning");
           ("-debug", Arg.Set arg_debug, "Print debug informations");
           ("-cnf", Arg.Unit (fun () -> arg_mode := Cnf_mode), "Use cnf mode");
           ("-tseitin", Arg.Unit (fun () -> arg_mode := Tseitin_mode), "Use tseitin mode");
           ("-equality", Arg.Unit (fun () -> arg_mode := Equality_mode), "Use equality mode");
           ("-congruence", Arg.Unit (fun () -> arg_mode := Congruence_mode), "Use congruence mode");
           ("-difference", Arg.Unit (fun () -> arg_mode := Difference_mode), "Use difference mode");
           ("-rand", Arg.Unit (fun () -> arg_heuristic := Rand_heuristic), "Use rand heuristic");
           ("-vsids", Arg.Unit (fun () -> arg_heuristic := Vsids_heuristic), "Use vsids heuristic");
           ("-moms", Arg.Unit (fun () -> arg_heuristic := Moms_heuristic), "Use moms heuristic");
           ("-dlis", Arg.Unit (fun () -> arg_heuristic := Dlis_heuristic), "Use dlis heuristic");
           ("-version", Arg.Unit (fun () -> print_endline version; exit 0), "Print version and exit")]

(** Fonction appelee par le parser de la ligne de commande *)
let add_file s =
  if !arg_input = ""
  then arg_input := s
  else if !arg_output = ""
  then arg_output := s
  else (prerr_string "Warning: File '"; prerr_string s; prerr_string "' ignored.\n")

(** Type d'un module permettant d'initialiser une theorie *)
module type Mode_type =
  sig
    module Theory : Theory_type
    val parse : Lexing.lexbuf -> cnf * Theory.T.t option array
    val print_solution : out_channel -> certificate -> Theory.T.t option array -> unit
  end

(** Permettant d'initialiser la theorie par defaut (donnees deja sous forme CNF) *)
module Mode_cnf : Mode_type =
  struct
    module Theory = Theory_default.Make
    let parse lexbuf = (Checker.check stderr (Parser.formula Lexer.main lexbuf), Array.make 0 None)
    let print_solution output l tab =
      List.iter (Printf.fprintf output "%d ") l;
      Printf.fprintf output "0\n"
  end

(** Permettant d'initialiser la theorie par defaut *)
module Mode_tseitin : Mode_type =
  struct
    module Theory = Theory_default.Make
    module Tseitin = Tseitin.Make(Theory.T)
    let parse lexbuf = Tseitin.make (Theory.T.make (Parser_tseitin.main Lexer_tseitin.main lexbuf))
    let print_solution output l tab =
      List.iter (fun x ->
                 let i = abs x in
                 let value = if x > 0 then "true" else "false" in
                 match tab.(i) with
                 | None -> ()
                 | Some s -> Theory.T.print output s;
                             Printf.fprintf output " = %s\n" value) l;
  end

(** Permettant d'initialiser la theorie de l'egalite *)
module Mode_equality : Mode_type =
  struct
    module Theory = Theory_equality.Make
    module Tseitin = Tseitin.Make(Theory.T)
    let parse lexbuf = Tseitin.make (Theory.T.make (Parser_equality.main Lexer_equality.main lexbuf))
    let print_solution output l tab =
      List.iter (fun x ->
                 let i = abs x in
                 let value = if x > 0 then "true" else "false" in
                 match tab.(i) with
                 | None -> ()
                 | Some s -> Printf.fprintf output "(";
                             Theory.T.print output s;
                             Printf.fprintf output ") = %s\n" value) l;
  end

(** Permettant d'initialiser la theorie de la congruence *)
module Mode_congruence : Mode_type =
  struct
    module Theory = Theory_congruence.Make
    module Tseitin = Tseitin.Make(Theory.T)
    let parse lexbuf = Tseitin.make (Theory.T.make (Parser_congruence.main Lexer_congruence.main lexbuf))
    let print_solution output l tab =
      List.iter (fun x ->
                 let i = abs x in
                 let value = if x > 0 then "true" else "false" in
                 match tab.(i) with
                 | None -> ()
                 | Some s -> Printf.fprintf output "(";
                             Theory.T.print output s;
                             Printf.fprintf output ") = %s\n" value) l;
  end
  
(** Permettant d'initialiser la theorie de la difference *)
module Mode_difference : Mode_type =
  struct
    module Theory = Theory_difference.Make
    module Tseitin = Tseitin.Make(Theory.T)
    let parse lexbuf = Tseitin.make (Theory.T.make (Parser_difference.main Lexer_difference.main lexbuf))
    let print_solution output l tab =
      List.iter (fun x ->
                 let i = abs x in
                 let value = if x > 0 then "true" else "false" in
                 match tab.(i) with
                 | None -> ()
                 | Some s -> Printf.fprintf output "(";
                             Theory.T.print output s;
                             Printf.fprintf output ") = %s\n" value) l;
  end

(** Functor permettant de lancer l'execution dans differents modes suivant les modules fournis  *)
module Main =
  functor (H : Heuristic_type) ->
  functor (F : Formula_type) ->
  functor (M : Mode_type) ->
  struct
    module Solver = Dpll.Make (H) (F) (M.Theory)
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

(** Cree une instance du module Main.Main en choisissant les parametres appropries *)
let run heuristic mode wl =
  let aux (module H : Heuristic_type) (module F : Formula_type) (module M : Mode_type) =
  let module Main = Main (H) (F) (M) in Main.main in
  let aux_mode (module H : Heuristic_type) (module F : Formula_type) = match mode with
  | Cnf_mode -> aux (module H) (module F) (module Mode_cnf)
  | Tseitin_mode -> aux (module H) (module F) (module Mode_tseitin)
  | Equality_mode -> aux (module H) (module F) (module Mode_equality) 
  | Congruence_mode -> aux (module H) (module F) (module Mode_congruence)
  | Difference_mode -> aux (module H) (module F) (module Mode_difference) in
  let aux_wl (module H : Heuristic_type) = match wl with
  | true -> aux_mode (module H) (module Formula_wl.Make)
  | false -> aux_mode (module H) (module Formula.Make) in
  let aux_heuristic () = match heuristic with
  | Rand_heuristic -> aux_wl (module Heuristic_rand.Make)
  | Moms_heuristic -> aux_wl (module Heuristic_moms.Make)
  | Dlis_heuristic -> aux_wl (module Heuristic_dlis.Make)
  | Vsids_heuristic -> aux_wl (module Heuristic_vsids.Make) in
  aux_heuristic ()

(** Fonction principale *)
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
      (run !arg_heuristic !arg_mode !arg_wl) input output
    with
    | Sys_error s -> prerr_endline s (* no such file or directory, ... *)
  end

(** Executer main *)
let _ = main ()
