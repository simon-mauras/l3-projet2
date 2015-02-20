module Formula = Formula.Make (Literal.Make)
module Solver = Dpll.Make(Formula)

let usage_msg = "Usage: ./resol <options> input_file <output_file>"
let version = "SAT-solver v0.1"

let arg_opt = ref false
let arg_debug = ref false
let arg_input = ref ""
let arg_output = ref ""

let doc = [("-opt", Arg.Set arg_opt, "Use optimisations to compute satisfiability");
           ("-debug", Arg.Set arg_debug, "Print debug informations");
           ("-version", Arg.Unit (fun () -> print_endline version; exit 0), "Print version and exit")]

let add_file s =
  if !arg_input = ""
    then arg_input := s
  else if !arg_output = ""
    then arg_output := s
    else (prerr_string "Warning: File '"; prerr_string s; prerr_string "' ignored.\n") 

let parse lexbuf = Parser.formula Lexer.main lexbuf

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
        let form = Formula.make stderr (parse lexbuf) in
        let s = Solver.solve stderr form in
        match s with
          | None -> output_string output "s UNSATISFIABLE\n"
          | Some l -> output_string output "s SATISFIABLE\n"
      with
        | Sys_error s -> prerr_endline s (* no such file or directory, ... *)
    end
 
  let _ = main ()
