type literal = X of int | Xbar of int
type clause = literal list
type formula = Cnf of int * int * clause list

let (print_literal : literal -> unit)  = function
  | X x    -> print_string "X";
              print_int x
  | Xbar x -> print_string "not X";
              print_int x

let rec (print_clause : clause -> unit) = function
  | []   -> ()
  | [l]  -> print_literal l
  | l::c -> print_literal l;
            print_string " | ";
            print_clause c

let print_formula = function
  | Cnf(v, c, l) ->
    let rec print_cnf = function
      | []   -> ()
      | [c]  -> print_string "(";
                print_clause c;
                print_string ")\n"
      | c::f -> print_string "(";
                print_clause c;
                print_string ") &\n";
                print_cnf f in
    print_cnf l

let check_formula = function
  | Cnf(v, c, l) ->
    let max_var_literal = function
      | X x | Xbar x -> x in
    let rec max_var_clause = function
      | [] -> 0
      | l::c -> max (max_var_clause c) (max_var_literal l) in
    let rec max_var_cnf = function
      | [] -> 0
      | c::f -> max (max_var_cnf f) (max_var_clause c) in
    let max_var = max_var_cnf l in
    if max_var > v then begin
      prerr_string "Warning: variable ids are expected to be between 1 and ";
      prerr_int v;
      prerr_string " (X";
      prerr_int max_var;
      prerr_string " found).\n";
    end;
    let nb_clauses = List.length l in
    if c <> nb_clauses then begin
      prerr_string "Warning: wrong number of clauses (";
      prerr_int nb_clauses;
      prerr_string " found, ";
      prerr_int c;
      prerr_string " expected).\n";
    end;
    Cnf(max max_var v, nb_clauses, l)
