open Types

module S : Formula_type =
  struct
    type t = Literal.t list list ref
    
    let make out (nb_vars, nb_clauses, clauses) =
      (* Check: number of variables *)
      let max0 x = abs x in
      let rec max1 = function
        | [] -> 0 | x::l -> max (max0 x) (max1 l) in
      let rec max2 = function
        | [] -> 0 | x::l -> max (max1 x) (max2 l) in
      let max_vars = max2 clauses in
      if max_vars > nb_vars then begin
        output_string out "Warning: variable ids are expected to be between 1 and ";
        output_string out (string_of_int nb_vars);
        output_string out " (X";
        output_string out (string_of_int max_vars);
        output_string out " found).\n";
      end;
      (* Check: number of clauses *)
      let c = List.length clauses in
      if c <> nb_clauses then begin
        output_string out "Warning: wrong number of clauses (";
        output_string out (string_of_int nb_clauses);
        output_string out " found, ";
        output_string out (string_of_int c);
        output_string out " expected).\n";
      end;
      (* Return *)
      ref (List.map (List.map Literal.make) clauses)
    
    let print out clauses =
      let rec print_clause = function
        | [] -> ()
        | x::[] -> Literal.print out x;
        | x::l -> Literal.print out x; output_string out " & "; print_clause l in
      List.iter (fun l -> print_clause l; output_string out "\n") !clauses
    
    let setLiteral x clauses = ()
    let forgetLiteral x clauses = ()
    
    let isFalse clauses = false
    let getUnitClause clauses = None
    let getPureLiteral clauses = None
    let getFreeLiteral clauses = None
     
  end
