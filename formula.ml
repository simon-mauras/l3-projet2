open Types

module S : Formula_type =
  struct
  
    module LiteralSet = Set.Make(Literal)
  
    (* Number of literal with a true value // Literals which allready have a value // Literals with no value *)
    type t = (int * LiteralSet.t * LiteralSet.t) array
    
    (* Create formula with data from Lexing/Parsing *)
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
      let res = Array.make c (0, LiteralSet.empty, LiteralSet.empty) in
      List.iteri (fun i x -> res.(i) <- (0, LiteralSet.empty,
                                            List.fold_left (fun x y -> LiteralSet.add y x)
                                                           LiteralSet.empty
                                                           (List.map Literal.make x)))
                 clauses;
      res
    
    (* Print formula on output "out" *)
    let print out clauses =
      Array.iter (fun (n,u,v)-> output_string out (string_of_int n);
                                LiteralSet.iter (fun x -> output_string out " "; Literal.print out x) u;
                                output_string out " //";
                                LiteralSet.iter (fun x -> output_string out " "; Literal.print out x) v;
                                output_string out "\n") clauses
    
    let setLiteral x form =
      for i = 0 to (Array.length form) - 1 do
        let n, u, v = form.(i) in
        if LiteralSet.mem x v
          then form.(i) <- (n+1, LiteralSet.add x u, LiteralSet.remove x v);
        if LiteralSet.mem (Literal.neg x) v
          then form.(i) <- (n, LiteralSet.add (Literal.neg x) u, LiteralSet.remove (Literal.neg x) v);
      done
    
    let forgetLiteral x form =
      for i = 0 to (Array.length form) - 1 do
        let n, u, v = form.(i) in
        if LiteralSet.mem x u
          then form.(i) <- (n-1, LiteralSet.remove x u, LiteralSet.add x v);
        if LiteralSet.mem (Literal.neg x) u
          then form.(i) <- (n, LiteralSet.remove (Literal.neg x) u, LiteralSet.add (Literal.neg x) v);
      done
    
    let isFalse form = Array.fold_left (fun b (n,_,u) -> b || (n = 0 && u = LiteralSet.empty)) false form 
    
    let isTrue form = Array.fold_left (fun b (n,_,_) -> b && n > 0) true form
    
    let getUnitClause form = Array.fold_left (fun res (n,_,v) -> if n = 0 && LiteralSet.cardinal v = 1
                                                                   then Some (LiteralSet.min_elt v)
                                                                   else res) None form
    
    let getPureLiteral form = None
    
    let getFreeLiteral form = Array.fold_left (fun res (_,_,v) -> if LiteralSet.is_empty v
                                                                    then res
                                                                    else Some (LiteralSet.min_elt v)) None form
     
  end
