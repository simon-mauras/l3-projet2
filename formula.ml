open Types

module S : Formula_type =
  struct
  
    module LiteralSet = Set.Make(Literal)
  
    (* A literal can be True, False or Undefined.*)
    type literal_state = True | False | Undefined
    
    (* Number of literal with a true value // Literals which allready have a value // Literals with no value *)
    type t = (int * LiteralSet.t * LiteralSet.t) array * (literal_state * int * int list) array
    
    (* Create formula with data from Lexing/Parsing *)
    let make out (nb_vars, nb_clauses, clauses) =
      (* Check: number of variables *)
      let max0 x = abs x in
      let rec max1 = function
        | [] -> 0 | x::l -> max (max0 x) (max1 l) in
      let rec max2 = function
        | [] -> 0 | x::l -> max (max1 x) (max2 l) in
      let max_vars = max2 clauses in
      let v = max max_vars nb_vars in
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
        output_string out (string_of_int c);
        output_string out " found, ";
        output_string out (string_of_int nb_clauses);
        output_string out " expected).\n";
      end;
      (* Return *)
      let tabLiterals = Array.make (2*v) (Undefined, 0, []) in
      let tabClauses = Array.make c (0, LiteralSet.empty, LiteralSet.empty) in
      List.iteri (fun i x -> tabClauses.(i) <- (0, LiteralSet.empty,
                                                   List.fold_left (fun x y -> let id = Literal.id_of_literal y in
                                                                              let _,nb,l = tabLiterals.(id) in
                                                                              tabLiterals.(id) <- (Undefined, nb+1, i::l);
                                                                              LiteralSet.add y x)
                                                                  LiteralSet.empty
                                                                  (List.map Literal.make x)))
                 clauses;
      (tabClauses, tabLiterals)
    
    (* Print formula on output "out" *)
    let print out (tabClauses, tabLiterals) =
      Array.iter (fun (n,u,v)-> output_string out (string_of_int n);
                                LiteralSet.iter (fun x -> output_string out " "; Literal.print out x) u;
                                output_string out " //";
                                LiteralSet.iter (fun x -> output_string out " "; Literal.print out x) v;
                                output_string out "\n") tabClauses
    
    let setLiteral x (tabClauses, tabLiterals) =
      let decr z =
        let state, nb, lst = tabLiterals.(Literal.id_of_literal z) in
        tabLiterals.(Literal.id_of_literal z) <- (state, nb-1, lst) in
      let _, nb1, lst1 = tabLiterals.(Literal.id_of_literal x) in
      let _, nb2, lst2 = tabLiterals.(Literal.id_of_literal (Literal.neg x)) in
      tabLiterals.(Literal.id_of_literal x) <- (True, nb1, lst1);
      tabLiterals.(Literal.id_of_literal (Literal.neg x)) <- (False, nb2, lst2);
      List.iter (fun i -> let n, u, v = tabClauses.(i) in
                          tabClauses.(i) <- (n+1, LiteralSet.add x u, LiteralSet.remove x v);
                          if n = 0
                            then LiteralSet.iter decr v
                            else decr x) lst1;
      List.iter (fun i -> let n, u, v = tabClauses.(i) in
                          tabClauses.(i) <- (n, LiteralSet.add (Literal.neg x) u, LiteralSet.remove (Literal.neg x) v);
                          decr (Literal.neg x)) lst2
    
    let forgetLiteral x (tabClauses, tabLiterals) =
      let incr z =
        let state, nb, lst = tabLiterals.(Literal.id_of_literal z) in
        tabLiterals.(Literal.id_of_literal z) <- (state, nb+1, lst) in
      let _, nb1, lst1 = tabLiterals.(Literal.id_of_literal x) in
      let _, nb2, lst2 = tabLiterals.(Literal.id_of_literal (Literal.neg x)) in
      tabLiterals.(Literal.id_of_literal x) <- (Undefined, nb1, lst1);
      tabLiterals.(Literal.id_of_literal (Literal.neg x)) <- (Undefined, nb2, lst2);
      List.iter (fun i -> let n, u, v = tabClauses.(i) in
                          tabClauses.(i) <- (n-1, LiteralSet.remove x u, LiteralSet.add x v);
                          if n = 1 then LiteralSet.iter incr v;
                          incr x) lst1;
      List.iter (fun i -> let n, u, v = tabClauses.(i) in
                          tabClauses.(i) <- (n, LiteralSet.remove (Literal.neg x) u, LiteralSet.add (Literal.neg x) v);
                          incr (Literal.neg x)) lst2
    
    let isFalse (tabClauses,_) =
      Array.fold_left (fun b (n,_,u) -> b || (n = 0 && u = LiteralSet.empty)) false tabClauses 
    
    let isTrue (tabClauses,_) =
      Array.fold_left (fun b (n,_,_) -> b && n > 0) true tabClauses
    
    let getUnitClause (tabClauses,_) =
      Array.fold_left (fun res (n,_,v) -> if n = 0 && LiteralSet.cardinal v = 1
                                            then Some (LiteralSet.min_elt v)
                                            else res) None tabClauses
    
    let getPureLiteral (_,tabLiterals) =
      let ind = ref (-1) in
      for i = 0 to (Array.length tabLiterals) - 1 do
        let state, n, lst = tabLiterals.(i) in
        if (state, n) = (Undefined, 0)
          then ind := i;
      done;
      if !ind <> -1
        then Some (Literal.neg (Literal.literal_of_id !ind))
        else None
    
    let getFreeLiteral (_, tabLiterals) =
      let value = ref min_int in
      let index = ref 0 in
      for i = 0 to (Array.length tabLiterals) - 1 do
        let state,v,_ = tabLiterals.(i) in
        if v > !value && state = Undefined then begin
          value := v;
          index := i;
        end;
      done;
      if !value <> min_int
        then Some(Literal.literal_of_id !index)
        else None
  end
