(* module Make : functor (L : Sigs.Literal_type) -> Sigs.Formula_type *)
module Make (L: Sigs.Literal_type) : Sigs.Formula_type =
struct

  module Literal = L
  module LiteralSet = Set.Make(Literal)

  (* A literal can be True, False or Undefined.*)
  type literal_state = True | False | Undefined

  (* The first array contains the clauses:
         The first element is the number of literals that makes one clause true.
         The second element is a set containing the literals that appear in the
         clause which already have a value (True or False).
         The third element is a set containing the literals that appear in the
         clause which are Undefined.
     The second array contains the literals:
        The first element is the state of a literal.
        The second element is the number of unsatisfied clauses in which a
        the literal appears.
        The third element is a list of the indices of the clauses containing
        the literal.
  *)
  type t = (int * LiteralSet.t * LiteralSet.t) array * (literal_state * int * int list) array

  (* Create formula with data from Lexing/Parsing *)
  let make out (nb_vars, nb_clauses, clauses) =
    let tabLiterals = Array.make (2*nb_vars) (Undefined, 0, []) in
    let tabClauses = Array.make nb_clauses (0, LiteralSet.empty, LiteralSet.empty) in
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

