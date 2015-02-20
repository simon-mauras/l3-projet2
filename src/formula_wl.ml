(* module Make : functor (L : Sigs.Literal) -> Sigs.Formula_type *)
module Make (L: Sigs.Literal) : Sigs.Formula_type =
struct

  module Literal = L
  
  type clausesArray = (Literal.t list * Literal.t option * Literal.t option) array
  type watchedLiteralsArray = int list array
  type literalsArray = bool array
  type requestAnswer = {
    mutable unitClauses : Literal.t list;
    mutable pureLiterals : Literal.t list;
    mutable freeLiterals : Literal.t list;
    mutable isFalse : bool
  }

  type t = clausesArray * watchedLiteralsArray * literalsArray * requestAnswer

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
    let nbOccur = Array.make (2*v) 0 in
    let rec literalList = function
      | 0 -> []
      | n -> (Literal.make n)::(literalList (n-1)) in
    let compare x y =
      let vx = nbOccur.(Literal.id_of_literal x) + nbOccur.(Literal.id_of_literal (Literal.neg x)) in
      let vy = nbOccur.(Literal.id_of_literal y) + nbOccur.(Literal.id_of_literal (Literal.neg y)) in
      vy - vx in
    
    let watchedLiterals = Array.make (2*v) [] in
    
    let tabLiterals = Array.make (2*v) false in
    
    let answers = {
      unitClauses = [];
      pureLiterals = [];
      freeLiterals = [];
      isFalse = false
    } in
    
    let tabClauses = Array.of_list (List.mapi (fun i l -> let lst = List.map (fun a -> let x = Literal.make a in
                                                                                       nbOccur.(Literal.id_of_literal x) <- nbOccur.(Literal.id_of_literal x) + 1;
                                                                                       x) l in
                                                          match lst with
                                                           | [] -> answers.isFalse <- true;
                                                                   (lst, None, None)
                                                           | x::[] -> answers.unitClauses <- x::answers.unitClauses;
                                                                      watchedLiterals.(Literal.id_of_literal x) <- i::watchedLiterals.(Literal.id_of_literal x);
                                                                      (lst, Some x, None)
                                                           | x::y::_-> watchedLiterals.(Literal.id_of_literal x) <- i::watchedLiterals.(Literal.id_of_literal x);
                                                                       watchedLiterals.(Literal.id_of_literal y) <- i::watchedLiterals.(Literal.id_of_literal y);
                                                                       (lst, Some x, Some y))
                                               clauses) in
                                               
    answers.freeLiterals <- List.sort compare (literalList v);
    
    (tabClauses, watchedLiterals, tabLiterals, answers)
    

  (* Print formula on output "out" *)
  let print out (clauses, watchedLiterals, literals, answers) =
    let f x = Literal.print out x; output_string out " " in
    output_string out "----------- Clauses ------------\n";
    Array.iter (function l, a, b -> List.iter (fun x -> if a = Some x || b = Some x then begin
                                                     output_string out "[";
                                                     Literal.print out x;
                                                     output_string out "] ";
                                                   end else begin
                                                     Literal.print out x;
                                                     output_string out " ";
                                                   end) l;
                               output_string out "\n") clauses;
    output_string out "--------- unitClauses ----------\n";
    List.iter f answers.unitClauses;
    output_string out "\n--------- pureLiterals ---------\n";
    List.iter f answers.pureLiterals;
    output_string out "\n--------- freeLiterals ---------\n";
    List.iter f answers.freeLiterals;
    output_string out "\n----------- isFalse ------------\n";
    output_string out (if answers.isFalse then "Formula is false" else "Can't assert that formula is false");
    output_string out "\n--------------------------------\n"

  let setLiteral x (clauses, watchedLiterals, literals, answers) =
    literals.(Literal.id_of_literal x) <- true;
    
    let rec remove = function
      | [] -> []
      | a::l when a = x -> remove l
      | a::l when a = Literal.neg x -> remove l
      | a::l -> a::(remove l) in
    answers.unitClauses <- remove answers.unitClauses;
    answers.pureLiterals <- remove answers.pureLiterals;
    answers.freeLiterals <- remove answers.freeLiterals;
    
    let find cl = match cl with
      | _,None,_ -> None
      | _,_,None -> None
      | lst, Some u, Some v -> (let rec aux = function
                                | [] -> None
                                | a::l -> if literals.(Literal.id_of_literal (Literal.neg a)) || a = u || a = v
                                            then aux l
                                            else Some a in
                              aux lst) in
                                  
    let old = Some (Literal.neg x) in
    let update i =
      let l, u, v = clauses.(i) in
      let other = if old = u then v else u in
      match find clauses.(i) with
        | Some a -> watchedLiterals.(Literal.id_of_literal a) <- i::watchedLiterals.(Literal.id_of_literal a);
                    clauses.(i) <- (l, Some a, other);
                    false (* On change le pointeur, car un autre a ete trouve *)
        | None -> (match other with
                     | None -> answers.isFalse <- true
                     | Some a -> if literals.(Literal.id_of_literal a)
                                   then () (* La clause est daje satisfaite *)
                                   else if literals.(Literal.id_of_literal (Literal.neg a))
                                     then answers.isFalse <- true (* La clause est fausse *)
                                     else answers.unitClauses <- a::answers.unitClauses);
                  true (* On garde le pointeur actuel car on en a pas trouve d'autre... *)
    in
    watchedLiterals.(Literal.id_of_literal (Literal.neg x)) <-
      List.filter update watchedLiterals.(Literal.id_of_literal (Literal.neg x))(*; 
    
    output_string stderr "---------------------- SET ";
    Literal.print stderr x;
    output_string stderr " ----------------------------\n";
    print stderr (clauses, watchedLiterals, literals, answers)*)

  let forgetLiteral x (clauses, watchedLiterals, literals, answers) =
    literals.(Literal.id_of_literal x) <- false;
    answers.unitClauses <- [];
    answers.freeLiterals <- x::answers.freeLiterals;
    answers.isFalse <- false(*; 
    
    output_string stderr "---------------------- FORGET ";
    Literal.print stderr x;
    output_string stderr " ----------------------------\n";
    print stderr (clauses, watchedLiterals, literals, answers)*)

  let isFalse (_,_,_,answers) = answers.isFalse

  let isTrue (_,_,_,answers) = (answers.freeLiterals = []) && (not answers.isFalse)

  let rec getUnitClause (_,_,_,answers) = match answers.unitClauses with
    | [] -> None
    | x::l -> Some x
    
  let getPureLiteral (_,_,_,answers) = match answers.pureLiterals with
    | [] -> None
    | x::l -> Some x
    
  let getFreeLiteral (_,_,_,answers) = match answers.freeLiterals with
    | [] -> None
    | x::l -> Some x

end

