(** Module implementant la theorie de la difference *)

open Sigs

(** Module de type Theory_type *)
module Make : Theory_type =
  struct
    
    (** Module representant un terme de la theorie de la difference *)
    module T =
      struct
        type var = string
        type atom = X of var | Zero
        type t = atom * atom * int (* xi - xj <= n *)
        
        let make x =
          let module S = Data.Difference in
          let rec aux_atom = function
          | S.X s -> X s in
          let aux_term = function
          | S.Ternary (a, b, S.Lt, n)  -> Atom (aux_atom a, aux_atom b, n-1)
          | S.Ternary (a, b, S.Leq, n) -> Atom (aux_atom a, aux_atom b, n)
          | S.Ternary (a, b, S.Gt, n)  -> Atom (aux_atom b, aux_atom a, -n-1)
          | S.Ternary (a, b, S.Geq, n) -> Atom (aux_atom b, aux_atom a, -n)
          | S.Ternary (a, b, S.Eq, n)  -> And (Atom (aux_atom a, aux_atom b, n),   Atom (aux_atom b, aux_atom a, -n))
          | S.Ternary (a, b, S.Neq, n) -> Or  (Atom (aux_atom a, aux_atom b, n-1), Atom (aux_atom b, aux_atom a, -n-1))
          | S.Binary (a, S.Lt, n)  -> Atom (aux_atom a, Zero, n-1)
          | S.Binary (a, S.Leq, n) -> Atom (aux_atom a, Zero, n)
          | S.Binary (a, S.Gt, n)  -> Atom (Zero, aux_atom a, 1-n)
          | S.Binary (a, S.Geq, n) -> Atom (Zero, aux_atom a, -n)
          | S.Binary (a, S.Eq, n)  -> And (Atom (aux_atom a, Zero, n),   Atom (Zero, aux_atom a, -n))
          | S.Binary (a, S.Neq, n) -> Or  (Atom (aux_atom a, Zero, n-1), Atom (Zero, aux_atom a, -n-1)) in
          let aux_parsing = function
          | Parsing_difference a -> aux_term a
          | _ -> failwith "Wrong parser" in
          let rec aux_formula = function
          | And(a, b) -> And(aux_formula a, aux_formula b)
          | Or(a, b) -> Or(aux_formula a, aux_formula b)
          | Imp(a, b) -> Imp(aux_formula a, aux_formula b)
          | Not(a) -> Not(aux_formula a)
          | Atom(a) -> aux_parsing a in
          aux_formula x
        
        let neg (a, b, n) : t = (b, a, -n-1)
        
        let compare = Pervasives.compare
        
        let print output (a, b, n) =
          let print_atom = function
            | X s -> output_string output s
            | Zero -> output_string output "Zero" in
          print_atom a;
          output_string output " - ";
          print_atom b;
          output_string output " <= ";
          output_string output (string_of_int n)
      end
    
    module MapVar = Map.Make (struct
      type t = T.atom
      let compare = Pervasives.compare
    end)
    
    (** Type d'un ensemble de contraintes *)
    type t_literal = T.t option array * L.t list ref
    type t_var = int MapVar.t * T.atom array
    type t = t_literal * t_var
    
    (** Un ensemble de contraintes vide *)
    let make tab : t =
    
      let tabLiterals = Array.make (2 * Array.length tab) None in
      for i=1 to (Array.length tab) - 1 do
        match tab.(i) with
          | None -> ()
          | Some x ->
            tabLiterals.(L.id_of_literal (L.make i)) <- Some x;
            tabLiterals.(L.id_of_literal (L.make (-i)))<- Some (T.neg x);
      done;
      
      let nbVars = ref 0 in
      let mapVars = ref MapVar.empty in
      let addVars s =
        if not (MapVar.mem s !mapVars) then begin
          mapVars := MapVar.add s !nbVars !mapVars;
          incr nbVars;
        end in
      Array.iter (function None -> () | Some (a,b,_) -> addVars a; addVars b) tab;
      let tabVars = Array.make !nbVars (T.X "") in
      MapVar.iter (fun s i -> tabVars.(i) <- s) !mapVars;
      
      ((tabLiterals, ref []), (!mapVars, tabVars))
    
    (** Ajoute une contrainte *)
    let setConstraint lit ((tabLiterals, currentLiterals), (mapVars, tabVars) : t) =
      currentLiterals := lit::!currentLiterals
    
    (** Oublie une contrainte *)
    let forgetConstraint lit ((tabLiterals, currentLiterals), (mapVars, tabVars) : t) =
      let rec remove = function
      | a::l when a = lit -> l
      | a::l -> a::(remove l)
      | [] -> [] in
      currentLiterals := remove !currentLiterals
    
    (** Renvoie une eventuelle contradiction entre les contraintes actuelles *)
    let getContradiction ((tabLiterals, currentLiterals), (mapVars, tabVars) : t) =
      let rec build_edges res = function
      | lit::lst -> (match tabLiterals.(L.id_of_literal lit) with
                     | None -> build_edges res lst
                     | Some (a, b, n) -> let aId = MapVar.find a mapVars in
                                         let bId = MapVar.find b mapVars in
                                         build_edges ((aId, bId, n, lit)::res) lst)
      | [] -> res in
      let edges = build_edges [] !currentLiterals in
      
      let dist = Array.make (Array.length tabVars) 0 in
      let prec = Array.make (Array.length tabVars) [] in
      for i=1 to Array.length tabVars do
        List.iter (fun (a, b, n, l) ->
                   if dist.(a) + n < dist.(b) then begin
                     dist.(b) <- dist.(a) + n;
                     prec.(b) <- l::prec.(a);
                   end) edges;
      done;
      
      try
        let id,_,_,_ = List.find (fun (a, b, n, l) -> dist.(a) + n < dist.(b)) edges in
        Some (List.map L.neg prec.(id))
      with
        | Not_found -> None
end

