(** Module implémentant la theorie de la congruence *)

module L = Sigs.Literal

(** Module de type Sigs.Theory_type *)
module Make : Sigs.Theory_type =
  struct
    
    (** Module représentant un terme de la théorie de la congruence *)
    module T =
      struct
      
        type var = string
        type atom = X of var | Fun of var * atom list
        type t = Eq of atom * atom | Neq of atom * atom
        
        let neg = function
          | Eq (a, b) -> Neq (a, b)
          | Neq (a, b) -> Eq (a, b)
        
        let make x =
          let module S = Sigs.Data.Congruence in
          let rec aux_atom = function
          | S.X s -> X s
          | S.Fun (s, l) -> Fun (s, List.map aux_atom l) in
          let aux_term = function
          | S.Eq (a, b) -> Eq (aux_atom a, aux_atom b)
          | S.Neq (a, b) -> Neq (aux_atom a, aux_atom b) in
          let aux_parsing = function
          | Sigs.Parsing_congruence a -> aux_term a
          | _ -> failwith "Wrong parser" in
          let rec aux_formula = function
          | Sigs.And(a, b) -> Sigs.And(aux_formula a, aux_formula b)
          | Sigs.Or(a, b) -> Sigs.Or(aux_formula a, aux_formula b)
          | Sigs.Imp(a, b) -> Sigs.Imp(aux_formula a, aux_formula b)
          | Sigs.Not(a) -> Sigs.Not(aux_formula a)
          | Sigs.Atom(a) -> Sigs.Atom(aux_parsing a) in
          aux_formula x
          
        let compare = Pervasives.compare
        
        let print output x =
          let rec print_atom = function
            | X s -> output_string output s
            | Fun (s, l) -> output_string output s;
                            List.iter (fun u -> output_string output "[";
                                                print_atom u;
                                                output_string output "]") l in
          match x with
            | Eq (a, b)  -> print_atom a;
                            output_string output " = ";
                            print_atom b
            | Neq (a, b) -> print_atom a;
                            output_string output " != ";
                            print_atom b
      end
    
    module MapVar = Map.Make (struct
      type t = T.atom
      let compare = Pervasives.compare
    end)
    
    (** Type d'un ensemble de contraintes *)
    type t_literal = T.t option array
    type t_var = int MapVar.t * T.atom array
    type t_equality = int array * L.t option array * int array * (int * int) list array
    type t_disequality = (int * int * L.t) list ref
    type t = t_literal * t_var * t_equality * t_disequality
    
    (** Un ensemble de contraintes vide *)
    let make tab =
    
      let nbLiterals = (2 * Array.length tab) - 2 in
      let tabLiterals = Array.make nbLiterals None in
      for i=1 to (Array.length tab) - 1 do
        match tab.(i) with
          | None -> ()
          | Some x ->
            tabLiterals.(L.id_of_literal (L.make i)) <- Some x;
            tabLiterals.(L.id_of_literal (L.make (-i)))<- Some (T.neg x);
      done;
      
      let nbVars = ref 0 in
      let mapVars = ref MapVar.empty in
      let addVars x =
        if not (MapVar.mem x !mapVars) then begin
          mapVars := MapVar.add x !nbVars !mapVars;
          incr nbVars;
        end in
      let findVars = function
        | T.Eq(a, b) -> addVars a; addVars b
        | T.Neq(a, b) -> addVars a; addVars b in
      Array.iter (function None -> () | Some x -> findVars x) tab;
      let tabVars = Array.make !nbVars (T.X "") in
      MapVar.iter (fun s i -> tabVars.(i) <- s) !mapVars;
      
      ((tabLiterals),
       (!mapVars, tabVars),
       (Array.init !nbVars (fun i -> i), Array.make !nbVars None, Array.make !nbVars 1, Array.make nbLiterals []),
       ref [])
    
        
    (** Ajoute une contrainte *)
    let setConstraint lit (tabLiterals, (mapVars, tabVars), (find, cause, size, equality), disequality) =
      let rec root i =
        if i = find.(i)
          then i
          else root find.(i) in
      let idLit = L.id_of_literal lit in
      match tabLiterals.(idLit) with
      | Some x -> begin
        match x with
        | T.Eq(a, b) ->
          let u = root (MapVar.find a mapVars) in
          let v = root (MapVar.find b mapVars) in
          if u <> v then begin
            if size.(u) > size.(v) then begin
              size.(u) <- size.(u) + size.(v);
              find.(v) <- u;
              cause.(v) <- Some lit;
              equality.(idLit) <- [(v, u)];
            end else begin
              size.(v) <- size.(u) + size.(v);
              find.(u) <- v;
              cause.(u) <- Some lit;
              equality.(idLit) <- [(u, v)];
            end
          end
        | T.Neq(a, b) -> disequality := ((MapVar.find a mapVars), (MapVar.find b mapVars), lit)::!disequality
        end
      | None -> ()
    
    (** Oublie une contrainte *)
    let forgetConstraint lit (tabLiterals, (mapVars, tabVars), (find, cause, size, equality), disequality) =
      let idLit = L.id_of_literal lit in
      match tabLiterals.(idLit) with
      | Some x -> begin
        match x with
        | T.Eq(a, b) ->
          List.iter (fun (u, v) -> find.(u) <- u;
                                   cause.(u) <- None;
                                   size.(v) <- size.(v) - size.(u)) equality.(idLit);
          equality.(idLit) <- []
        | T.Neq _ ->
          disequality := List.filter (fun (_,_,l) -> lit <> l) !disequality
        end
      | None -> ()
    
    (** Renvoie une eventuelle contradiction entre les contraintes actuelles *)
    let getContradiction (tabLiterals, (mapVars, tabVars), (find, cause, size, equality), disequality) =
    
      let rec root i =
        if i <> find.(i)
          then root find.(i)
          else i in
          
      let module SetInt = Set.Make(struct
        type t = int
        let compare = Pervasives.compare
      end) in
      let rec dfs seen i =
        if i = find.(i) || SetInt.mem i seen
          then seen
          else
            let id = match cause.(i) with
                     | None -> failwith "Impossible"
                     | Some l -> L.id_of_literal l in
            match tabLiterals.(id) with
            | Some (T.Eq (a, b)) ->
              let aId = MapVar.find a mapVars in
              let bId = MapVar.find b mapVars in
              dfs (dfs (SetInt.add i seen) aId) bId
            | _ -> failwith "Impossible" in
      
      let conflict = ref [] in
      List.iter (fun (a, b, lit) ->
                 if !conflict = [] then
                   let u = root a in
                   let v = root b in
                   if u = v then begin
                     let seen = dfs (dfs SetInt.empty a) b in
                     conflict := lit::!conflict;
                     SetInt.iter (fun i -> match cause.(i) with
                                             | Some l -> conflict := l::!conflict
                                             | None -> ()) seen;
                 end) !disequality;
      if !conflict = [] then None else Some (List.map L.neg !conflict)

end

