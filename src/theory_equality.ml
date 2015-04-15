(** Module implémentant la theorie de l'égalité *)

module L = Sigs.Literal

(** Module de type Sigs.Theory_type *)
module Make : Sigs.Theory_type =
  struct
    
    (** Module représentant un terme de la théorie de l'égalité *)
    module T =
      struct
        type var = string
        type atom = X of var
        type t = Eq of atom * atom | Neq of atom * atom
        
        let neg = function
          | Eq (a, b) -> Neq (a, b)
          | Neq (a, b) -> Eq (a, b)
        
        let make x =
          let module S = Sigs.Data.Equality in
          match x with
          | Sigs.Parsing_equality (S.Eq (S.X a, S.X b)) -> Eq (X a, X b)
          | Sigs.Parsing_equality (S.Neq (S.X a, S.X b)) -> Neq (X a, X b)
          | _ -> failwith "Wrong parser"
          
        let compare = Pervasives.compare
        
        let print output x =
          let print_atom = function
            | X s -> output_string output s in
          match x with
            | Eq (a, b)  -> print_atom a;
                            output_string output " = ";
                            print_atom b
            | Neq (a, b) -> print_atom a;
                            output_string output " != ";
                            print_atom b
      end
    
    module MapVar = Map.Make (struct
      type t = T.var
      let compare = Pervasives.compare
    end)
    
    (** Type d'un ensemble de contraintes *)
    type t_literal = T.t option array * L.t list ref
    type t_var = int MapVar.t * T.var array
    type t_equality = int array * int array * (int * int * int * int) list ref
    type t_disequality = (int * int) list ref
    type t = t_literal * t_var * t_equality * t_disequality
    
    (** Un ensemble de contraintes vide *)
    let make tab =
    
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
      let findVars = function
        | T.Eq(T.X a, T.X b) -> addVars a; addVars b
        | T.Neq(T.X a, T.X b) -> addVars a; addVars b in
      Array.iter (function None -> () | Some x -> findVars x) tab;
      let tabVars = Array.make !nbVars "" in
      MapVar.iter (fun s i -> tabVars.(i) <- s) !mapVars;
      
      ((tabLiterals, ref []), (!mapVars, tabVars), (Array.init !nbVars (fun i -> i), Array.make !nbVars 1, ref []), ref [])
    
        
    (** Ajoute une contrainte *)
    let setConstraint l ((tabLiterals, currentLiterals), (mapVars, tabVars), (find, size, equality), disequality) =
      let rec root i =
        if i = find.(i)
          then i
          else root find.(i) in
      currentLiterals := l::!currentLiterals;
      match tabLiterals.(L.id_of_literal l) with
      | Some x -> begin
        match x with
        | T.Eq(T.X a, T.X b) ->
          let u = root (MapVar.find a mapVars) in
          let v = root (MapVar.find b mapVars) in
          if u <> v then begin
            if size.(u) > size.(v) then begin
              size.(u) <- size.(u) + size.(v);
              find.(v) <- u;
              equality := ((MapVar.find a mapVars), (MapVar.find b mapVars), v, u)::!equality;
            end else begin
              size.(v) <- size.(u) + size.(v);
              find.(u) <- v;
              equality := ((MapVar.find a mapVars), (MapVar.find b mapVars), u, v)::!equality;
            end
          end
        | T.Neq(T.X a, T.X b) -> disequality := ((MapVar.find a mapVars), (MapVar.find b mapVars))::!disequality
        end
      | None -> ()
    
    (** Oublie une contrainte *)
    let forgetConstraint l ((tabLiterals, currentLiterals), (mapVars, tabVars), (find, size, equality), disequality) =
      let rec remove = function
        | [] -> []
        | a::b when a = l -> b
        | a::b -> a::(remove b) in
      currentLiterals := remove !currentLiterals;
      match tabLiterals.(L.id_of_literal l) with
      | Some x -> begin
        match x with
        | T.Eq(T.X a, T.X b) ->
          let aId = MapVar.find a mapVars in
          let bId = MapVar.find b mapVars in
          let _,_,u,v = List.find (fun (u,v,_,_) -> u = aId && v = bId) !equality in
          equality := List.filter (fun (u,v,_,_) -> u <> aId || v <> bId) !equality;
          find.(u) <- u;
          size.(v) <- size.(v) - size.(u);
        | T.Neq(T.X a, T.X b) ->
          let aId = MapVar.find a mapVars in
          let bId = MapVar.find b mapVars in
          disequality := List.filter (fun (u,v) -> u <> aId || v <> bId) !disequality
        end
      | None -> ()
    
    (** Renvoie une eventuelle contradiction entre les contraintes actuelles *)
    let getContradiction ((tabLiterals, currentLiterals), (mapVars, tabVars), (find, size, equality), disequality) =
      let rec root i =
        if i = find.(i)
          then i
          else root find.(i) in
      if List.exists (fun (a, b) -> root a = root b) !disequality
        then Some (List.map L.neg !currentLiterals)
        else None
end

