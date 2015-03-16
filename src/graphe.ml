(** Module implémentant la génération de graphes *)

(** Module de type Sigs.Solver_type *)
module Make (F: Sigs.Formula_type) =
struct

  module Formula = F
  module Literal = F.Literal

  (** Type d'un graphe **)
  type t = {
    adjacencyMatrix : bool array array;
    displayed: bool array;
  }

  (** Construit un graphe (de type t) à partir d'un tableau de litéraux (un noeud par litéral) en y ajoutant un symbole d'absurdité **)
  let make n =
    let m = Array.make_matrix (n+1) (n+1) false in
    let d = Array.make (n+1) false in
    {adjacencyMatrix = m; displayed = d};;

  (** Modifie notre matrice pour y ajouter une arête **)
  let add orId destId m =
    m.adjacencyMatrix.(orId).(destId) <- true;;

  let populateEdges m =
    Array.iteri
      (fun destId row ->
         match (Formula.getCause (Literal.literal_of_id destId)) with
         |None -> ();
         |Some cause ->
           begin
             let literals = Formula.getClause cause in
             List.iter
               (fun literal ->
                  add (Literal.id_of_literal literal) destId m;
               )
               literals;
           end;
      ) m.adjacencyMatrix;;

  let export m name =
    Printf.printf "digraph %s {\n" name;
    Array.iteri
      (fun orId line ->
         let ori = Literal.to_int (Literal.literal_of_id orId) in
         Array.iteri (fun destId b ->
             let dest =  Literal.to_int (Literal.literal_of_id destId) in
             if(b) then Printf.printf "\"%d\" -> \"%d\";\n" ori dest;)
           line;
      )
      m.adjacencyMatrix;
    Printf.printf "}";
end
