open Sigs

module Make =
  functor (S : Map.OrderedType) ->
  struct
    
    module MapS = Map.Make(S)
    
    let make (data : S.t formula) : (cnf * S.t option array) =
    
      (* On commence par construire une map *)
      let leaf = ref 0 in
      let node = ref 0 in
      let rec build_map s = function
        | And (a, b) -> incr node; build_map (build_map s a) b
        | Or  (a, b) -> incr node; build_map (build_map s a) b
        | Imp (a, b) -> incr node; build_map (build_map s a) b
        | Not (a)    -> incr node; build_map s a
        | Atom (x)   -> if MapS.mem x s
                          then s
                          else (incr leaf; MapS.add x !leaf s)
      in
      let map = build_map MapS.empty data in
      
      (* Puis on remplis un tableau *)
      let tab = Array.make (!leaf + !node + 1) None in
      ignore (MapS.iter (fun x i -> tab.(i) <- Some x) map);
      
      (* Puis on génère une formule cnf *)
      node := !leaf;
      let build_form x =
        let rec aux l0 = function
          | And (a, b) -> incr node; let n0 = !node in
                          let l1, n1 = aux l0 a in
                          let l2, n2 = aux l1 b in
                          ([n0;-n1;-n2]::[-n0;n1]::[-n0;n2]::l2, n0)
          | Or  (a, b) -> incr node; let n0 = !node in
                          let l1, n1 = aux l0 a in
                          let l2, n2 = aux l1 b in
                          ([-n0;n1;n2]::[n0;-n1]::[n0;-n2]::l2, n0)
          | Imp (a, b) -> incr node; let n0 = !node in
                          let l1, n1 = aux l0 a in
                          let l2, n2 = aux l1 b in
                          ([-n0;-n1;n2]::[n0;n1]::[n0;-n2]::l2, n0)
          | Not (a)    -> incr node; let n0 = !node in
                          let l1, n1 = aux l0 a in
                          ([-n0;-n1]::[n0;n1]::l1, n0)
          | Atom (x)   -> (l0, MapS.find x map)
        in
        let form, n = aux [] x in [n]::form
      in
      let form = build_form data in
      
      (* Formule cnf obtenue *)
      ((!node, List.length form, form), tab)
  
  end
