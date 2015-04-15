(* Gen1 : efficient for stupid implementation of Davis-Putnam (without checking redondant clauses) *)

let rec print_clauses res = function
| 0 -> List.iter (fun x -> print_int x; print_string " ") res;
       print_string "0 \n"
| n -> print_clauses ((n)::res) (n-1);
       print_clauses ((-n)::res) (n-1) in
let rec pow2 = function
| 0 -> 1
| n -> 2 * pow2 (n-1) in

let n = read_int () in
print_string "p cnf ";
print_int n;
print_string " ";
print_int (2 * pow2 n);
print_string "\n";
print_clauses [] n;
print_clauses [] n;
