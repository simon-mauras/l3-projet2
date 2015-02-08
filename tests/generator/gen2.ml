(* Gen2 : For SAT-solver with optimisation *)

let n = read_int () in
print_string "p cnf ";
print_int (3*n);
print_string " ";
print_int (2*n+1);
print_string "\n";

for i=3*n downto 2*n+1 do
  print_int i;
  print_string " ";
done;
print_string "0\n";

for i=1 to 2*n do
  print_int (((i-1) / 2) - 3*n);
  print_string " ";
  print_int i;
  print_string " 0\n";
done;
