let a = read_int() in
let b = read_int() in

let rec print = function
| 0 -> print_string "X";
| n -> print_string "S(";
       print (n-1);
       print_string ")" in

print b;
print_string " = X\n";
print_string "/\\\n";
print a;
print_string " = X\n";
print_string "/\\\n";
print_string "S(X) != X";
