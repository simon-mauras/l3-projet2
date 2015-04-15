let size = read_int () in

for i = 1 to size do
  for j = i+1 to size do
    Printf.printf "%d != %d /\\ " i j;
  done;
  Printf.printf "\n";
done;

for i = 1 to size do
  for j = 1 to size do
    Printf.printf "(X%d%d = 1" i j;
    for k = 2 to size do
      Printf.printf " \\/ X%d%d = %d" i j k;
    done;
    Printf.printf ") /\\\n";
  done;
done;
Printf.printf "\n";

for i = 1 to size do
  for j = 1 to size do
    for k = j+1 to size do
      Printf.printf "(X%d%d != X%d%d) /\\ " i j i k;
    done;
  done;
  Printf.printf "\n";
done;

for i = 1 to size do
  for j = 1 to size do
    for k = j+1 to size do
      Printf.printf "(X%d%d != X%d%d) /\\ " j i k i;
    done;
  done;
  Printf.printf "\n";
done;

Printf.printf "EOF = EOF\n";
