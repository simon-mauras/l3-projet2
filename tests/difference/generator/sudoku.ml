for i = 1 to 9 do
  for j = 1 to 9 do
    Printf.printf "(X%d%d <= 9 /\\ X%d%d >= 0) /\\\n" i j i j;
    for k = 1 to 9 do
      if k <> j then
        Printf.printf "(X%d%d - X%d%d != 0) /\\ " i j i k;
    done;
    Printf.printf "\n";
    for k = 1 to 9 do
      if k <> i then
        Printf.printf "(X%d%d - X%d%d != 0) /\\ " i j k j;
    done;
    Printf.printf "\n";
    let a = 3 * ((i - 1) / 3) + 1 in
    let b = 3 * ((j - 1) / 3) + 1 in
    for u = 0 to 2 do
    for v = 0 to 2 do
      if i <> u+a || j <> v+b then
        Printf.printf "(X%d%d - X%d%d != 0) /\\ " i j (u+a) (v+b);
    done;
    done;
    Printf.printf "\n";
  done;
done;

for i = 1 to 9 do
  for j = 1 to 9 do
    let n = Scanf.scanf "%d " (fun i -> i) in
    if 1 <= n && n <= 9 then
      Printf.printf "(X%d%d = %d) /\\\n" i j n;
  done;
done;

Printf.printf "EOF = 0\n";
