for i = 1 to 9 do
  for j = 1 to 9 do
    (* Unicité de la valeur d'une case *)
    for u = 1 to 9 do
    for v = u+1 to 9 do
      Printf.printf "(X%d%d%d => ~X%d%d%d) /\\ " i j u i j v;
    done;
    done;
    Printf.printf "\n";
    
    (* Existence de la valeur d'une case *)
    Printf.printf "(X%d%d1" i j;
    for u = 2 to 9 do
      Printf.printf " \\/ X%d%d%d" i j u;
    done;
    Printf.printf ") /\\\n";
    
    (* Colonne *)
    for u = 1 to 9 do
    for k = 1 to 9 do
      if k <> j then
        Printf.printf "(X%d%d%d => ~X%d%d%d) /\\ " i j u i k u;
    done;
    done;
    Printf.printf "\n";
    
    (* Ligne *)
    for u = 1 to 9 do
    for k = 1 to 9 do
      if k <> i then
        Printf.printf "(X%d%d%d => ~X%d%d%d) /\\ " i j u k j u;
    done;
    done;
    Printf.printf "\n";
    
    (* Carre *)
    let a = 3 * ((i - 1) / 3) + 1 in
    let b = 3 * ((j - 1) / 3) + 1 in
    for u = 1 to 9 do
    for x = 0 to 2 do
    for y = 0 to 2 do
      if i <> x+a || j <> y+b then
        Printf.printf "(X%d%d%d => ~X%d%d%d) /\\ " i j u (x+a) (y+b) u;
    done;
    done;
    done;
    Printf.printf "\n";
    Printf.printf "\n";
  done;
done;

for i = 1 to 9 do
  for j = 1 to 9 do
    let n = Scanf.scanf "%d " (fun i -> i) in
    if 1 <= n && n <= 9 then
      Printf.printf "X%d%d%d /\\\n" i j n;
  done;
done;

Printf.printf "EOF\n";
