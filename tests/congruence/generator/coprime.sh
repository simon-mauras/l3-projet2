for i in `seq 10 20`; do
for j in `seq $i 20`; do
  echo -e "$i\n$j" > coprime.in
  echo -e "------- Are $i and $j coprime integers ? ------"
  ocaml coprime.ml < coprime.in > coprime.cong
  ../../../resol -cl -congruence coprime.cong coprime.out
  grep "s UNSATISFIABLE" coprime.out > coprime.unsat

  # Euclidean algorithm
  let "a = $i"
  let "b = $j"
  let "c = 1"
  until [ "$c" -eq 0 ]
  do
    let "c = $a % $b"
    let "a = $b"
    let "b = $c"
  done

  if [ -s coprime.unsat ]; then
    echo "Yes !"
    if [ "$a" -ne 1 ]; then
      echo "Wrong answer !!"
      rm coprime.in coprime.cong coprime.out coprime.unsat
      exit
    fi
  else
    echo "No..."
    if [ "$a" -eq 1 ]; then
      echo "Wrong answer !!"
      rm coprime.in coprime.cong coprime.out coprime.unsat
      exit
    fi
  fi
done;
done;

rm coprime.in coprime.cong coprime.out coprime.unsat
