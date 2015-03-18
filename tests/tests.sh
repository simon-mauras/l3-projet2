for i in `find -name "*.cnf" | sort`;
do
  echo "-----------------------------------------";
  echo $i;

  time ../resol -wl -cl $i resol.out
  minisat $i > minisat.out

  grep "s UNSATISFIABLE" resol.out > resol.unsat
  grep "UNSATISFIABLE" minisat.out > minisat.unsat

  if [ -s resol.unsat ]; then
    if [ -s minisat.unsat ]; then
      echo "Ok UNSATISFIABLE"
    else
      echo "Resol : UNSATISFIABLE"
      echo "Minisat : SATISFIABLE"
      break
    fi
  else
    if [ -s minisat.unsat ]; then
      echo "Resol : SATISFIABLE"
      echo "Minisat : UNSATISFIABLE"
      break
    else
      echo "Ok SATISFISABLE"
      cat $i > resol.sat
      echo "" >> resol.sat
      grep "0" resol.out | tr ' ' '\n' | { read var; until [ 0 -eq $var ]; do echo $var 0 >> resol.sat; read var; done }
      minisat resol.sat 2> /dev/null | grep "UNSATISFIABLE" > minisat.sat
      if [ -s minisat.sat ]; then
        echo "Error : Wrong solution"
        break
      else
        echo "Solution Ok"
      fi
    fi
  fi
done
rm resol.out
rm minisat.out
rm resol.unsat
rm minisat.unsat
rm resol.sat
rm minisat.sat
