for i in `find -name "*.cnf" | sort`;
do
  echo "-----------------------------------------";
  echo $i;
  time ../resol $i
done
