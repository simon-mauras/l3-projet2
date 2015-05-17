#!/bin/bash
LANG=C
body() {
	IFS= read -r header
	printf '%s\n' "$header"
	"$@"
}
cd ./cnf/downloads;
make clean;
wget --continue http://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/DIMACS/AIM/aim.tar.gz;
wget --continue http://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/RND3SAT/uf200-860.tar.gz;
wget --continue http://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/RND3SAT/uuf200-860.tar.gz;
wget --continue http://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/DIMACS/DUBOIS/dubois.tar.gz;
wget --continue http://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/DIMACS/II/inductive-inference.tar.gz;
wget --continue http://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/DIMACS/JNH/jnh.tar.gz;
wget --continue http://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/DIMACS/PARITY/parity.tar.gz;
for i in *.tar.gz; do tar -xf $i; done
cd ../../;
# Les lignes commentées représentent des tests pour lesquels on a besoin de beaucoup de temps
python3 ./benchmark_clwl.py ../resol cnf/downloads/aim*.cnf | body sort -t, -k 2 -n> aim.csv;
python3 ./plot_clwl.py aim.csv "Source: http://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/DIMACS/AIM/descr.html" &
python3 ./benchmark_clwl.py ../resol cnf/downloads/*dubois*.cnf | body sort -t, -k 2 -n> dubois.csv;
python3 ./plot_clwl.py dubois.csv "Source: http://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/DIMACS/DUBOIS/descr.html" &
# python3 ./benchmark_clwl.py ../resol cnf/downloads/ii*.cnf | body sort -t, -k 2 -n> ii.csv;
#python3 ./plot_clwl.py ii.csv "Source: http://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/DIMACS/II/descr.html" &
python3 ./benchmark_clwl.py ../resol cnf/downloads/jnh*.cnf | body sort -t, -k 2 -n> jnh.csv;
python3 ./plot_clwl.py jnh.csv "Source: http://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/DIMACS/JNH/descr.html" &
#python3 ./benchmark_clwl.py ../resol cnf/downloads/par*.cnf | body sort -t, -k 2 -n> par.csv;
#python3 ./plot_clwl.py uf.csv "Source: http://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/DIMACS/PARITY/descr.html"&
python3 ./benchmark_clwl.py ../resol cnf/downloads/uf*100-*.cnf | body sort -t, -k 2 -n> u-100.csv;
python3 ./plot_clwl.py u-100.csv "Source: http://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/RND3SAT/descr.html (100, satisfiables)"&
#python3 ./benchmark_clwl.py ../resol cnf/downloads/uuf*100-*.cnf | body sort -t, -k 2 -n> uu-100.csv;
#python3 ./plot_clwl.py uu-100.csv "Source: http://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/RND3SAT/descr.html (100, non satisfiable)"&

# python3 ./benchmark_clwl.py ../resol cnf/downloads/u*.cnf | body sort -t, -k 2 -n> u-all.csv;
# python3 ./plot_clwl.py u-all.csv "Source: http://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/RND3SAT/descr.html (all)"&



