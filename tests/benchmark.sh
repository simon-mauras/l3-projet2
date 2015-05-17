#!/bin/bash
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
python3 ./benchmark_clwl.py ../resol cnf/downloads/aim*.cnf > aim.csv;
python3 ./plot_clwl.py aim.csv "Source: http://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/DIMACS/AIM/descr.html" &
python3 ./benchmark_clwl.py ../resol cnf/downloads/*dubois*.cnf > dubois.csv;
python3 ./plot_clwl.py dubois.csv "Source: http://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/DIMACS/DUBOIS/descr.html" &
python3 ./benchmark_clwl.py ../resol cnf/downloads/ii*.cnf > ii.csv;
python3 ./plot_clwl.py ii.csv "Source: http://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/DIMACS/II/descr.html" &
python3 ./benchmark_clwl.py ../resol cnf/downloads/jnh*.cnf > jnh.csv;
python3 ./plot_clwl.py jnh.csv "Source: http://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/DIMACS/JNH/descr.html" &
python3 ./benchmark_clwl.py ../resol cnf/downloads/par*.cnf > par.csv;
python3 ./plot_clwl.py uf.csv "Source: http://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/DIMACS/PARITY/descr.html"&
python3 ./benchmark_clwl.py ../resol cnf/downloads/u*.cnf > uf.csv;
python3 ./plot_clwl.py uf.csv "Source: http://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/RND3SAT/descr.html"&

