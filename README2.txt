--- Comment profiler ? -----
make prof
./resol.p tests/ex1.cnf
gprof resol.p > prof.txt

--- Tests intéressants ? ---
tests/generator/gen1-10.cnf
==> WL 10x plus rapide
