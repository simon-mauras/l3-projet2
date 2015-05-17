 # -*- coding: utf-8 -*-
 #!/bin/python3

import sys
from timeit import timeit
import numpy

heuristics = ["-dlis", "-vsids", "-moms", "-rand"]
reps = 10


def bench(cmd_params):
    time_taken = timeit(stmt = "subprocess.call(%s, stdout=devnull, stderr=devnull)" % cmd_params, setup = ""
        "import subprocess; devnull = open('/dev/null', 'w');"
        "", number = 1)
    return time_taken

if __name__ == '__main__':
    if (len(sys.argv) < 3):
        print("Usage : " + sys.argv[0] + " sat_solver file1.cnf file2.cnf... > data.csv")
        sys.exit(1)
cnffiles = sys.argv[2: ]
solver = sys.argv[1].split()

a = numpy.zeros(shape = (1, heuristics.__len__(), reps))
print("name,clwldlis,clwlvsids,clwlmoms,clwlrand,")
for x in cnffiles:
    sys.stderr.write("Traitement du fichier " + x + "\n")
    for j, heuristic in enumerate(heuristics):
        for k in range(0, reps):
            a[0][j][k] = bench(solver + ["-cl"] + ["-wl"] + [heuristic] + [x])
    sys.stdout.write(x + ",");
    for j, heuristic in enumerate(heuristics):
        sys.stdout.write(str(numpy.median(a[0][j])) + ",")
    print("")
