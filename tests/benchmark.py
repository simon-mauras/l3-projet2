#!/bin/python3
import sys
from timeit import timeit
import numpy

options = ["-cl", "-wl", "-cl -wl"]
heuristics = ["-dlis", "-vsids", "-moms", "-rand"]
reps = 3


def bench(cmd_params):
        time_taken = timeit(stmt="subprocess.call(%s, stdout=devnull, stderr=devnull)" % cmd_params, setup="""import subprocess; devnull = open('/dev/null', 'w');""", number=1)
        return time_taken

if __name__ == '__main__':
        if(len(sys.argv) < 3):
                print("Usage : " + sys.argv[0] + " sat_solver file1.cnf file2.cnf... > data.csv")
                sys.exit(1)
        cnffiles = sys.argv[2:]
        solver = sys.argv[1].split()

        a = numpy.zeros(shape=(options.__len__(), heuristics.__len__(), reps))
        print("name,cldlis,clvsids,clmoms,clrand,wldlis,wlvsids,wlmoms,wlrand,clwldlis,clwlvsids,clwlmoms,clwlrand,")
        for x in cnffiles:
                for i, option in enumerate(options):
                        for j, heuristic in enumerate(heuristics):
                            for k in range(0, reps):
                                a[i][j][k] = bench(solver+[option]+[heuristic]+[x])
                sys.stdout.write(x + ",")
                for i, option in enumerate(options):
                    for j, heuristic in enumerate(heuristics):
                        sys.stdout.write(str(numpy.median(a[i][j]))+",")
                print("")
