 # -*- coding: utf-8 -*-
#!/bin/python3
import sys
import csv
import matplotlib.pyplot as plt
from pylab import *

if(len(sys.argv) != 3):
    print("Usage : " + sys.argv[0] + " data.csv \"sous-titre\"")
    sys.exit(1)

clwldlis = [[], []]
clwlvsids = [[], []]
clwlmoms = [[], []]
clwlrand = [[], []]

with open(sys.argv[1], "rt") as file:
    reader = csv.DictReader(file,  delimiter=',')
    for row in reader:
        clwldlis[0].append(clwldlis[0].__len__())
        clwldlis[1].append(row['clwldlis'])
        clwlvsids[0].append(clwlvsids[0].__len__())
        clwlvsids[1].append(row['clwlvsids'])
        clwlmoms[0].append(clwlmoms[0].__len__())
        clwlmoms[1].append(row['clwlmoms'])
        clwlrand[0].append(clwlrand[0].__len__())
        clwlrand[1].append(row['clwlrand'])


with plt.style.context('fivethirtyeight'):

        figure(3)
        plt.semilogy(clwldlis[0], clwldlis[1], '-', linewidth=2, label='clwl-dlis')
        plt.semilogy(clwlvsids[0], clwlvsids[1], '-', linewidth=2, label='clwl-vsids')
        plt.semilogy(clwlmoms[0], clwlmoms[1], '-', linewidth=2, label='clwl-moms')
        plt.semilogy(clwlrand[0], clwlrand[1], '-', linewidth=2, label='clwl-rand')
        plt.legend(loc = 2)
        plt.title('Comparaisons des heuristiques VSIDS, DLIS et MOMS avec Clause Learning et Watched Literals\n'+sys.argv[2])
        plt.grid(True)
        plt.xlabel('Numéro du test. Se référer à la ligne du fichier source csv')
        plt.ylabel('Temps en secondes (s)')
        ax = plt.subplot(111)
        #Shrink current axis by 20%
        box = ax.get_position()
        ax.set_position([box.x0, box.y0, box.width * 0.8, box.height])
        # Put a legend to the right of the current axis
        ax.legend(loc='center left', bbox_to_anchor=(1, 0.5))
        plt.show()

