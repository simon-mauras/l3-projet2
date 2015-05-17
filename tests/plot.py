#!/bin/python3
import sys
import csv
import matplotlib.pyplot as plt
from pylab import *

if(len(sys.argv) != 2):
        print("Usage : " + sys.argv[0] + " data.csv")
        sys.exit(1)


cldlis = [[], []]
clvsids = [[], []]
clmoms = [[], []]
clrand = [[], []]
wldlis = [[], []]
wlvsids = [[], []]
wlmoms = [[], []]
wlrand = [[], []]
clwldlis = [[], []]
clwlvsids = [[], []]
clwlmoms = [[], []]
clwlrand = [[], []]

with open(sys.argv[1], "rb") as file:
    reader = csv.DictReader(file,  delimiter=',')
    for row in reader:
        cldlis[0].append(cldlis[0].__len__())
        cldlis[1].append(row['cldlis'])
        clvsids[0].append(clvsids[0].__len__())
        clvsids[1].append(row['clvsids'])
        clmoms[0].append(clmoms[0].__len__())
        clmoms[1].append(row['clmoms'])
        clrand[0].append(clrand[0].__len__())
        clrand[1].append(row['clrand'])
        wldlis[0].append(wldlis[0].__len__())
        wldlis[1].append(row['wldlis'])
        wlvsids[0].append(wlvsids[0].__len__())
        wlvsids[1].append(row['wlvsids'])
        wlmoms[0].append(wlmoms[0].__len__())
        wlmoms[1].append(row['wlmoms'])
        wlrand[0].append(wlrand[0].__len__())
        wlrand[1].append(row['wlrand'])
        clwldlis[0].append(clwldlis[0].__len__())
        clwldlis[1].append(row['clwldlis'])
        clwlvsids[0].append(clwlvsids[0].__len__())
        clwlvsids[1].append(row['clwlvsids'])
        clwlmoms[0].append(clwlmoms[0].__len__())
        clwlmoms[1].append(row['clwlmoms'])
        clwlrand[0].append(clwlrand[0].__len__())
        clwlrand[1].append(row['clwlrand'])


with plt.style.context('fivethirtyeight'):
        figure(1)
        plt.semilogy(cldlis[0], cldlis[1], '-', linewidth=2, label='cl-dlis')
        plt.semilogy(clvsids[0], clvsids[1], '-', linewidth=2, label='cl-vsids')
        plt.semilogy(clmoms[0], clmoms[1], '-', linewidth=2, label='cl-moms')
        plt.semilogy(clrand[0], clrand[1], '-', linewidth=2, label='cl-rand')
        plt.legend(loc = 2)
        plt.title('Comparaisons des heuristiques VSIDS, DLIS et MOMS avec Clause Learning')
        plt.xlabel('Numéro du test. Se référer à la ligne du fichier source csv')
        plt.ylabel('Temps en secondes (s)')
        plt.grid(True)
        ax = plt.subplot(111)
        #Shrink current axis by 20%
        box = ax.get_position()
        ax.set_position([box.x0, box.y0, box.width * 0.8, box.height])
        # Put a legend to the right of the current axis
        ax.legend(loc='center left', bbox_to_anchor=(1, 0.5))
        plt.show()

        figure(2)
        plt.semilogy(wldlis[0], wldlis[1], '-', linewidth=2, label='wl-dlis')
        plt.semilogy(wlvsids[0], wlvsids[1], '-', linewidth=2, label='wl-vsids')
        plt.semilogy(wlmoms[0], wlmoms[1], '-', linewidth=2, label='wl-moms')
        plt.semilogy(wlrand[0], wlrand[1], '-', linewidth=2, label='wl-rand')
        plt.legend(loc = 2)
        plt.title('Comparaisons des heuristiques VSIDS, DLIS et MOMS avec Watched Literals')
        plt.xlabel('Numéro du test. Se référer à la ligne du fichier source csv')
        plt.ylabel('Temps en secondes (s)')
        plt.grid(True)
        ax = plt.subplot(111)
        #Shrink current axis by 20%
        box = ax.get_position()
        ax.set_position([box.x0, box.y0, box.width * 0.8, box.height])
        # Put a legend to the right of the current axis
        ax.legend(loc='center left', bbox_to_anchor=(1, 0.5))
        plt.show();

        figure(3)
        plt.semilogy(clwldlis[0], clwldlis[1], '-', linewidth=2, label='clwl-dlis')
        plt.semilogy(clwlvsids[0], clwlvsids[1], '-', linewidth=2, label='clwl-vsids')
        plt.semilogy(clwlmoms[0], clwlmoms[1], '-', linewidth=2, label='clwl-moms')
        plt.semilogy(clwlrand[0], clwlrand[1], '-', linewidth=2, label='clwl-rand')
        plt.legend(loc = 2)
        plt.title('Comparaisons des heuristiques VSIDS, DLIS et MOMS avec Clause Learning et Watched Literals')
        plt.xlabel('Numéro du test. Se référer à la ligne du fichier source csv')
        plt.ylabel('Temps en secondes (s)')
        plt.grid(True)
        ax = plt.subplot(111)
        #Shrink current axis by 20%
        box = ax.get_position()
        ax.set_position([box.x0, box.y0, box.width * 0.8, box.height])
        # Put a legend to the right of the current axis
        ax.legend(loc='center left', bbox_to_anchor=(1, 0.5))
        plt.show()

