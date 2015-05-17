*********************************************************
*                                                       *
*                      MODULARITÉ                       *
*                                                       *
*********************************************************

Depuis le dernier rendu nous avons ajouté la possibilité d'avoir une documentation
du projet (avec ocamldoc). La commande "make doc" permet de générer une doc à ouvrir
avec un navigateur internet (manual.docdir/index.html) 


*********************************************************
*                                                       *
*                          COQ                          *
*                                                       *
*********************************************************

Nous avons préféré la partie 4 (Coq) à la partie 3 (Simplexe). Malheureusement l'export
d'une preuve n'est pas encore possible. Les différentes preuves nous ont en effet pris
un temps conséquent.

Il est possible de regarder ce à quoi aurait du ressembler une preuve exportée.

Dans le cas satisfiable, il suffit d'exporter le certificat (dans la fonction v du fichier sat.v)
Coq etant capable de simplifier une formule, il est facile de montrer la satisfiabilité d'une formule
ou d'une liste de clauses.

Dans le cas non satisfiable nous avons réfléchi à la manière de prouver qu'une liste de clause n'est
pas satisfiable. Une des méthodes est de procéder par déduction successive des clauses apprises.
En effet les clauses initiales permettent dans ce cas de déduire la clause vide qui est par définition
non satisfiable. Un des écueils est de raisonner par l'absurde (Coq refuse les preuves non constructives)
C'est la partie que nous avons pas eu le temps de faire.

Une fois que nous sommes capables de faire ça de manière automatique se pose la question de prouver
qu'une formule (sous forme arborescente) n'est pas satisfiable. C'est la grosse partie que nous avons aborder,
à savoir la preuve que la transformation de tseitin converve le caractère satisfiable.
Un exemple de formule non satisfiable est donné dans le fichier unsat.v.


*********************************************************
*                                                       *
*                  EXECUTION DU CODE                    *
*                                                       *
*********************************************************

Pour tester la correction de l'algorithme DPLL (rendu 2)

make # Compilation du code
make doc # Génération de la doc

cd tests/cnf
./tests.sh # Test des exemples basiques

cd downloads
make # Récupération de tests plus gros
cd ..

cd generator
make # Génération de tests
cd ..

./tests.sh # Test sur tous les exemples
nano tests.sh # Ligne 5 : Par défaut le script utilise les options -wl -cl 


*********************************************************
*                                                       *
*                        MOULINETTES                    *
*                                                       *
*********************************************************

Nous avons tracé différents graphes. Dans un premier temps nous avons testé
les options -cl et -wl sur des "petits" tests. La version la plus rapide étant
de loin -wl -cl, nous avons testé ensuite les heuristiques sur des tests plus
conséquents avec les litéraux surveillés et l'apprentissage de clause activés.

Pour tracer les courbes nous avons utilisé python3, numpy et matlibplot.
Il est possible de les tracer en lancant le script tests/benchmark.sh


*********************************************************
*                                                       *
*                  RÉPARTITION DU TRAVAIL               *
*                                                       *
*********************************************************

Rémy a implémenté les différentes heuristiques et a lancé les moulinettes (graphes, ...)
Simon a d'incorporé les heuristiques à l'algo DPLL et s'est occupé de la partie Coq.
Le partage des fichier s'est faite à l'aide du dépôt Git le l'association Aliens.

