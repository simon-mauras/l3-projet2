*********************************************************
*                                                       *
*                      MODULARITÉ                       *
*                                                       *
*********************************************************

Nous avons découpé les différentes parties de notre projet en modules/fichier.
Voici une arborescence de quelques dépendances entre les différentes parties :

Main
 |
 +-- Sigs
 |    |
 |    +-- Literal / Literal2
 |
 +-- Tseitin
 |
 +-- Mode_cnf : Mode_type
 |    |
 |    +-- Lexer
 |    +-- Parser
 |    +-- Checker
 |    +-- Theory_default
 |
 +-- Mode_tseitin : Mode_type
 |    |
 |    +-- Lexer_tseitin
 |    +-- Parser_tseitin
 |    +-- Theory_default
 |
 +-- Mode_equality : Mode_type
 |    |
 |    +-- Lexer_equality
 |    +-- Parser_equality
 |    +-- Theory_default
 |
 +-- Mode_equality : Mode_type
 |    |
 |    +-- Lexer_equality
 |    +-- Parser_equality
 |    +-- Theory_default
 |
 +-- Mode_equality : Mode_type
 |    |
 |    +-- Lexer_equality
 |    +-- Parser_equality
 |    +-- Theory_default
 |
 +-- Formula
 |
 +-- Formula_wl
 |
 +-- Main (F : Formula_type) (M : Mode_type)
      |
      +-- Dpll (F : Formula_type)
           |
           +-- Graph (F : Formula_type)
           |
           +-- Graph2 (F : Formula_type)
           
Depuis le dernier rendu nous avons fait passer le module Literal en global,
avoir trop de foncteurs rendait l'implémentation des théories difficile (typage, ...)

De plus pour pouvoir utiliser plusieurs lexers/parsers et plusieures théories de manière générique,
nous avons ajouter un foncteur au main qui prend en paramètre le mode d'exécution souhaité.

Le module tseitin est utilisée par chaque logique, c'est une couche intermédiaire entre le parsing et la théorie 

Nous avons également réimplémenté l'apprentissage de clause. Le module Graph a une complexité quadratique
en le nombre de variable ce qui est beaucoup trop pour nos tests sur les différentes théories.
La version Graph2 est linéaire en le nombre de variables dans la clause apprise mais ne permet
pas d'exporter le graphe en mode interactif (ce qui n'est pas gênant...)


*********************************************************
*                                                       *
*               CHOIX D'IMPLÉMENTATION                  *
*                                                       *
*********************************************************

--------------- TSEITIN ---------------------------------

Le module tseitin utilise une map (abr) pour référencer les atomes (litéraux)
Il se contente de prendre une formule quelconque et d'en retourner une sous forme
normale conjonctive en appliquant la transformation demandée.
Nous avons utilisé une table de Karnaugh pour obtenir l'équivalent de l'implication :
(C = A => B) <=> ((-C \/ -A \/ -B) /\ (C \/ A) /\ (C \/ -B))

--------------- EGALITÉ ---------------------------------

Nous avons implémenté l'union find décrit dans le fichier EgaliteCongruence.pdf
Les deux points clés sont le backtrack et l'obtention de la contradiction.
- Pour le backtrack nous avons opté pour ne pas utiliser la compression de chemin.
  Nous stockons pour chaque lien [a -> find.(a)] sa cause (litéral).
- Pour obtenir la clause il nous faut faire attention au cas suivant :
  {a -> a, b -> b, c -> c}
  b = c
  {a -> a, b -> b, c -> b (à cause de b = c)}
  a = c
  {a -> a, b -> a (à cause de a = c), c -> b (à cause de b = c)}
  a != b
  Si on se contente de remonter l'arbre on obtient comme conflit (a != b /\ a = c)
  Il nous faut donc redescendre dans l'arbre pour obtenir le bon conflit.
  
--------------- CONGRUENCE ------------------------------

Nous avons adapté la théorie de l'égalité en utilisant l'algorithme décrit dans le
fichier CongruenceClosure.pdf. Cependant au lieu d'avoir recours à des fonctions de
hashage nous avons utilisé la relation d'ordre disponible par défaut (Pervasives.compare)
afin de garder une complexité linéaire dans la fonction merge.

Il est fort probable qu'en activant le clause learning les résultats soient faux car nous
n'avons pas eu le temps de généraliser la technique décrite pour obtenir une contradiction
dans la théorie de l'égalité.

--------------- DIFFERENCE ------------------------------

Nous avons utilisé l'idée exposée dans le fichier Differences.pdf.
Nous n'avons pas trouvé d'algorithme incrémental efficace pour implémenter la théorie de la différence.
L'algorithme utilisé est donc celui de Bellman-Ford avec une dernière itération qui permet de détecter
un éventuel cycle de poids négatif.
Pour obtenir la contradiction à partir de l'algorithme de Bellman-Ford, une méthode (qui peux ne pas fonctionner)
est d'initialiser tous les poids à 0 et de retenir pour chaque noeud son prédecesseur. En effet des modifications
peuvent être faites simultanément à pusieurs endroits du graphe à chaque itération.
Nous avons donc utilisé le partage de donnée persistantes d'OCaml pour garder pour chaque noeud le chemin qui permet
d'y accéder. (Le partage des données assure que la complexité temporelle est conservée)


*********************************************************
*                                                       *
*                  EXECUTION DU CODE                    *
*                                                       *
*********************************************************

Pour tester la correction de l'algorithme DPLL (rendu 2)

make # Compilation du code

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
*                          TESTS                        *
*                                                       *
*********************************************************

Comment tester les différentes logiques ? Nous avons essayé de trouver des tests dont la solution a un sens.

Exemples de test :
--------------------------------------------------------
# Ici nous avons opté pour la résolution d'un sudoku.
# Un petit générateur en OCaml génère un fichier test (.equ)
cd tests/equality/generator
make
time ../../../resol -equality -wl -cl wrong_sudoku.equ # UNSATISFIABLE, 0.35s
time ../../../resol -equality -wl -cl complete_sudoku.equ complete_sudoku.out # SATISFIABLE, 0.40s
time ../../../resol -equality -wl -cl incomplete_sudoku.equ incomplete_sudoku.out # SATISFIABLE, 6.3s
diff incomplete_sudoku.out complete_sudoku.out # The sudoku is correctly solved !
rm *.out # Delete temporary files
--------------------------------------------------------
# Ici pour tester la correction de la théorie, on calcule (et on vérifie) si deux entiers sont premiers entre eux
cd tests/congruence/generator
./coprime.sh # The theory is correctly implemented
--------------------------------------------------------


*********************************************************
*                                                       *
*                  IDÉE D'AMÉLIORATION                  *
*                                                       *
*********************************************************

L'implémentation de la théorie de la différence n'est pas vraiment incrémentale, de nombreux calculs sont
fait à chaque appel de la fonction getContradiction. Il peut être possible de ne regarder que les chemins
du graphe utilisant au moins un literal ayant été mis à vrai depuis le dernier appel.

La contradiction renvoyée par la théorie de la congruence peut être fausse (des littéraux manquent pour obtenir une contradiction)


*********************************************************
*                                                       *
*                  RÉPARTITION DU TRAVAIL               *
*                                                       *
*********************************************************

Rémy s'est occupé du module tseitin et de la logique de la différence
Simon s'est occupé de la logique de l'égalité/congruence ainsi que de la connexion entre les différentes parties.

Le partage des fichier s'est faite à l'aide du dépôt Git le l'association Aliens.

