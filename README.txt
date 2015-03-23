*********************************************************
*                                                       *
*                      MODULARITÉ                       *
*                                                       *
*********************************************************

Nous avons découpé les différentes parties de notre projet en modules/fichier.
Voici une arborescence de quelques dépendances entre les différentes parties :

Main
 |
 +-- Parser
 |    |
 |    +-- Lexer
 |
 +-- Checker
 |
 +-- Formula
 |    |
 |    +-- L : Literal_type
 |
 +-- Formula_wl
 |    |
 |    +-- L : Literal_type
 |
 +-- Dpll
      |
      +-- L : Literal_type
      |
      +-- F : Formula_type
      |
      +-- Graph
           |
           +-- F : Formula_type


*********************************************************
*                                                       *
*               CHOIX D'IMPLÉMENTATION                  *
*                                                       *
*********************************************************

Nous avons conservé la grande majoritée du code du rendu 1. Les modules Formula
et Formula_wl étant utilisés de manière similaire (Dpll est un functor), nous
avons pu ajouter l'apprentissage de clause de manière simultanée aux deux versions
(avec et sans litéraux surveillés).

La seule modification est au niveau du backtrack non-chronologique avec le
clause learning.

Le module Graph permet de generer la clause à apprendre à partir d'une formule fausse.


*********************************************************
*                                                       *
*                  EXECUTION DU CODE                    *
*                                                       *
*********************************************************

make # Compilation du code

cd tests
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

Avec l'ajout du clause learning, les temps d'exécution des différentes versions
ne sont plus comparables. Du plus efficace au moins efficace :
resol -cl -wl
resol -cl
resol
resol -wl

Nous avions déjà expliqué les différences de performances avec et sans litéraux
surveillés dans le rendu 1. La petite taille des clauses rend la version sans
litéraux surveillés plus rapide.
De plus l'ordre dans lequel les variables sont choisis est beaucoup plus pertinent
dans la version sans litéraux surveillés.

Avec l'apprentissage de clause, de grosses clauses sont potentiellement ajoutées ce qui
favorise la version avec litéraux surveillés. De plus le backtrack non-chronologique
rend superflu le choix d'un ordre efficace pour les paris sur les variables.

*********************************************************
*                                                       *
*                  IDÉE D'AMÉLIORATION                  *
*                                                       *
*********************************************************

Du fait que les différentes version peuvent être meilleures sur certains types de tests
et moins bonnes sur d'autres, il est difficile de faire une comparaison vraiment cohérente.
Il faut notamment pouvoir décider du degré de difficulté d'un test. Une idée est de concevoir
un générateur qui produit des tests de difficulté croissante en fonction d'un paramètre.
Les deux générateurs actuels sont trop spécifiques et ne sont probablement pas assez représentatifs.


*********************************************************
*                                                       *
*                  RÉPARTITION DU TRAVAIL               *
*                                                       *
*********************************************************

Nous avons commencé par discuter de la manière dont l'ajout de l'apprentissage de clause pouvait se
faire dans notre projet. Rémy s'est occupé de la génération du graphe, du calcul de la clause à apprendre, ...
Simon s'est occupé de l'intégration de ce module dans le code existant, du backtrack non-chronologique ainsi
que du debug.

Le partage des fichier s'est faite à l'aide du dépôt Git le l'association Aliens.

