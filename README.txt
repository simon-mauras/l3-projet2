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

Une formule est un type mutable (Sigs.Formula_type.t) qui est modifiée pour éviter la duplication des données.
Les fonctions permettant de modifier une formule sont :
- setLiteral
- forgetLiteral
Il est possible de demander des informations sur une formule avec les fonctions
- isFalse
- getUnitClause
- getPureLiteral
- getFreeLiteral

*************** Version simple **************************

Nous avons choisis de représenter les clauses comme des ensembles de litéraux.
On implémente les ensembles avec des Set (arbres de recherche).
Chaque clause est :
- L'ensemble des litéraux dont la valeur (vrai ou faux) est déjà déterminée
- L'ensemble des litéraux dont la valeur n'est pas encore déterminée
- Le nombre de litéraux dont la valeur est vrai.
On dispose égalemet d'information sur les litéraux :
- L'état actuel du litéral (vrai | faux | non défini)
- Le nombre de clauses non satisfaites dans lesquelles le litéral intervient
- La liste des clauses dans lesquelles le litéral intervient

Avec ces informations il est possible d'implémenter les différentes opérations de manière efficace.

Pour le choix de la variable sur laquelle faire un paris, nous avons choisi de maximiser le nombre
de clauses non satisfaites dans lesquelles le litéral intervient. L'implémentation est aisée car
nous avons besoin de cette valeur dans une autre partie de l'algorithme.

*************** Version wl ******************************

Nous représentons les clauses par les informations suivantes :
* Des informations sur les clauses.
  - La liste des litéraux de la clause
  - Les litéraux surveillés dans cette clause (si sa taille est >= 2)
* Des information sur les litéraux surveillés.
  - La liste des clauses dans lesquelles le litéral est surveillé
* Des informations sur les litéraux.
  - Un booléen égal à vrai ssi le litéral a été mis à vrai
* Les réponses aux différentes requètes.

Les fonctions setLiteral et forgetLiteral modifient les valeurs des réponses.
Nous n'avons pas implémenter les litéraux purs à cause du temps de calculs nécessaire
lors de la rétropropagation, rendant inutile l'optimisation des litéraux surveillés.

Pour le choix de la variable sur laquelle faire un paris, nous commencons par trier les
variables par nombre d'occurence décroissant et d'utiliser cet ordre par la suite.

*********************************************************
*                                                       *
*              PRÉ-TRAITEMENT DE L'ENTRÉE               *
*                                                       *
*********************************************************

Dans le fichier checker.ml la fonctionn check vérifie que le nombre de clauses et
le nombre de variables est correct et affiche un warning si ce n'et pas le cas.
Puis supprime les clauses triviales (de la forme "x or not x or P") et affiche un
message en informant l'utilisateur

*********************************************************
*                                                       *
*               TRACABILITÉ ÉLÉMENTAIRE                 *
*                                                       *
*********************************************************

Lancer le programme avec l'option -debug permet d'afficher la formule initiale,
vérifier que le fichier a bien été parsé et que la formule a bien été initialisée.
A cause du grand nombre de calculs, nous avons choisis de ne pas afficher les paris et les déductions.

*********************************************************
*                                                       *
*                          TESTS                        *
*                                                       *
*********************************************************

Le script tests/test.sh permet de lancer l'exécutable sur tous les tests disponibles.
(Il est possible de rajouter l'option -wl pour utiliser les watched literals)

Sans les watched literals, tous les fichiers tests sont exécutés en moins d'une seconde.
Avec les watched literals, le premier test (tests/downloads/aim-50-1_6-no-1.cnf) s'exécute en une douzaine de minutes,
et les dernier (aim-50-6_0-*) s'exécute environ deux fois plus rapidement.

L'explication pour le premier test est que l'algorithme de base arrive à utiliser
un grand nombre de fois la propagation de litéraux purs (qui n'a pas été implémenté
dans la version litéraux surveillés).

Les temps d'exécution varient également car le choix de la prochaine variable est différent dans les deux implémentations.

On ne voit pas d'améliorations significatives car tous les fichier tests que nous utilisons
possèdent des clauses avec un nombre très limité de litéraux (généralement 3). Les litéraux
surveillés ont un intérêt lorsque la taille des clauses est suffisament grande.

*********************************************************
*                                                       *
*                  IDÉE D'AMÉLIORATION                  *
*                                                       *
*********************************************************



*********************************************************
*                                                       *
*                  RÉPARTITION DU TRAVAIL               *
*                                                       *
*********************************************************

Pour la répartition du travail, nous avons commencé par chercher comment organiser de manière modulaire notre code (lundi matin).
Rémy a commencé par déclarer les différents types et signatures du projet.
Le solver a été réalisé en majeur partie par Rémy et les modules permettant la manipulation des formules (avec et sans wl) par Simon.

Le partage des fichier s'est faite à l'aide du dépôt Git le l'association Aliens.

