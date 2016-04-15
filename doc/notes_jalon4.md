

type t := {K};

environnement_initial : empty;
ConstructorMap.add(K,0);      // entier unique pour chaque constructeur

// Pour les *label*, l'entier n'est pas unique.

type D {l : int; a : int};

exemple :

l : 0
a : 1

exemple 2 :

val e := {l:=3 ; a:=5} sera transforme en val e := Block[3,5]

AllocateBlock puis WriteBlock
