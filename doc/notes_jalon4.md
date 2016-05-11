

    type t := {K};

environnement_initial : empty;
ConstructorMap.add(K,0);      // entier unique pour chaque constructeur

// Pour les *label*, l'entier n'est pas unique.

    type D {l : int; a : int};

exemple 1 :

    l : 0
    a : 1

exemple 2 :

    val e := {l:=3 ; a:=5}

sera transformé en val e := Block[3,5]

AllocateBlock puis WriteBlock

> EXERCICE n° 1

Soit le programme suivant :

    type t := { x : int; y : int }.     // p1
    val r := { x = 32; y = 24}.         // p2
    val rx := r#x.                      // p3
    val change := r#x <- 2;             // p4

Code Hobix équivalent :

    p1 : ...
    p2 : r := AllocateBlock(2); \_1 := Writeblock(r,0,32); \_2 := Writeblock(r,1,24); r;
    p3 : ReadBlock(0)
    p4 : WriteBlock(ReadBlock(0),2)    // écrire à l'addresse 0 (dans *x*) la valeur 2


Compilation Hopix ->hobix

val n := { x = 32 ; y = 1024 ; z = 16}#x.



--
