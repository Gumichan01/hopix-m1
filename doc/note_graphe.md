
# Algorithme de coloration de graphe #

  Soit ***G*** un graphe que l'on veut colorier avec K couleurs (K entier).
Il y a 3 cas :
 - G est vide !
 - Il existe ***n*** tel que ***degré(n) < K → G \\ {n}*** puis choisir une
 couleur ***n*** qui n'est n'est pas utilisée par un de ses voisins.
 - Il existe ***n*** tel que ***degré(n) >= K → G \\ {n}*** puis,  
   S'il existe une couleur disponible :
   - on la prend pour ***n***
   - Sinon on marque n comme une variable de pile


type Edge = { label : string ; degree : int ; neighbour : Edge list};
type Graph = Edge list;
