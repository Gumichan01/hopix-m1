
# Algorithme de coloration de graphe #

  Soit ***G*** un graphe que l'on veut colorier avec K couleurs (K est un entier).
Il y a 3 cas :
 - G est vide !
 - Il existe ***n*** tel que ***degré(n) < K → G \\ {n}*** puis choisir une
 couleur ***n*** qui n'est n'est pas utilisée par un de ses voisins.
 - Il existe ***n*** tel que ***degré(n) >= K → G \\ {n}*** puis,  
   S'il existe une couleur disponible :
   - on la prend pour ***n***
   - Sinon on marque n comme une variable de pile


type Edge = { label : string ; degree : int ; neighbour : Edge list };
type Graph = Edge list;

> Graphe

*next_node_id* : valeur à donner au prochain noeud
*node_of_label* : nom du noeud -> entier
*labels* : entier -> node_of_label
*neighbours* : map définissant arête avec les voisins
*degrees* : map associant les noeud à un degré donnée

> Hors-sujet (Hobix ->  Fopix)

Fonctions mutuellement récursives : environnement mutuellement recursives
