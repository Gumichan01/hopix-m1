DONE :

vdefinition ::= val var_id := expr (1)
| rec var_id := expr
  { and var_id { pattern }[ : type ] := expr } (1) et (2)

(1) : La partie optionnelle n'a pas encore été faite
(2) : Fonctionne normalement.

NB : Si une erreur survient dans l'interpréteur à cause de DefineRecValue(),  
c'est normal.
