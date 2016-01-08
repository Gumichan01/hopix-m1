(*

  e ::= n | e + e | e * e

*)

type e =
  | Int of int (* e ::= n *)
  | Plus of e * e (* e ::= e + e *)
  | Mul of e * e (* e ::= e * e *)
