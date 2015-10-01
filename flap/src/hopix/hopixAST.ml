(** The abstract syntax tree for hopix programs. *)

open Position

type program = definition located list

and definition =
  (** A toplevel definition for a value. *)
  | DefineValue of identifier located * expression located

and expression =
  (** A literal is a constant written "as is". *)
  | Literal of literal
  (** A variable identifies a value. *)
  | Variable of identifier
  (** A local definition [val p = e₁ ; e₂]. *)
  | Define of identifier located * expression located * expression located
  (** A function application [a b]. *)
  | Apply of expression located * expression located
  (** A conditional expression. *)
  | IfThenElse of expression located * expression located * expression located

and literal =
  | LInt  of int
  | LBool of bool (** Nouveau type LBool *)

and identifier =
  | Id of string

and t = program

