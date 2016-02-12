(** The abstract syntax tree for hobix programs. *)

(** A program is a list of definitions. *)
type program = definition list

and definition =
  (** A toplevel declaration for an external value. *)
  | DeclareExtern of identifier
  (** A toplevel definition for a value. *)
  | DefineValue of identifier * expression
  (** A toplevel definition for mutually recursive values. *)
  | DefineRecValue of (identifier * expression ) list

and expression =
  (** A literal is a constant written "as is". *)
  | Literal of literal
  (** A variable identifies a value. *)
  | Variable of identifier
  (** A local definition [val x₁ := e₁ ; e₂]. *)
  | Define of identifier * expression * expression
  (** Local mutually recursive values [rec x₁ := e₁ and ... and xₙ := eₙ; e]. *)
  | DefineRec of (identifier * expression) list * expression
  (** A function application [a b]. *)
  | Apply of expression  * expression
  (** A conditional expression of the form [if ... then ... else ... fi]. *)
  | IfThenElse of expression * expression * expression
  (** An anonymous function [ \ x => e ]. *)
  | Fun of identifier * expression

  | AllocateBlock of expression
  | WriteBlock of expression * expression * expression
  | ReadBlock of expression * expression

and literal =
  | LInt    of Int32.t
  | LString of string
  | LChar   of char
  | LBool   of bool

and identifier =
  | Id of string

and t = program

