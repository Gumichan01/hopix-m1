type t = int

exception LiteralExceeds16bits of int

(** [check_invariant x] ensures that the integer [x] is a valid
    representation for a 16 bits signed integer. *)
let check_invariant x =
  failwith "TODO int16 check_invariant"

(** [hi x] returns the 16 highest bits of [x]'s 32 bits. *)
let hi x =
  failwith "TODO int16 hi"

(** [low x] returns the 16 lowests bits of [x]'s 32 bits. *)
let low x =
  failwith "TODO int16 low"

(** [of_int x] turns an OCaml integer literal into a 16 bits literal. *)
let of_int x =
  check_invariant x;
  x

(** [of_int32 x] turns an OCaml integer literal into a 16 bits literal. *)
let of_int32 x =
  of_int (Int32.to_int x)

(** [to_string x] turns an integer [x] into a string. *)
let to_string x =
  string_of_int x
