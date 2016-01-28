(** This module implements a type checker for Datix. *)
open HopixAST
open HopixTypes
open HopixPrettyPrinter

let initial_typing_environment = HopixTypes.initial_typing_environment
type typing_environment = HopixTypes.typing_environment

let type_error = Error.error "typechecking"

let located f x = f (Position.position x) (Position.value x)

module SimpleTypes = struct

  (** A program is fully annotated if the programmer wrote a type
      annotation on variables of patterns and has given the return
      type of recursive functions. The following function checks if an
      input program is fully annotated. If not, a type error is
      issued. *)
  let check_program_is_fully_annotated =
     failwith "Students! This is your job!"

  let undress_expression = function
    | TypeAnnotation (e, ty) ->
      (e, ty)
    | _ ->
      assert false (* by is_fully_annotated. *)

  (**
     [typecheck tenv p] returns the typing environment
     augmented with the variables defined by the program.

     We first check that [p] is fully annotated. Then,
     the algorithm is meant to check that annotation are
     consistent.

     For programs and definitions, the algorithm is a straightforward
     traversal of the abstract syntax tree. Each node of the abstract
     syntax tree is locally checked for welltypedness: indeed, there
     is exactly one typing rule per syntactic construction and this
     rule only requires the subexpressions have been recursively
     checked.

     For expressions, things are a bit more complicated since some
     nodes of the abstract syntax tree do not carry a type annotation.
     For these nodes, we do not *check* that the expression has some
     type but we *compute* a type instead. Therefore, we must
     implement two mutually recursive functions: one that checks if an
     input expression has a specific type (in that case, the
     expression and the type are inputs) and another one which compute
     a type for a given expression (in that casse, the expression is
     the input and the type is the output).

  *)

  let typecheck tenv p =
    check_program_is_fully_annotated p;

    let rec program tenv p =
      List.fold_left (fun tenv -> located (definition tenv)) tenv p

    and definition tenv pos = function
      | DeclareExtern (x, ty) ->
	bind_value_type (Position.value x) (Position.value ty) tenv

      | DefineValue (x, e) ->
	(*

	            Γ ⊢ e : τ
	   ————————————————————————————
	   Γ ⊢ x := (e : τ) → Γ (x : τ)

	*)
	let e, pos = Position.destruct e in
	let (e, ty) = undress_expression e in
	located (check_expression tenv (Position.value ty)) e;
	bind_value_type (Position.value x) (Position.value ty) tenv

      | DefineType (tcon, ts, tdef) ->
	well_formed_type_definition tenv ts tdef;
	bind_type_definition
	  (Position.value tcon)
	  (List.map Position.value ts)
	  tdef
	  tenv

    and well_formed_type_definition tenv ts tdef =
	 failwith "Students, this is your job!"

    and check_expression tenv xty pos = function
      | Literal x ->
	check_types pos xty (located (literal tenv) x)
      | Variable x ->
	let x, pos = Position.destruct x in
	begin match lookup_value_type x tenv with
	  | Some ity ->
	    check_types pos xty ity
	  | None ->
	    let Id s = x in
	    type_error pos (Printf.sprintf "Unbound identifier `%s'." s)
	end

    (** [compute_expression_type tenv pos e] traverses [e] and tries
	to compute a type from the user type annotations and the
	shape of the input expression. *)
    and compute_expression_type tenv pos =
      failwith "Students, this is your job!"

    and literal tenv pos = function
      | LInt    _ ->
	PrimitiveTypes.int
      | LString _ ->
	PrimitiveTypes.string
      | LChar _ ->
	PrimitiveTypes.char
      | LBool _ ->
	PrimitiveTypes.bool

    and check_types pos xty ity =
      if xty <> ity then
	type_error pos
	  (Printf.sprintf "Expecting: %s\n  Inferred: %s"
	     (to_string ty xty)
	     (to_string ty ity))
    in
    program tenv p

  let make_fresh_type_variable =
    let c = ref 0 in
    fun () ->
      incr c;
      TyVar (TId ("x" ^ string_of_int !c))

  (** [annotate p] completes [p] with enough type annotations to
      make it a fully annotated program. To generate a new type
      annotation, use [make_fresh_type_variable] defined below. *)
  let annotate : program -> program = fun p ->
       failwith "Students, this is your job!"

   (** A typing constraint is a conjunction of type equalities. *)
   type typing_constraint = (ty * ty) list

   (** [generate_constraint tenv p] takes a fully annotated program
       and generate typing constraints that are equivalent of
       its welltypedness. *)
   let generate_constraint : typing_environment -> program -> typing_constraint = fun tenv p ->
       failwith "Students, this is your job!"

   (** A type substitution maps type variable to types. *)
   type substitution = (type_variable, ty) Dict.t

   (** A type is ground if it does not contain any type variable. *)
   let rec ground_type pos = function
     | TyCon (_, tys) -> List.for_all (located ground_type) tys
     | _ -> false

   (** A substitution is ground if its image is only composed of
       ground types. *)
   let ground_substitution : substitution -> bool =
     fun phi ->
       Dict.to_list phi
       |> List.for_all (fun (_, ty) -> ground_type Position.dummy ty)

  (** [solve_constraint c] simplifies the typing constraint [c] and
      returns a substitution equivalent to [c] if [c] is
      satisfiable and has a unique ground solution. Otherwise, a
      type error message is issued. *)
   let solve_constraint : typing_constraint -> substitution = fun c ->
       failwith "Students, this is your job!"

  (** [elaborate phi p] takes a ground substitution [phi] and
      turns [p], a fully annotated program coming from the
      [annotate] function, into a program annotated only with
      ground types. *)
   let elaborate : substitution -> program -> program = fun phi p ->
       failwith "Students, this is your job!"

   (** [infer tenv p] performs type inference on the program [p]. *)
   let infer tenv p =
     let p = annotate p in
     let c = generate_constraint tenv p in
     let phi = solve_constraint c in
     let p = elaborate phi p in
     typecheck tenv p

end

(** [typecheck tenv ast] checks that [ast] is a well-formed program
    under the typing environment [tenv]. *)
let typecheck tenv ast =
  if Options.get_infer_types () then
    SimpleTypes.infer tenv ast
  else
    SimpleTypes.typecheck tenv ast

let print_typing_environment =
  HopixTypes.print_typing_environment
