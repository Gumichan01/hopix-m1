(** This module implements a type checker for Datix. *)
open HopixAST
open HopixTypes
open HopixPrettyPrinter

let initial_typing_environment = HopixTypes.initial_typing_environment
type typing_environment = HopixTypes.typing_environment

let type_error = Error.error "typechecking"

let located f x = f (Position.position x) (Position.value x)

module SimpleTypes = struct

  (* Check if a definition is annotated *)
  let rec is_def_annotated = function
    | DeclareExtern(_,_) | DefineType(_,_,_) -> ()
    | DefineValue(_,ex_l) -> is_expression_annotated (Position.value ex_l)
    | DefineRecValue(l) -> is_rec_value_annotated l


  (* Check if an expression is annotated *)
  and is_expression_annotated = function
    | TypeAnnotation(_,_) -> ()

    | Define(_,e1_l,e2_l)
    | Apply(e1_l,e2_l) -> is_expr_list_annotated [e1_l;e2_l];

    | DefineRec(l,ex_l) -> is_rec_value_annotated l;

    | IfThenElse(c_l,e1_l,e2_l) -> is_expr_list_annotated [c_l;e1_l;e2_l]

    | Fun(p_l,ex_l) -> is_pattern_annotated (Position.value p_l);
		       is_expression_annotated (Position.value ex_l)

    | Tagged(_,l) -> is_expr_list_annotated l

    | Case(ex_l,l) ->
       is_expression_annotated (Position.value ex_l);
       is_branch_list_annotated (l)

    | _ -> failwith ("This expression is not annotated")

  (* Check if every expressions in a list are annotated *)
  and is_expr_list_annotated = function
    | [] -> ()
    | e_l::q -> is_expression_annotated (Position.value e_l);
		is_expr_list_annotated q

  (* Check if a pattern is annotated *)
  and is_pattern_annotated = function
    | PTypeAnnotation(_,_) -> ()
    | _ -> failwith "This pattern is not annotated"

  (* Check if every branches in the list are annotated *)
  and is_branch_list_annotated = function
    | [] -> assert false (* by typing *)
    | [b_l] -> is_branch_annotated (Position.value b_l)
    | br_l::q -> is_branch_annotated (Position.value br_l);
		 is_branch_list_annotated q

  (* Check if a branch is annotated *)
  and is_branch_annotated = function
    | Branch(p_l,ex_l) ->
       let (p,ex) = (Position.value p_l,Position.value ex_l) in
       is_pattern_annotated p;
       is_expression_annotated ex
    | _ -> failwith ("This branch is not annotated")

  (* Check if mutually recursive values are annotated *)
  and is_rec_value_annotated = function
    | [] -> ()
    | (_,ex_l)::q -> is_expression_annotated (Position.value ex_l);
		     is_rec_value_annotated q



  (** A program is fully annotated if the programmer wrote a type
      annotation on variables of patterns and has given the return
      type of recursive functions. The following function checks if an
      input program is fully annotated. If not, a type error is
      issued. *)
  let check_program_is_fully_annotated prog =
    let rec aux_check =
      (fun p -> match p with
		| [] -> ()
		| def_l::q -> is_def_annotated (Position.value def_l);
			      aux_check q)
    in aux_check prog


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

      | DefineRecValue(l) -> rec_definition tenv pos l

      | DefineType(_,_,_) -> failwith("TODO DefineType")

    (* Separated function that deal with recursive value*)
    and rec_definition tenv pos = function
      | [] -> tenv
      | (id_l,ex_l)::q ->
	 let d_val = DefineValue(id_l,ex_l) in
	 let new_tenv = definition tenv pos (d_val) in
	 rec_definition new_tenv pos q

    (* A definition is well formmed if what?  *)
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
      | Define(id_l,e1_l,e2_l) -> (*check_expr_rec tenv xty pos [e2_l;e1_l]*)
	 (*let new_tenv = check_expression tenv xty pos (Position.value e1_l) in
	 let ntenv = (check_expression new_tenv xty pos (Position.value e2_l)) in
	 ()*)
	 failwith("check_expression : TODO Define")
      | DefineRec(_,_) -> failwith("check_expression : TODO DefineRec")
      | Apply(e1_l,e2_l) -> (*check_expr_rec tenv xty pos [e2_l;e1_l]*)
	 failwith("check_expression : TODO Apply")
      | IfThenElse(_,_,_) -> failwith("check_expression : TODO IfThenElse")
      | Fun(_,_) -> failwith("check_expression : TODO Fun")
      | Tagged(_,_) -> failwith("check_expression : TODO Tagged")
      | Case(_,_) -> failwith("check_expression : TODO Case")

      | TypeAnnotation(e_l,ty_l) -> check_expression tenv (Position.value ty_l) pos (Position.value e_l)

      | Field(_,_) -> failwith("check_expression : TODO Field")
      | ChangeField(_,_,_) -> failwith("check_expression : TODO Field")
      | _ -> assert(false)


(*    and check_expr_rec tenv xty pos = function
      | [] -> ()
      | e_l::q ->
	 (
	   let ntenv =  check_expression tenv xty pos (Position.value e_l) in
	   check_expr_rec ntenv xty pos q
	 )*)

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
      type error message is issued.

      The solving process is based on first-order unification.
      (See: https://en.wikipedia.org/wiki/Unification_(computer_science))

      The first-order unification algorithm rewrites the type
      equalities using 6 simplification rules (described below) as
      long as possible. Once finished, this rewriting process leads
      to three situations:

      1. The type equalities are of the form [∧ᵢ Xᵢ = Tᵢ],
         this form also encodes a substitution (which may or
         may not be ground).

         Also, ∀i j, Xᵢ ∉ FV(Tⱼ).

         (Be careful, this does not mean that the resulting substitution
          is ground...)

      2. The type equalities are inconsistent (written ⊥).
         (The program is ill-typed.)

      3. The type equalities contain a cycle.
         (The program is also ill-typed.)

      Here are the 6 simplification rules that must be used to
      simplify the initial constraint:

      - [delete]

                        c = (t, t) :: c' ———–→ c'

        If an equality simply says that a type is equal to itself,
        it can be removed since it is not informative.

      - [decompose]

          c = (TyCon (a, tys1) = TyCon (a, tys2)) :: c' ———–→ tys1 = tys2 @ c'

        Two types made from the same type constructors are equal if and
        only if their arguments are equal.

      - [conflict]

                c = (TyCon (a, tys1) = TyCon (b, tys2)) :: c' ———–→ ⊥

        Two types made from distinct type constructors are equal is an
        inconsistency: the constraint is equivalent to ⊥.

      - [swap]

       c = (TyCon (a, ts) = TyVar x) :: c' ———–→ (TyVar x = TyCon (a, ts) :: c'

        We orient equalities to have equalities of the form X = T in the end.

      - [eliminate]

                c = (TyVar x = t) :: c' ———–→ (TyVar x = t) :: c' { TyVar x ↦ t }

        Once we know that [x] is equal to [T], every occurence of [x] in other
        equalities can be removed.

       - [occur-check]

                c = (TyVar x = TyCon (a, ts)) :: c' ———–→ ⊥     if x ∈ FV(ts)

        The equalities encode a cycle: the constraint is not
        satisfiable in the model of trees (and our types are syntactic
        representatives of this model).

      To implement the solver, you can either:

      - implement the 6 simplification rules and a function that goes
        through the list of equalities to apply them *in the right
        order*, you will get a naive but sound algorithm ;

      - or, you can implement a more sophisticated algorithm as described in
            Franz Baader and Tobias Nipkow,
            Term Rewriting and All That. Cambridge University Press, 1998.
        This algorithm is based on the UnionFind data structures and offers
        a linear complexity thanks to an optimisation of the [eliminate]
        simplification rule.

  *)
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
  if not (Options.get_check_types ()) then
    tenv
  else if Options.get_infer_types () then
    SimpleTypes.infer tenv ast
  else
    SimpleTypes.typecheck tenv ast

let print_typing_environment =
  HopixTypes.print_typing_environment
