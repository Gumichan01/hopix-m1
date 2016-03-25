(** From Hopix to Hobix *)

module Source = Hopix
module Target = Hobix

(** The compilation environment.
    ———————————————————————————–

    To translate a program written in a source language into another
    semantically equivalent program written in a target language, it
    is convenient to carry some information about the correspondence
    between the two programs along the process. The compilation
    environment is meant to that.

    In this particular pass, we want to remember an assignment of
    integers to constructor and label identifiers. Therefore, the
    compilation environment is composed of two maps representing these
    assignments. The environment is populated each time we cross a
    type definitions while it is read each time we translate language
    constructions related to record and tagged values.
*)

module ConstructorMap = Map.Make (struct
  type t = HopixAST.constructor
  let compare = compare
end)

module LabelMap = Map.Make (struct
  type t = HopixAST.label
  let compare = compare
end)

type environment = {
  constructor_tags : Int32.t ConstructorMap.t;
  label_position   : Int32.t LabelMap.t
}

let initial_environment () = {
  constructor_tags = ConstructorMap.empty;
  label_position = LabelMap.empty
}

let index_of_constructor env k =
  ConstructorMap.find k env.constructor_tags

let index_of_label env l =
  LabelMap.find l env.label_position

let add_label key value map = 
  LabelMap.add key value map

(** Code generation
    ———————————————


    A compilation pass produces code. We could directly
    write down caml expressions made of applications of
    HobixAST constructors. Yet, the resulting code would
    be ugly...

    A better way consists in defining functions that build
    Hobix AST terms and are convenient to use. Here are a
    list of functions that may be convenient to you when
    you will implement this pass.

*)

(** [fresh_identifier ()] returns a fresh identifier, that is
    an identifier that has never been seen before. *)
let fresh_identifier =
  let r = ref 0 in
  fun () -> incr r; HobixAST.Id ("_" ^ string_of_int !r)

(** [def w (fun x -> e)] returns an abstract syntax tree of
    the form:

    val x = w; e

    where [x] is chosen fresh.
*)
let def w f =
  let x = fresh_identifier () in
  HobixAST.Define (x, w, f x)

(** [defines [d1; ..; dN] e] returns an abstract syntax tree of
    the form:

    val d1;
    ..
    val dN;
    e

*)
let defines =
  List.fold_right (fun (x, xe) e -> HobixAST.Define (x, xe, e))

(** [seq s1 s2] is

    val _ = s1;
    s2

*)
let seq s1 s2 =
  HobixAST.Define (fresh_identifier (), s1, s2)

(** [seqs [s1; ...; sN] is

    val _ = s1;
    ...
    val _ = s(N - 1);
    sN
*)
let rec seqs = function
  | [] -> assert false
  | [e] -> e
  | e :: es -> seq e (seqs es)

(** [is_equal e1 e2] is the boolean expression [e1 = e2]. *)
let is_equal e1 e2 =
  HobixAST.(Apply (Apply (Variable (Id "`="), e1), e2))

(** [conj e1 e2] is the boolean expression [e1 && e2]. *)
let conj e1 e2 =
  HobixAST.(Apply (Apply (Variable (Id "`&&"), e1), e2))

(** [conjs [e1; ..; eN]] is the boolean expression [e1 && .. && eN]. *)
let rec conjs = HobixAST.(function
  | [] -> Literal (LBool true)
  | [c] -> c
  | c :: cs -> conj c (conjs cs)
)

(** [component x i] returns [x[i]]. *)
let component x i = [x[i]]
(*		      failwith "Students! This is your job!"*)


let located  f x = f (Position.value x)
let located' f x = Position.map f x

(** [program env p] turns an Hopix program into an equivalent
    Hobix program. *)
let rec program env p =
  let env, defs = ExtStd.List.foldmap definition' env p in
  (List.flatten defs, env)

(** Compilation of Hopix toplevel definitions. *)
and definition' env p =
  definition env (Position.value p)

and definition env = HobixAST.(function
  | HopixAST.DeclareExtern (x, _) ->
    env, [DeclareExtern (located identifier x)]

  | HopixAST.DefineValue (x, e) ->
    env, [DefineValue (located identifier x, located (expression env) e)]

  | HopixAST.DefineRecValue recs ->
    env, [DefineRecValue (List.map (value_definition env) recs)]

  | HopixAST.DefineType (_, _, tydef) ->
    type_definition env tydef, []
)

and value_definition env (x, e) =
  (located identifier x, located (expression env) e)

and identifier (HopixAST.Id x) =
  HobixAST.Id x

(** Compilation of Hopix expressions. *)
and expression env = HobixAST.(function
  | HopixAST.Variable x ->
    Variable (located identifier x)

  | HopixAST.Tagged (k, es) ->
    failwith "Students! This is your job! Tagged"

  | HopixAST.Record rfs ->
    failwith "Students! This is your job! Record"

  | HopixAST.Apply (e1, e2) ->
    Apply (located (expression env) e1,
	   located (expression env) e2)

  | HopixAST.Literal l ->
    Literal (located literal l)

  | HopixAST.Define (x, e1, e2) ->
    Define (located identifier x,
	    located (expression env) e1,
	    located (expression env) e2)

  | HopixAST.DefineRec (recs, e) ->
    DefineRec (List.map (value_definition env) recs,
	       located (expression env) e)

  | HopixAST.TypeAnnotation (e, ty) ->
    located (expression env) e

  | HopixAST.IfThenElse (e1, e2, e3) ->
    IfThenElse (located (expression env) e1,
		located (expression env) e2,
		located (expression env) e3)

  | HopixAST.Fun (p, e) ->
    failwith "Students! This is your job! Fun"
)


(** [expands_or_patterns branches] returns a sequence of branches
    equivalent to [branches] except that their patterns do not contain
    any disjunction. {ListMonad} can be useful to implement this
    transformation. *)
and expands_or_patterns branches =
 failwith "Students! This is your job! expands_or_pattern"


(** [pattern env scrutinee p] returns a boolean condition [c]
    and a list of definitions [ds] such that:

    - [c = true] if and only if [p] matches the [scrutinee] ;
    - [ds] binds all the variables that appear in [p].

*)
and pattern env scrutinee p = HobixAST.(
    failwith "Students! This is your job! pattern"
)

and literal = HobixAST.(function
  | HopixAST.LInt x -> LInt x
  | HopixAST.LString s -> LString s
  | HopixAST.LChar c -> LChar c
  | HopixAST.LBool b -> LBool b
)

(** Compilation of type definitions. *)
and type_definition env =
  HopixAST.(function
	     | HopixAST.Abstract -> env
	     | HopixAST.DefineRecordType(l) -> add_rec_label env l
	     | _ -> failwith "Students! This is your job! type_def"
	   )

and add_rec_label env l =
  let rec aux_label index a_env = function 
    | [] -> env
    | (lab_l,_)::q ->
      let lab = Position.value lab_l in
      let nmap = add_label lab (Int32.of_int index) (a_env.label_position) in
      aux_label (index+1) {a_env with label_position = nmap} q
  in aux_label 0 env l


(** Here is the compiler! *)
let translate source env =
  program env source
