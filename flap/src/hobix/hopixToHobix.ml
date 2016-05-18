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

let add_constructor key value map =
  ConstructorMap.add key value map

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

let located  f x = f (Position.value x)
let located' f x = Position.map f x

let to_int32 x = Int32.of_int x

let hbx_int32 y = HobixAST.Literal(HobixAST.LInt(to_int32 y))

(*
    [ map_extract_expr [(a1,b1); (a2,b2); ... ; (an,bn)] ]
    Extract the second part of each pair of the list given in argument .
    So, the returned list is [b1; b2; ...; bn] *)
let map_extract_expr l =
  let rec aux_map l' lres =
  match l' with
  | [] -> lres
  | (_,b)::q -> aux_map q (b::lres)
  in aux_map l []

(*
  [ map_located_list [e1); e2; ...; en] ]
  Transforms a list of located expression to a list of expressions
  So, the returned list is
  [Position.value(e1); Position.value(e2); ...; Position.value(en)]*)
let map_located_list l =
  let rec aux_loc l' lres =
  match l' with
  | [] -> lres
  | e::q -> aux_loc q ((Position.value e)::lres)
  in aux_loc l []

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

  | HopixAST.Tagged (k, es) ->                      (* TODO it *)
    failwith "TODO : Hopix -> Hobix Tagged"

  | HopixAST.Record (l) -> record_compile env l     (* TODO it *)

  | HopixAST.Field (e,l) -> record_field env e l    (* TODO it *)

  | HopixAST.ChangeField (e1,l,e2) ->               (* TODO it *)
    failwith "TODO : Hopix -> Hobix ChangeField"

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
    compile_fun env p e

  | HopixAST.Case(exp,branches) -> compile_case env exp branches
    (*failwith "TODO : Hopix -> Hobix Case"*)
)

(* Compile HopixAST.Fun -> HobixAST.Fun *)
and compile_fun env p e =
  match get_pattern env (Position.value p) with
  | HopixAST.PVariable(idlocated) ->
    let HopixAST.Id(i) = Position.value idlocated in
    let id = HobixAST.Id(i) in
    HobixAST.Fun(id,(expression env (Position.value e)))

  | HopixAST.PWildcard ->
    let id = fresh_identifier () in
    HobixAST.Fun(id,(expression env (Position.value e)))

  | _ -> failwith "error : invalid pattern"

(*
    [ get_pattern env p ]
    returned a pattern that is not an annotated pattern *)
and get_pattern env = HopixAST.(function
  | PTypeAnnotation(pta,_) -> get_pattern env (Position.value pta)
  | _ as p -> p
  )

(* Compile HopixAST.Case -> HobixAST.Switch *)
and compile_case (env : environment) exp branches =
  let e = Position.value exp in
  let brlist = ( branches |> branch_pair_list env ) in
  let (plist,earray) = ( (brlist |> List.map fst),
                            (brlist |> map_extract_expr |> Array.of_list) ) in
  (
   match generate_switch e (plist,earray) with
   | None -> failwith "syntax error : case"
   | Some(instr) -> instr
  )

  (*failwith "TODO compile_case : generate_switch"*)

(*
    [ branch_pair_list env [HopixAST.Branch(p₁,e₁),...,HopixAST.Branch(pn,en)] ]
    retrieves all pairs of not located values from the list of HopixAST branches
    and returns a list of pair → [(p₁,e₁),(p₂,e₂),...,(pn,en)]
     - p₁,...,pn are patterns that are not annotated
     - e₁,...,en are compiled expressions (HobixAST)

    Algorithm :
     ① Extract an element of the list and get its value (not located) :
          → HopixAST.Branch(p,e)    { p and e are located }

     ② Generate the pair (p',e')
        with p': the non-annotated pattern
        and e' : the compiled expression of e

     ③ Add the new pair in the result list
          → [(p'₁,e'₁),(p'₂,e'₂),...,(pn,en)]

*)
and branch_pair_list (env : environment) l =
  let rec aux_pair_list (env : environment) l' lres =
  match l' with
  | [] -> lres
  | h::q ->
    let HopixAST.Branch(p,e) = Position.value h in
    let new_pair = (get_pattern env (Position.value p),
                    expression env (Position.value e)) in
    aux_pair_list env q ( new_pair::lres )
  in aux_pair_list env l []

(*
    [ generate_switch e ([p₁,p₂,...,pn],[|e1,e₂,...en|]) ]
    generates the HobixAST.Switch instruction.

*)
and generate_switch e (plist,earray) =
  let rec aux_switch (i : int) e (plist,earray) =
  match plist with
  | [] -> None
  | [p] -> pattern_to_switch i e earray p
  | ptrn::q -> failwith "TODO test if exppresion \"=\" pattern"

  and pattern_to_switch i e earray = HopixAST.(function
    | PWildcard -> Some(HobixAST.Switch(hbx_int32 i,earray,None))
    | PLiteral(ll) -> failwith "TODO : literal pattern"
    | _ -> None
    )
  in aux_switch 0 e (plist,earray)

(*
    [record_compile env l] generate the
    hobix instruction associated with Hopix.Record *)
and record_compile env l =
  match List.length l with
  | 0 -> failwith "error : empty record."
  | _ as sz ->
    let b = [HobixAST.AllocateBlock(hbx_int32 sz)] in
    let hopix_instr_l = (l |> map_extract_expr |> map_located_list) in
    let lseq = b@(generate_instructions env hopix_instr_l)@b in
    seq_record lseq (*Not correct*)

(*
    [generate_instructions env l] generate the
    hobix instruction list from l ven in argument *)
and generate_instructions env l =
  let rec aux_gen env l' lres =
  match l' with
  | [e]  -> (expression env e)::lres
  | h::q -> (expression env h)::(aux_gen env q lres)
  | _ -> assert false

  in aux_gen env l []

(* Generate a sequence of hobix instructions to build a record : TODO*)
and seq_record lseq = seqs (lseq)


(* TODO *)
and record_field env el ll =
  let aux_record_field env e l =
    match e with
    | HopixAST.Literal(_) ->
      failwith "error: access to a field of a literal value that is not a record."
    | HopixAST.Variable(vl) ->
      let i = index_of_label env l in
      let index = HobixAST.Literal(HobixAST.LInt(i)) in
      (HobixAST.ReadBlock(HobixAST.Literal(HobixAST.LString("error")),index)) (*Not correct*)
    | _ -> failwith "TODO : record_field"

  in aux_record_field env (Position.value el) (Position.value ll)


(** [expands_or_patterns branches] returns a sequence of branches
    equivalent to [branches] except that their patterns do not contain
    any disjunction. {ListMonad} can be useful to implement this
    transformation. *)
and expands_or_patterns branches =
 failwith "TODO expands_or_pattern"


(** [pattern env scrutinee p] returns a boolean condition [c]
    and a list of definitions [ds] such that:

    - [c = true] if and only if [p] matches the [scrutinee] ;
    - [ds] binds all the variables that appear in [p].

*)
and pattern env scrutinee p = HobixAST.(
    failwith "TODO pattern -> Switch(c,expr1,expr2)"
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
         | HopixAST.DefineSumType(l) -> add_rec_cons env l
	   )

and add_rec_label env l =
  let rec aux_label index a_env = function
    | [] -> a_env
    | (lab_l,_)::q ->
      let lab = Position.value lab_l in
      let nmap = add_label lab (Int32.of_int index) (a_env.label_position) in
      aux_label (index+1) {a_env with label_position = nmap} q
  in aux_label 0 env l

and add_rec_cons env l =
  let rec aux_cons index a_env = function
  | [] -> a_env
  | (cons_l,_)::q ->
    let cons = Position.value cons_l in
    let nmap = add_constructor cons (Int32.of_int index) (a_env.constructor_tags)
    in aux_cons (index+1) {a_env with constructor_tags = nmap} q
    in aux_cons 0 env l

(** Here is the compiler! *)
let translate source env =
  program env source
