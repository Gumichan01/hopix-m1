(** This module implements a compiler from Fopix to Retrolix. *)

let error pos msg =
  Error.error "compilation" pos msg

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Fopix
module Target = Retrolix
module S = Source.AST
module T = Target.AST

(** We will need the following pieces of information to be carrying
    along the translation: *)
module IdCmp = struct
  type t = T.identifier
  let compare = compare
end
module IdSet = Set.Make (IdCmp)

(** The compilation environment stores the list of global
    variables (to compute local variables) and a table
    representing a renaming (for alpha-conversion). *)
type environment = IdSet.t * (S.identifier * S.identifier) list

(** Initially, the environment is empty. *)
let initial_environment () = (IdSet.empty, [])

(** [fresh_label ()] returns a new identifier for a label. *)
let fresh_label =
  let c = ref 0 in
  fun () -> incr c; T.Label ("l" ^ string_of_int !c)

(** [fresh_label ()] returns a new identifier for a variable. *)
let fresh_variable =
  let c = ref 0 in
  fun () -> incr c; T.(Id ("X" ^ string_of_int !c))

(**
   Every function in Retrolix starts with a declaration
   of local variables. So we need a way to compute the
   local variables of some generated code. This is the
   purpose of the next function:
*)

(** Get the updated list of variables adding if
    the rvalue given in argument is a variable *)
let destruct_rvalue (v : T.rvalue) (l : T.identifier list) : T.identifier list =
  match v with
  | `Variable(vid) -> vid::l
  | _ -> l

(** Get the updated list of variables adding if
    the lvalue given in argument is a variable *)
let destruct_lvalue (v : T.lvalue) (l : T.identifier list) : T.identifier list =
  match v with
  | `Variable(vid) -> vid::l
  | _ -> l


(** Build a list of rvalues that are variables *)
let destruct_rvalues (vl : T.rvalue list) : T.identifier list =
  let rec aux_destruct_rvalues rvl l =
  match rvl with
  | [] -> l
  | h::q -> aux_destruct_rvalues q (destruct_rvalue h l)
  in aux_destruct_rvalues vl []

(** Get the list of variables used in the instruction given in argument*)
let get_var_from_instr (i : T.labelled_instruction) : T.identifier list =
  let label, instr = i in
    match instr with
    | T.Call(lval,rval,rvl) ->
      destruct_lvalue lval (destruct_rvalue rval (destruct_rvalues rvl))

    | T.TailCall(rval,rvl) ->
      destruct_rvalue rval (destruct_rvalues rvl)

    | T.Ret(rv) -> destruct_rvalues [rv]

    | T.Assign(lval,_,rvl) -> destruct_lvalue lval (destruct_rvalues rvl)

    | T.ConditionalJump(_,rl,_,_) -> destruct_rvalues rl

    | T.Switch(rval,_,_) -> destruct_rvalues [rval]

    | T.Jump(_) | T.Comment(_) | T.Exit -> []



(** Get the list of variables in the list of instructions *)
let get_variables b =
  let rec aux_getv b l =
    match b with
    | [] -> l
    | i::q -> aux_getv q ((get_var_from_instr i) @ l)
  in aux_getv b []

(*
    [ construct local globals vlocals ]
    bulds the list of variables from vlocals that are not in glocals
*)
let construct_local (globals : IdSet.t) (vlocals : T.identifier list)
: T.identifier list =
  let rec aux_constr (gl : IdSet.t) vl l =
    match vl with
    | []   -> l
    | h::q ->
      (match opt_find h gl with
       | true  -> aux_constr gl q l
       | false -> aux_constr gl q (h::l))

  and opt_find : T.identifier -> IdSet.t -> bool =
    fun h gl -> IdSet.mem h gl

  in aux_constr globals vlocals []

(** [locals globals b] takes a set of variables [globals] and returns
    the variables use in the list of instructions [b] which are not
    in [globals]. *)
let locals globals b =
  let vlocals = get_variables b in
  construct_local globals vlocals


(** [translate' p env] turns a Fopix program [p] into a Retrolix
    program using [env] to retrieve contextual information. *)
let rec translate' p env =
  (** The global variables are extracted in a first pass. *)
  let (globals, renaming) = env in
  let globals = List.fold_left get_globals globals p in
  let env = (globals, renaming) in
  (** Then, we translate Fopix declarations into Retrolix declarations. *)
  let defs = List.map (declaration globals) p in
  (defs, env)

and identifier (S.Id x) = T.Id x

and get_globals env = function
  | S.DefineValue (x, _) ->
    push env x
  | _ ->
    env

and push env x =
  IdSet.add (identifier x) env

and declaration env = T.(function
  | S.DefineValue (S.Id x, e) ->
    let x = Id x in
    let ec = expression (`Variable x) e in
    let locals = locals env ec in
    DValue (x, (locals, ec))

  | S.DefineFunction (S.FunId f, xs, e) ->
    let x = fresh_variable () in
    let ec = expression (`Variable x) e in
    DFunction (FId f,
               List.map identifier xs,
               (locals env ec,
                ec @ [labelled (Ret (`Variable x))]))

  | S.ExternalFunction (S.FunId f) ->
    DExternalFunction (FId f)
)
(** [expression out e] compiles [e] into a block of Retrolix
    instructions that stores the evaluation of [e] into [out]. *)
and expression out = T.(function
  | S.Literal l ->
    [labelled (Assign (out, Load, [ `Immediate (literal l) ]))]

  | S.Variable (S.Id x) ->
    [labelled (Assign (out, Load, [ `Variable (Id x) ]))]

  | S.Define (S.Id x, e1, e2) ->
    (** Hey student! The following code is wrong in general,
	hopefully, you will implement [preprocess] in such a way that
	it will work, right? *)
    expression (`Variable (Id x)) e1 @ expression out e2

  | S.IfThenElse (c, t, f) ->
    failwith "TODO fopix -> Retrolix IfThenElse"

  | S.FunCall (S.FunId "allocate_block", es) ->
    assign out BlockCreate es

  | S.FunCall (S.FunId "read_block", es) ->
    assign out BlockGet es

  | S.FunCall (S.FunId "write_block", es) ->
    assign out BlockSet es

  | S.FunCall (S.FunId f, es) when is_binop f ->
    assign out (binop f) es

  | S.FunCall (S.FunId f, actuals) ->
    failwith "TODO fopix -> Retrolix FunCall"

  | S.UnknownFunCall (ef, actuals) ->
    failwith "TODO fopix -> Retrolix UnknownFunCall"

  | S.Switch (e, cases, default) ->
    failwith "TODO fopix -> Retrolix Switch"
)


and as_rvalue e =
  let x = `Variable (fresh_variable ()) in
  (x, expression x e)

and as_rvalues rs f =
  let xs, es = List.(split (map as_rvalue rs)) in
  List.flatten es @ f xs

and assign out op rs =
  as_rvalues rs (fun xs ->
    [labelled (T.Assign (out, op, xs))]
  )

and condition lt lf c = T.(
  let x = fresh_variable () in
  expression (`Variable x) c
  @ [ labelled (ConditionalJump (EQ, [ `Variable x;
                                       `Immediate (LInt (Int32.of_int 0)) ],
                                 lf,
                                 lt))]
)

and first_label = function
  | [] -> assert false
  | (l, _) :: _ -> l

and labelled i =
  (fresh_label (), i)

and literal = T.(function
  | S.LInt x ->
    LInt x
  | S.LFun (S.FunId f) ->
    LFun (FId f)
  | S.LChar c ->
    LChar c
  | S.LString s ->
    LString s
  | S.LBool b ->
    LBool b
)

and is_binop = function
  | "`+" | "`-" | "`*" | "`/" -> true
  | c -> is_condition c

and is_condition = function
  | "`<" | "`>" | "`=" | "`<=" | "`>=" -> true
  | _ -> false

and binop = T.(function
  | "`+" -> Add
  | "`-" -> Sub
  | "`*" -> Mul
  | "`/" -> Div
  | c -> Bool (condition_op c)
)

and condition_op = T.(function
  | "`<" -> LT
  | "`>" -> GT
  | "`<=" -> LTE
  | "`>=" -> GTE
  | "`=" -> EQ
  | _ -> assert false
)

let preprocess p env =
  (p, env)


(** [translate p env] turns the fopix program [p] into a semantically
    equivalent retrolix program. *)
let translate p env =
  let p, env = preprocess p env in
  let p, env = translate' p env in
  (*let p = RetrolixRegisterAllocation.translate p in*)
  (p, env)
