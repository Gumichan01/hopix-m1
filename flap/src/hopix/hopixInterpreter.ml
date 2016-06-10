  open Position
  open Error
  open HopixAST

  (** [error pos msg] reports runtime error messages. *)
  let error positions msg =
    errorN "execution" positions msg

  (** Every expression of datix evaluates into a [value]. *)
  type 'e gvalue =

    | VInt          of Int32.t
    | VChar         of char
    | VString       of string
    | VUnit
    | VAddress      of Memory.address
    | VTaggedValues of constructor * 'e gvalue list
    | VPrimitive    of string * ('e gvalue -> 'e gvalue)
    | VBool         of bool
    | VFun          of pattern located * expression located * 'e

  type 'e record =
    | VRecord of (label * 'e gvalue) list


  type ('a, 'e) coercion = 'e gvalue -> 'a option
  let value_as_int       = function VInt x -> Some x | _ -> None
  let value_as_bool      = function VBool x -> Some x | _ -> None
  let value_as_char      = function VChar c -> Some c | _ -> None
  let value_as_tagged    = function VTaggedValues(c,v) -> Some(c,v) | _ -> None
  let value_as_address   = function VAddress(addr)    -> Some(addr) | _ -> None

  type ('a, 'e) wrapper = 'a -> 'e gvalue
  let int_as_value x  = VInt x

  let primitive name ?(error = fun () -> assert false) coercion wrapper f =
    VPrimitive (name, fun x ->
      match coercion x with
        | None -> error ()
        | Some x -> wrapper (f x)
    )

  (* Ces deux fonctions sont-elles utiles ?*)
  let print_tagged_value = function
    | KId s -> s


  let print_pattern_value = function
    | PWildcard -> "_"
    | PVariable(i) -> let Id(v) = Position.value i in v
    | _ -> failwith "Invalid pattern value"
  (* FIN question *)

  let printv f m v = f m v;;

  let print_value m v =
    let max_depth = 5 in
    let rec print_value_aux d v =
      if d >= max_depth then "..." else
        match v with
        | VInt x -> Int32.to_string x
        | VChar c -> "'" ^ Char.escaped c ^ "'"
        | VString s -> "\"" ^ s ^ "\""
        | VUnit -> "()"
        | VAddress a -> print_record_value d (Memory.read_block m a)
	    | VTaggedValues (t,l) -> (print_tagged_value t);
        | VPrimitive (s, _) ->  Printf.sprintf "<primitive: %s>" s
        | VBool x -> string_of_bool x
        | VFun (pl,el,e') -> "<fun>"

    and print_record_value d r =
      "{ " ^ String.concat "; " (List.map (print_field d) r) ^ " }"

    and print_field d (LId l, v) =
      l ^ " = " ^ print_value_aux (d + 1) v

    in
    print_value_aux 0 v


 (* Environnement d'execution *)

  module Environment : sig
    type t
    val empty : t
    val bind    : t -> identifier -> t gvalue -> t
    val update  : Position.t -> identifier -> t -> t gvalue -> unit
    exception UnboundIdentifier of identifier * Position.t
    val lookup  : Position.t -> identifier -> t -> t gvalue
    val last    : t -> (identifier * t gvalue * t) option
    val print   : t gvalue Memory.t -> t -> string
  end = struct

    type t =
      | EEmpty
      | EBind of identifier * t gvalue ref * t

    let empty = EEmpty

    let bind e x v =
      EBind (x, ref v, e)

    exception UnboundIdentifier of identifier * Position.t

    let lookup' pos x =
      let rec aux = function
        | EEmpty -> raise (UnboundIdentifier (x, pos))
        | EBind (y, v, e) ->
          if x = y then v else aux e
      in
      aux

    let lookup pos x e = !(lookup' pos x e)

    let update pos x e v =
      lookup' pos x e := v

    let last = function
      | EBind (x, v, e) -> Some (x, !v, e)
      | EEmpty -> None

    let print_binding m (Id x, v) =
      x ^ " = " ^ print_value m !v

    let print m e =
      let b = Buffer.create 13 in
      let push x v = Buffer.add_string b (print_binding m (x, v)) in
      let rec aux = function
        | EEmpty -> Buffer.contents b
        | EBind (x, v, EEmpty) -> push x v; aux EEmpty
        | EBind (x, v, e) -> push x v; Buffer.add_string b "\n"; aux e
      in
      aux e

  end

  type value = Environment.t gvalue
  type grec  = Environment.t record (* type record *)

  type formals = identifier list

  type runtime = {
    memory      : value Memory.t;
    environment : Environment.t;
  }

  type observable = {
    new_memory      : value Memory.t;
    new_environment : Environment.t;
  }

  (* Define a record that contains:
     - a list of evaluated expressions in the record
     - and the memory M' = Mn + → { l₁ := v₁, l₂ := v₂ , ..., ln := vn } *)
  type efields = {

    eval_fields : (HopixAST.label * value) list;
    mem         : value Memory.t;

  }

  let empty_list : unit -> 'a list = fun () -> [];;
  let empty_efields () : efields =
    { eval_fields = empty_list () ; mem = Memory.fresh () };;


  (* HopixInt is an signature that defines a generic module that handles
    different kinds of integer values (16-bit, 32-bit or 64-bit integer) *)
  module type HopixInt =
  sig

    type t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val lt : t -> t -> bool
    val lte : t -> t -> bool
    val gt : t -> t -> bool
    val gte : t -> t -> bool
    val eq : t -> t -> bool

  end

  (* HopixInt32 is a submodule that implement operations on 32-bit integers
     with basic operations (+, -, *, /) + the comparison operations
     (<, ≤, >, ≥, =) that are not implemented in Int32 *)
  module HopixInt32 : HopixInt with type t = Int32.t =
  struct

    type t = Int32.t
    let add  x y = Int32.add x y
    let sub  x y = Int32.sub x y
    let mul  x y = Int32.mul x y
    let div  x y = Int32.div x y
    let lt   x y = x < y
    let lte  x y = x <= y
    let gt   x y = x > y
    let gte  x y = x >= y
    let eq   x y = x = y

  end

  (* HopixBool is a submodule that implements boolean operations *)
  module HopixBool :
  sig

  val bor : bool -> bool -> bool
  val band : bool -> bool -> bool

  end = struct

  let bor  x y = x || y
  let band x y = x && y

  end


  let arithop () = HopixInt32.([ ("`+", add); ("`-", sub);
                                 ("`*", mul); ("`/", div) ])

  let cmpop () = HopixInt32.([ ("`=", eq); ("`<", lt);
                               ("`<=", lte); ("`>", gt); ("`>=", gte) ])

  let boolop () = HopixBool.( [ ("`||", bor); ("`&&", band) ] )


  (** [primitives] is an environment that contains the implementation
      of all primitives (+, <, ...). *)
  let primitives =
    (* For arithmetic operations *)
    let intbin name out op =
      VPrimitive (name, function VInt x ->
        VPrimitive (name, function
          | VInt y -> out (op x y)
          | _ -> assert false (* By typing. *)
        )
        | _ -> assert false (* By typing. *)
      )
    in
    (* For boolean operations : &&, || *)
    let boolbin bname out op =
      VPrimitive (bname, function VBool x ->
        VPrimitive (bname, function
          | VBool y -> out (op x y)
          | _ -> assert false (* By typing. *)
        )
        | _ -> assert false (* By typing. *)
      )
    in
    (* For comparison operations : <, ≤, ≥, >, = *)
    let bcmpbin name out op =
      VPrimitive (name, function VInt x ->
        VPrimitive (name, function
          | VInt y -> out (op x y)
          | _ -> assert false (* By typing. *)
        )
        | _ -> assert false (* By typing. *)
      )
    in
    let bind_all what l x =
      List.fold_left (fun env (x, v)
                       -> Environment.bind env (Id x) (what x v)) x l
    in
    (* Define arithmetic binary operators. *)
    let binarith name =
      intbin name (fun x -> VInt x) in
    let binarithops = arithop ()
    in
    (* Define boolean binary operators. *)
    let binboolean name =
      boolbin name (fun x -> VBool x) in
      let boolops = boolop ()
    in
    (* Define boolean binary operators. *)
    let bcompare name =
      bcmpbin name (fun x -> VBool x) in
      let bincmpops = cmpop ()
    in
    (Environment.empty |> bind_all binarith binarithops
      |> bind_all binboolean boolops |> bind_all bcompare bincmpops)

  let initial_runtime () = {
    memory      = Memory.fresh ();
    environment = primitives;
  }

  let rec evaluate runtime ast =
    try
      let runtime' = List.fold_left definition runtime ast in
      (runtime', extract_observable runtime runtime')
    with Environment.UnboundIdentifier (Id x, pos) ->
      Error.error "interpretation" pos (Printf.sprintf "`%s' is unbound." x)

  (* [definition pos runtime d] evaluates the new definition [d]
     into a new runtime [runtime']. In the specification, this
     is the judgment:

  			E, M ⊢ dᵥ ⇒ E', M'

  *)
  and definition runtime d =
    match Position.value d with
    | DefineValue (x, e) ->
      let v, memory = expression' runtime.environment runtime.memory e in
      {
        environment = bind_identifier runtime.environment x v;
        memory
      }
    (* Recursive function *)
    | DefineRecValue (l) ->
       let rec new_env list env mem =
       (match list with
  	    | [] -> env
	    | (x,v)::q ->
          let nv,n_mem = (expression' env mem v) in
		  let tmp_env = (bind_identifier env x nv) in
          new_env q tmp_env n_mem
       )
       in
       {
	     environment = new_env l (runtime.environment) (runtime.memory);
	     memory = runtime.memory
       }
    (* Sum type *)
    | DefineType(_) | DeclareExtern(_) -> runtime

  and expression' environment memory e =
    expression (position e) environment memory (value e)

  (* [expression pos runtime e] evaluates into a value [v] if

                            E, M ⊢ e ⇓ v, M'

     and E = [runtime.environment], M = [runtime.memory].
  *)
  and expression position environment memory = function
    | Apply (a, b) ->
      let vb, memory = expression' environment memory b in
      begin match expression' environment memory a with
        | VPrimitive (_, f), memory ->
          f vb, memory
        | VFun ({ Position.value = PVariable x }, e, environment), memory ->
  	(*

  	   E ⊢ a ⇓ \x => e [ E' ]
  	   E ⊢ b ⇓ v_b
  	    E' + x ↦ v_b ⊢ e ⇓ v
  	   —————————————————————————————
  	   E ⊢ a b ⇓ v

  	*)
  	expression' (bind_identifier environment x vb) memory e

        | v, memory ->
          assert false (* By typing. *)
      end

    | Literal l ->
      literal (Position.value l), memory

    | Variable x ->
      Environment.lookup (Position.position x) (Position.value x) environment,
      memory

    | Define (x, ex, e) ->
      let v, memory = expression' environment memory ex in
      let environment = bind_identifier environment x v in
      expression' environment memory e

    | IfThenElse (c, t, f) ->
      let v, memory = expression' environment memory c in
      begin match value_as_bool v with
      | None -> assert false (* By typing. *)
      | Some true -> expression' environment memory t
      | Some false -> expression' environment memory f
      end

    | Record(l) ->
      let mapped_rec = eval_record_fields environment memory l in
      let (l,m) = (mapped_rec.eval_fields, mapped_rec.mem) in
      let (addr,mem) = Memory.allocate m l in
      (VAddress(addr),mem)

    | DefineRec (l,ex) -> (*  NOTE : Check if it is correct..*)
      let tenv = bind_fidentifiers environment l in
      let nenv, mem = expression_rec tenv memory l in
      expression' nenv mem ex

    | Fun(p,ex) -> func position environment memory p ex
    | Tagged(k,e) -> failwith "TODO Tagged."
    | Case(cc,ec) -> failwith "TODO Case."
    | TypeAnnotation(ex,_) -> expression' environment memory ex
    | Field(el,ll) -> field position environment memory (el,ll)
    | ChangeField(el,ll,vall) ->
      change_field position environment memory (el,ll, vall)

  (* Bind every function labels *)
  and bind_fidentifiers env = function
    | [] -> env
    | (x,_)::q -> bind_fidentifiers (bind_identifier env x (VUnit)) q

  (* Evaluate the expressions of the recursive functions *)
  and expression_rec env memory = function
    | [] -> env, memory
    | (x,e)::q ->
      let v, mem = expression' env memory e in
      expression_rec (bind_identifier env x v) mem q

  (* Evaluate every fields of the record *)
  and eval_record_fields environment memory l : efields =
    let rec ev_aux env m (res : efields) = function
    | [] -> { res with mem = res.mem }

    | (a,b)::q -> let a' = (value a) in
      let b', m' = (expression' environment memory b) in
      ev_aux environment memory
        ({ eval_fields = res.eval_fields@[(a',b')] ; mem = m' }) q

    in (ev_aux environment memory (empty_efields ()) l )

  (* Interpretation of the access to a field of a record *)
  and field position environment memory (ex,ll) =
    let l = Position.value ll in
    match (Position.value ex) with
    | Variable(id) ->
      let v, mem = expression' environment memory ex in
      begin
       match value_as_address v with
       | Some(addr) ->
         let r = Memory.read_block mem addr in
         (List.assoc l r), memory
       | None -> failwith "Field of record : Invalid address."
      end

    | Record(rl) ->
      let r = List.map (fun (x,y) -> (Position.value x, Position.value y)) rl in
      expression position environment memory (List.assoc l r)

    | Field(e',l') ->
      let v, m = field position environment memory (e',l') in
        begin
          match v with
          | VAddress a ->
            let r = Memory.read_block m a in
            (List.assoc l r), m
          | _ -> assert false (* by field *)
        end

    | _ -> failwith "Field of record : Not supported operation."

  (* Interpretation of the modification of a field of a record *)
  and change_field position environment memory (ex,ll, e') =
   let l = Position.value ll in
   begin
     match (Position.value ex) with
     | Variable(id) ->
       let v, mem = expression' environment memory ex in
       begin
        match value_as_address v with
        | Some(addr) ->
          let value', nmem = expression' environment memory e' in
          VUnit, (Memory.write nmem addr l value')

        | None -> failwith("Change field of record: Not supported operation.")
       end
     | _ -> failwith "Change field of record: Not supported operation."
   end

  (* Interpratation of the function *)
  and func position env memory ptrn expr =
    let ptrn' = Position.value ptrn in
    match ptrn' with (*  TODO Some pattern matching are not done *)
    | PTypeAnnotation(pat,_) -> func position env memory pat expr
    | PVariable(i)           -> VFun(ptrn,expr,env), memory
    | PTaggedValue(cs, patl) -> failwith "@todo func: PTaggedValue"
    | PWildcard              -> expression' env memory expr
    | PLiteral(li)           -> expression' env memory expr
    | PRecord(rl)            -> failwith "@todo func: PRecord"
    | POr(pat)               -> func_ptrn position env memory pat expr
    | PAnd(pat)              -> func_ptrn position env memory pat expr


  (* Function with patterns *)
  and func_ptrn position env memory pat expr =
    let get_memory_from (_,m) = m in
    let get_vvalue (v,_) = v in
    let rec fpat_aux pos env pl vfm =
    match pl with
      | [] -> vfm
      | hpat::q ->
        let vfunc, mem =
          (match get_vvalue vfm with
           | VUnit           -> func pos env (get_memory_from vfm) hpat expr
           | VFun(_,ex,nenv) -> func pos nenv (get_memory_from vfm) hpat ex
           | _ -> assert false (* func_patrn for POr/PAnd with VUnit or Vfun *)
        )
        in
        fpat_aux position env q (vfunc,mem)
    in fpat_aux position env pat (VUnit,memory)


  and bind_identifier environment x v =
    Environment.bind environment (Position.value x) v


  and literal = function
    | LInt x -> VInt x
    | LBool x -> VBool x
    | LString x -> VString x
    | LChar x -> VChar x

  and extract_observable runtime runtime' =
    let rec substract new_environment env env' =
      if env == env' then new_environment
      else
        match Environment.last env' with
          | None -> assert false (* Absurd. *)
          | Some (x, v, env') ->
            let new_environment = Environment.bind new_environment x v in
            substract new_environment env env'
    in
    {
      new_environment =
        substract Environment.empty runtime.environment runtime'.environment;
      new_memory =
        runtime'.memory
    }

  let print_observable runtime observation =
    Environment.print observation.new_memory observation.new_environment
