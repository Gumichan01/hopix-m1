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
  let value_as_string    = function VString s -> Some s | _ -> None
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


  let print_pattern_value = function
    | PWildcard -> "_"
    | PVariable(i) -> let Id(v) = Position.value i in v
    | _ -> failwith "Invalid pattern value"


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
	    | VTaggedValues (t,l) -> (print_tagged_value d t l)
        | VPrimitive (s, _) ->  Printf.sprintf "<primitive: %s>" s
        | VBool x -> string_of_bool x
        | VFun (pl,el,e') -> "<fun>"

    and print_record_value d r =
      "{ " ^ String.concat "; " (List.map (print_field d) r) ^ " }"

    and print_field d (LId l, v) =
      l ^ " = " ^ print_value_aux (d + 1) v

    and print_tagged_value d t l =
      let s = print_tagged t in
      match l with
      | [] -> s
      | _  ->
        s ^ " (" ^ String.concat ", " (List.map (print_value_aux (d+1)) l) ^ ")"

    and print_tagged (KId(s)) : string = s

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


  (** HopixInt is a signature that defines a generic module that handles
    different kinds of integer values (16-bit, 32-bit or 64-bit integers) *)
  module type HopixInt =
  sig

    type t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val lt  : t -> t -> bool
    val lte : t -> t -> bool
    val gt  : t -> t -> bool
    val gte : t -> t -> bool
    val eq  : t -> t -> bool

  end

  (* HopixInt32 is a submodule that implement operations on 32-bit integers
     with basic operations (+, -, *, /) and the comparison operations
     (<, ≤, =, ≥, >) that are not implemented in Int32 *)
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

  val bor  : bool -> bool -> bool
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
    (* Arithmetic operations *)
    let intbin name out op =
      VPrimitive (name, function VInt x ->
        VPrimitive (name, function
          | VInt y -> out (op x y)
          | _ -> assert false (* By typing. *)
        )
        | _ -> assert false (* By typing. *)
      )
    in
    (* Boolean operations : &&, || *)
    let boolbin bname out op =
      VPrimitive (bname, function VBool x ->
        VPrimitive (bname, function
          | VBool y -> out (op x y)
          | _ -> assert false (* By typing. *)
        )
        | _ -> assert false (* By typing. *)
      )
    in
    (* Comparison operations : <, ≤, =, ≥, > *)
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
    (* Define comparison operators. *)
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
       let nenv = bind_fidentifiers l runtime.environment in
       let rec new_env ls env mem =
       begin
         match ls with
  	      | [] -> env, mem
	      | (x,v)::q ->
            let nv,n_mem = (expression' nenv mem v) in
            let x',pos = Position.destruct x in
		    Environment.update pos x' nenv nv; new_env q nenv n_mem
       end
       in
       let envt,m = new_env l (runtime.environment) (runtime.memory) in
       {
	     environment = envt;
	     memory = m
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

    | DefineRec (l,ex) -> expression' environment memory ex

    | IfThenElse (c, t, f) ->
      let v, memory = expression' environment memory c in
      begin
        match value_as_bool v with
          | None -> assert false (* By typing. *)
          | Some true -> expression' environment memory t
          | Some false -> expression' environment memory f
      end

    | Record(l) ->
      let mapped_rec = eval_record_fields environment memory l in
      let (l,m) = (mapped_rec.eval_fields, mapped_rec.mem) in
      let (addr,mem) = Memory.allocate m l in
      (VAddress(addr),mem)

    | Field(el,ll) -> field environment memory (el,ll)
    | ChangeField(el,ll,vall) -> change_field environment memory (el,ll, vall)

    | Fun(p,ex)            -> func environment memory p ex
    | Tagged(k,e)          -> tagged environment memory (Position.value k) e
    | Case(e,br)           -> case_branches position environment memory e br
    | TypeAnnotation(ex,_) -> expression' environment memory ex


  (* Bind every function labels *)
  and bind_fidentifiers l env =
    match l with
    | [] -> env
    | (x,_)::q -> bind_fidentifiers q (bind_identifier env x (VUnit))


  (* Evaluate every fields of the record *)
  and eval_record_fields environment memory l : efields =
    let rec ev_aux env m (res : efields) = function
    | [] -> { res with mem = res.mem }

    | (a,b)::q -> let a' = (value a) in
      let b', m' = (expression' environment m b) in
      ev_aux environment m'
        ({ eval_fields = res.eval_fields@[(a',b')] ; mem = m' }) q

    in (ev_aux environment memory (empty_efields ()) l )


  (* Interpretation of the access to a field of a record *)
  and field environment memory (ex,ll) =
    let l = Position.value ll in
    match (Position.value ex) with
    | Variable(id) ->
      let v, mem = expression' environment memory ex in
      begin
       match value_as_address v with
       | Some(addr) ->
         let r = Memory.read_block mem addr in
         (List.assoc l r), memory
       | None -> failwith "Field of record : Invalid record."
      end

    | Record(rl) ->
      let r = List.map (fun (x,y) -> (Position.value x, Position.value y)) rl in
      expression' environment memory (Position.unknown_pos (List.assoc l r))

    | Field(e',l') ->
      let v, m = field environment memory (e',l') in
        begin
          match v with
          | VAddress a ->
            let r = Memory.read_block m a in
            (List.assoc l r), m
          | _ -> assert false (* by field *)
        end

    | _ -> failwith "Field of record : Not supported operation."


  (* Interpretation of the modification of a field of a record *)
  and change_field environment memory (ex,ll, e') =
   let l = Position.value ll in
   begin
     match (Position.value ex) with
     | Variable(id) ->
       let v, mem = expression' environment memory ex in
       begin
        match value_as_address v with
          | Some(addr) ->
            let value', nmem = expression' environment mem e' in
            VUnit, (Memory.write nmem addr l value')

          | None -> failwith("Change field of record: Not a record.")
       end

     | Record(_) ->
       failwith ("Cannot change the field of a record created \" on the fly \" .")

     | _ -> failwith "Change field of record: Invalid operation."
   end


  (* Interpretation of the function *)
  and func env memory ptrn expr =
    let ptrn' = Position.value ptrn in
    match ptrn' with
    | PTypeAnnotation(pat,_) -> func env memory pat expr
    | POr(pat)               -> func_ptrn env memory pat expr
    | PAnd(pat)              -> func_ptrn env memory pat expr
    | _                      -> VFun(ptrn,expr,env), memory


  (*  Interpretation of functions with the POr or the PAnd patterns *)
  and func_ptrn env memory pat expr =
    let get_memory_from (_,m) = m in
    let get_vvalue (v,_) = v in
    let rec fpat_aux env pl vfm =
    match pl with
      | [] -> vfm
      | hpat::q ->
        let vfunc, mem =
        begin
          match get_vvalue vfm with
            | VUnit           -> func env (get_memory_from vfm) hpat expr
            | VFun(_,ex,nenv) -> func nenv (get_memory_from vfm) hpat ex
            | _ -> assert false (* func_patrn for POr/PAnd with VUnit or Vfun *)
        end
        in
        fpat_aux env q (vfunc,mem)
    in fpat_aux env pat (VUnit,memory)


  (* Interpretation of the tagged value *)
  and tagged env memory cons el =
    match el with
      | [] -> VTaggedValues(cons,[]), memory
      | _  ->
        begin
          let rec tagged_aux mem l lres =
            match l with
              | []   -> lres, mem
              | h::q ->
                let v,m = expression' env mem h in
                tagged_aux m q (lres@[v])

          in
          let lp,nmem = tagged_aux memory el [] in
          VTaggedValues(cons,lp), nmem
        end


  (* Interpretation of branches (pattern matching) *)
  and case_branches pos env memory e brl =
    let v,m = expression' env memory e in
    let rec case_branches_aux env mem bl =
    begin
      match bl with
        | [] -> failwith "HopixInterpreter: pattern not found"
        | b::q ->
          let HopixAST.Branch(pl,e') = Position.value b in
          let p = Position.value pl in
          begin
             match (patrn_aux p) with
               | PWildcard        -> expression' env mem e'

               | PVariable(id)    -> expression' (bind_identifier env id v) m e'

               | PLiteral(lv)     -> case_pliteral env v m lv e' q

               | PTaggedValue(kl,pl') -> case_ptagged env v m kl pl' e' q

               | PRecord(l)       -> case_precord env v m l e'

               (* NOTE some patterns are not checked (PRecord, POr,PAnd) *)
               | _ -> case_branches_aux env mem q
          end
    end

    and patrn_aux p =
      match p with
        | PTypeAnnotation(p',_) -> patrn_aux (Position.value p')
        | _ as p' -> p'

    (** [case_pliteral env v m lval e'] deals with the PLiteral case
        in the pattern mathing

        env  : the environment
        v    : the value of the evaluated expression to match
        m    : the new memory after the evaluation of e → v
        lval : the value of the pattern
        e'   : the expression associated with lval
        nextl: the following list to check if the match failed *)
    and case_pliteral env v m lval e' nextl =
      match v,(Position.value lval) with
      | VInt(x), LInt(y) when HopixInt32.eq x y ->
        expression' env m e'

      | VBool(i), LBool(j) when i = j ->
        expression' env m e'

      | VChar(vc), LChar(c) when vc = c ->
        expression' env m e'

      | VString(vs), LString(s) when vs = s ->
        expression' env m e'

      | _ -> case_branches_aux env m nextl

    (** [case_precord env v m l e'] deals with the PRecord case
        in the pattern mathing

        env: the environment
        m  : the new memory after the evaluation of e → v
        l  : the content of PRecord
        e' : the expression associated with lval
    *)
    and case_precord env v m l e' =
      match value_as_address v with
      | Some(addr) ->
        let r = Memory.read_block m addr in
        expression' (bind_record env r (map_record l)) m e'

      | None -> failwith "HopixInterpreter: No record matched."

    (* Bind every fields of the record with a value *)
    and bind_record env r = function
      | []         -> env
      | (lab,p)::q ->
        let id,osp = ((label_to_identifier lab),(filter_pattern p)) in
        let nv     =    bind_identifier env id (List.assoc lab r)   in
        match osp with
         | Some sp ->
           let nenv  = bind_identifier nv sp (List.assoc lab r) in
           bind_record nenv r q
         | None    -> bind_record nv r q




    (* Filter the inner pattern of the record *)
    and filter_pattern = function
      | PVariable(v)         -> Some v
      | PWildcard            -> None
      | PTaggedValue(cs, []) ->
        let KId(s) = Position.value cs in
        (function
           | true -> None
           | false -> failwith("pattern mathcing failed")) (s = "_")

      | PTypeAnnotation(p,_) -> filter_pattern (Position.value p)
      | _ -> assert false (* by bind_record *)

    (** [case_ptagged env v m kl pl' e' nextl] deals with
        the PTaggedValue case in the pattern mathing.

        kl : the constructor from PTagged
        pl': the pattern associated with kl from PTagged
    *)
    and case_ptagged env v m kl pl' e' nextl =

      let eq_value x = function
        | LInt i    ->
          (function Some j -> HopixInt32.eq j i | None -> false) (value_as_int x)

        | LChar c   ->
          (function Some c' -> c' = c | None -> false) (value_as_char x)

        | LBool b   ->
          (function Some b' -> b' = b | None -> false) (value_as_bool x)

        | LString s ->
          (function Some s' -> s' = s | None -> false) (value_as_string x)

      in (* let eq_value ↑ *)
      (** [ptagged_params_eq l1 l2] compare the equality of l1 and l2
          from VTaggedValues and PTaggedValue respectively

          That is to say:
          with Γ = VTaggedValues(A,[l₁,...,ln]) and Δ = B([lB₁,...lBn])
          A and B are constructor identifers and A = B (by hypothesis)

          ([l₁,...,ln] = [lB₁,...lBn])
          ≡ ∀i,j ∈ ([l₁,...,ln],[lB₁,...lBn]), i = j \/ i = _ \/ j = _ *)
      let rec ptagged_params_eq l1 l2 =
        match l1,l2 with
          | [],[] -> true
          | [],_ | _,[] -> false
          | h1::q1,h2::q2 ->
            let hv1 = (Position.value h1 |> patrn_aux) in
            (equals hv1 h2) && ptagged_params_eq q1 q2

      and equals y x =
        match y with
          | PLiteral(y') -> eq_value x (Position.value y')
          | PWildcard    -> true
          | PTaggedValue(cs, []) -> let KId(s) = Position.value cs in s = "_"
          | _            -> false

      in (* let ptagged_params_eq ↑ *)
      let KId(kid) = Position.value(kl) in
        match kid,pl',v with
          (* Wildcard → default case *)
          | "_",[],_ -> expression' env m e'

          (* A constructor with no argument, example : A *)
          | _,[],VTaggedValues(KId(k),[]) when k = kid -> expression' env m e'

          (* A constructor with arguments, example : B(1024,'g') *)
          | _,l,VTaggedValues(KId(kv),l')
            when (List.length l) = (List.length l') && kid = kv ->
            if ptagged_params_eq l l'
            then expression' env m e'
            else case_branches_aux env m nextl

          | _,_,_  -> case_branches_aux env m nextl

    in case_branches_aux env memory brl

  (* Map the PRecord pattern to get the unlocated list of pairs *)
  and map_record l =
    List.map (fun (x,y) -> (Position.value x, Position.value y)) l

  and label_to_identifier (LId(s)) =
    Position.unknown_pos (Id(s))

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
