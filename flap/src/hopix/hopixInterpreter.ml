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
  let value_as_int      = function VInt x -> Some x | _ -> None
  let value_as_bool     = function VBool x -> Some x | _ -> None
  let value_as_char     = function VChar c -> Some c | _ -> None

  type ('a, 'e) wrapper = 'a -> 'e gvalue
  let int_as_value x  = VInt x

  let primitive name ?(error = fun () -> assert false) coercion wrapper f =
    VPrimitive (name, fun x ->
      match coercion x with
        | None -> error ()
        | Some x -> wrapper (f x)
    )

  (* Ces deux fonction sont-elles utiles ?*)
  let print_tagged_value = function
    | KId s -> s
    | _ -> failwith "Invalid tagged value"


  let print_pattern_value = function
    | PWildcard -> "_"
    | _ -> failwith "Invalid pattern value"
  (* FIN question *)

  let print_value m v =
    let max_depth = 5 in
    let rec print_value d v =
      if d >= max_depth then "..." else
        match v with
        | VInt x -> Int32.to_string x
  	| VBool x -> string_of_bool x
  	| VString s -> s
  	| VChar c -> Char.escaped c
	| VTaggedValues (t,e) -> (print_tagged_value t);
        | VPrimitive (s, _) ->  Printf.sprintf "<primitive: %s>" s

    and print_record_value d r =
      "{ " ^ String.concat "; " (List.map (print_field d) r) ^ " }"

    and print_field d (LId l, v) =
      l ^ " = " ^ print_value (d + 1) v
    in
    print_value 0 v


  (* Type mémoire *)

  (*type hopixTag = Sum | Rec;;

  type hopixMemory = 
    | EmptyMemory
    | SumData of hopixTag * (string * string) list
  ;;

  (* Build an empty memory *)
  let hopix_empty_memory = EmptyMemory;;

  (* Check if the memory is empty *)
  let hopix_is_empty = function
    | EmptyMemory -> true
    | _ -> false
  ;;


  let build_memory (old_mem : (string * hopixMemory) list) 
      (tag: hopixTag ) (c : string) (ss : (string * string) list) =
  match old_mem with
    | [] -> [(c,SumData((tag,ss)))]
    | _ -> (c,SumData((tag,ss)))::old_mem
  ;;*)



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

  type formals = identifier list

  type runtime = {
    memory      : value Memory.t;
    environment : Environment.t;
  }

  type observable = {
    new_memory      : value Memory.t;
    new_environment : Environment.t;
  }


  (** [primitives] is an environment that contains the implementation
      of all primitives (+, <, ...). *)
  let primitives =
    let intbin name out op =
      VPrimitive (name, function VInt x ->
        VPrimitive (name, function
          | VInt y -> out (op x y)
          | _ -> assert false (* By typing. *)
        )
        | _ -> assert false (* By typing. *)
      )
    in
    let bind_all what l x =
      List.fold_left (fun env (x, v) -> Environment.bind env (Id x) (what x v)) x l
    in
    (* Define arithmetic binary operators. *)
    let binarith name =
      intbin name (fun x -> VInt x) in
    let binarithops = Int32.(
      [ ("`+", add); ("`-", sub); ("`*", mul); ("`/", div) ]
    ) in
    Environment.empty
    |> bind_all binarith binarithops

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
    (* Fonction récursive *)
    | DefineRecValue (l) ->
       let rec new_env list env mem =
         (match list with
  	  | [] -> env
	  | (x,v)::q -> let nv,n_mem = (expression' env mem v) in
			let tmp_env = (bind_identifier env x nv) in
			new_env q tmp_env n_mem
         )
       in
       {
	 environment = new_env l (runtime.environment) (runtime.memory);
	 memory = runtime.memory
       }
    (* Pour le type somme *)
    | DefineType(t,[],td) ->
      (
        match (value t) with
  	| TCon(_) ->
	   (
	     match td with
	     | DefineSumType(l) -> failwith "TODO Type somme"
	     | DefineRecordType(l) ->
		  {
		    environment = runtime.environment;
		    memory = (
		      let map_value (a,b) = ((value a),(value b)) in
   		      Memory.allocate (runtime.memory) (List.map map_value l)
		    )
		  }
	     | _ -> failwith "Unrecognized definition"
	   )
  	| _ -> failwith "DefineType: Bad constructor"
      )
    | _ -> failwith "definition: Not dealt"

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
      Environment.lookup (Position.position x) (Position.value x) environment, memory

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

(*    | Record(lab,ex) ->*)
    | DefineRec (l,ex) -> failwith "TODO DefineRec."
    | Fun(cc,ec) -> failwith "TODO Fun."
    | Tagged(k,e) -> failwith "TODO Tagged."
    | Case(cc,ec) -> failwith "TODO Case."
    | TypeAnnotation(cc,ec) -> failwith "TODO Annotate."
    | Field(cc,ec) -> failwith "TODO Field."
    | ChangeField(cc,ch,ec) -> failwith "TODO Change."
    | _ -> failwith "TODO it."


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
