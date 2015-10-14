open Position
open Error
open HopixAST

(** [error pos msg] reports runtime error messages. *)
let error positions msg =
  errorN "execution" positions msg

(** Every expression of datix evaluates into a [value]. *)
type 'e gvalue =
  | VInt       of int
  | VBool      of bool
  | VPrimitive of string * ('e gvalue -> 'e gvalue)

type ('a, 'e) coercion = 'e gvalue -> 'a option
let value_as_int      = function VInt x -> Some x | _ -> None

type ('a, 'e) wrapper = 'a -> 'e gvalue
let int_as_value x  = VInt x

let primitive name ?(error = fun () -> assert false) coercion wrapper f =
  VPrimitive (name, fun x ->
    match coercion x with
      | None -> error ()
      | Some x -> wrapper (f x)
  )

let print_value v =
  let max_depth = 20 in

  let rec print_value d v =
    if d >= max_depth then "..." else
      match v with
        | VInt x ->
           string_of_int x
	| VBool x -> string_of_bool x
        | VPrimitive (s, _) ->
          Printf.sprintf "<primitive: %s>" s
  in
  print_value 0 v

module Environment : sig
  type t
  val empty : t
  val bind    : t -> identifier -> t gvalue -> t
  val update  : identifier -> t -> t gvalue -> unit
  exception UnboundIdentifier of identifier
  val lookup  : identifier -> t -> t gvalue
  val last    : t -> (identifier * t gvalue * t) option
  val print   : t -> string
end = struct

  type t =
    | EEmpty
    | EBind of identifier * t gvalue ref * t

  let empty = EEmpty

  let bind e x v =
    EBind (x, ref v, e)

  exception UnboundIdentifier of identifier

  let lookup' x =
    let rec aux = function
      | EEmpty -> raise (UnboundIdentifier x)
      | EBind (y, v, e) ->
        if x = y then v else aux e
    in
    aux

  let lookup x e = !(lookup' x e)

  let update x e v =
    lookup' x e := v

  let last = function
    | EBind (x, v, e) -> Some (x, !v, e)
    | EEmpty -> None

  let print_binding (Id x, v) =
    x ^ " = " ^ print_value !v

  let print e =
    let b = Buffer.create 13 in
    let rec aux = function
      | EEmpty -> Buffer.contents b
      | EBind (x, v, e) -> Buffer.add_string b (print_binding (x, v) ^ "\n"); aux e
    in
    aux e

end

type value = Environment.t gvalue

type formals = identifier list

type runtime = {
  environment : Environment.t;
}

type observable = {
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
  let binarithops =
    [ ("`+", ( + )); ("`-", ( - )); ("`*", ( * )); ("`/", ( / )) ]
  in
  Environment.empty
  |> bind_all binarith binarithops

let initial_runtime () = {
  environment = primitives;
}

let rec evaluate runtime ast =
  let runtime' = List.fold_left definition runtime ast in
  (runtime', extract_observable runtime runtime')

and definition runtime d =
  match Position.value d with
  | DefineValue (x, e) ->
     let runtime = bind_identifier runtime x (expression' runtime e) in
    runtime

and expression' runtime e =
  expression (position e) runtime (value e)

and expression position runtime = function
  | Apply (a, b) ->
    let vb = expression' runtime b in
    begin match expression' runtime a with
      | VPrimitive (_, f) ->
        f vb
      | _ ->
        assert false (* By typing. *)
    end

  | IfThenElse(c,e1,e2) -> (let cond = expression' runtime c
				  in(match cond with
				     | VBool(true) -> expression' runtime e1
				     | VBool(false) -> expression' runtime e2
				     | _ -> failwith "ERROR -_- "))
  | Literal l ->
    literal l

  | Variable x ->
    Environment.lookup x runtime.environment

  | Define (x, ex, e) ->
    let v = expression' runtime ex in
    expression' (bind_identifier runtime x v) e

and bind_identifier runtime x v =
  { environment = Environment.bind runtime.environment (Position.value x) v }

and literal = function
  | LInt x -> VInt x
  | LBool x -> VBool x

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
      substract Environment.empty runtime.environment runtime'.environment
  }

let print_observable runtime observation =
  Environment.print observation.new_environment
