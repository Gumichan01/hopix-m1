open HopixAST
open Dict

module PrimitiveTypes = struct

  let constant s = TyCon (TCon s, [])

  let int    = constant "int"
  let string = constant "string"
  let char   = constant "char"
  let bool   = constant "bool"

  let as_arrow = function
    | TyCon (TCon "->", [i; o]) ->
      Some (i, o)
    | _ ->
      None

end

(** Type checker error message producer. *)
let error = Error.error "during type checking"

(** Γ ::= • | Γ (x : τ) | Γ dₜ *)
type typing_environment = {
  variables    : (identifier, ty) dict;
  typecons     : (type_constructor, type_variable list * type_definition) dict;
  constructors : (constructor, type_constructor * type_variable list * ty list) dict;
  labels       : (label, type_constructor * type_variable list * (label * ty) list) dict
}

let initial_typing_environment () =
  {
    variables = Dict.empty;
    typecons = Dict.empty;
    constructors = Dict.empty;
    labels = Dict.empty
  }

let bind_value_type x ty tenv =
  { tenv with variables = Dict.insert x ty tenv.variables }

let lookup_value_type x tenv =
  Dict.lookup x tenv.variables

let bind_type_definition t ts tdef tenv =
  { tenv with typecons = Dict.insert t (ts, tdef) tenv.typecons }

let print_typing_environment tenv =
  String.concat "\n" (List.map (fun (Id s, v) ->
    Printf.sprintf "%s : %s" s (HopixPrettyPrinter.(to_string ty v))
  ) (Dict.to_list tenv.variables))

let types_of_constructor tenv k =
  Dict.lookup k tenv.constructors

let types_of_label tenv l =
  Dict.lookup l tenv.labels

let equal_typing_environments tenv1 tenv2 =
  Dict.equal tenv1.variables tenv2.variables
  && Dict.equal tenv1.typecons tenv2.typecons
  && Dict.equal tenv1.constructors tenv2.constructors
  && Dict.equal tenv1.labels tenv2.labels
