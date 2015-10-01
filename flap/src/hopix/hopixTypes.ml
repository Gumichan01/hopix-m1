open HopixAST

(** Type checker error message producer. *)
let error = Error.error "during type checking"

type typing_environment = unit

let initial_typing_environment () = ()
