(** This module implements a type checker for Datix. *)
open HopixAST
open HopixTypes

let initial_typing_environment = HopixTypes.initial_typing_environment
type typing_environment = HopixTypes.typing_environment

(** [typecheck tenv ast] checks that [ast] is a well-formed program
    under the typing environment [tenv]. *)
let typecheck tenv ast =
  ()
