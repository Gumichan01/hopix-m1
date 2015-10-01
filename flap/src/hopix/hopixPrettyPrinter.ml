open PPrint
open PPrintCombinators
open PPrintEngine
open ExtPPrint
open HopixAST
open Position

let int i = string (string_of_int i)

let rec program p =
  separate_map hardline (located definition) p

and definition = function
  | DefineValue (x, e) ->
    nest 2 (
      group (string "val" ++ located identifier x ++ string ":=")
      ++ group (located expression e) ^^ string "."
    )

and identifier (Id x) =
  string x

and expression = function
  | Literal l ->
    literal l

  | Variable x ->
    identifier x

  | Define (x, e1, e2) ->
    nest 2 (
      group (string "val" ++ located identifier x ++ string ":=")
      ++ group (located expression e1)
      ^^ string ";"
    )
    ++ group (located expression e2)

  | Apply (a, b) ->
    group (
      parens_at_left_of_application a (located expression a)
      ++ parens_at_right_of_application b (located expression b)
    )

  | IfThenElse (c, t, f) ->
    nest 2 (
      group (string "if"
             ++ group (located expression c)
             ++ string "then"
      )
      ++ group (located expression t)
      ++ string "else"
      ++ group (located expression f)
      ++ string "fi"
    )


and literal = function
  | LInt x ->
    int x

and parens_at_left_of_application e =
  match Position.value e with
  | Apply _ | Variable _ | Literal _ -> fun x -> x
  | _ -> parens

and parens_at_right_of_application e =
  match Position.value e with
  | Variable _ | Literal _ -> fun x -> x
  | _ -> parens

let to_string f x =
  let b = Buffer.create 13 in
  ToBuffer.pretty 0.5 80 b (f x);
  Buffer.contents b
