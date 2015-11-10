exception Error

type token = 
  | VAL
  | TYPE_VAR of (string)
  | TYPE
  | STAR of (string)
  | SLASH of (string)
  | SEMICOLON
  | RSBRACK
  | RPAREN
  | REC
  | RCBRACK
  | RARROW
  | PLUS of (string)
  | MINUS of (string)
  | MASTER_TKN of (string)
  | LSBRACK
  | LPAREN
  | LCBRACK
  | INT of (int)
  | ID of (string)
  | EXTERN
  | EOF
  | DOT
  | DEQUAL
  | DDOT
  | COMMA
  | AND


val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (HopixAST.t)