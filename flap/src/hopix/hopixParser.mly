%{

  open HopixAST

%}

%token VAL
%token PLUS MINUS STAR SLASH
%token LPAREN RPAREN
%token SEMICOLON DOT DEQUAL EOF
%token<int> INT
%token<string> ID INFIXID

%right SEMICOLON
%nonassoc INFIXID
%left PLUS MINUS
%left STAR SLASH

%start<HopixAST.t> program

%%

program: ds=located(definition)* EOF
{
  ds
}
| error {
  let pos = Position.lex_join $startpos $endpos in
  Error.error "parsing" pos "Syntax error."
}

definition: VAL x=located(identifier) DEQUAL e=located(expression) DOT
{
  DefineValue (x, e)
}

expression:
s=simple_expression
{
      s
}
| VAL x=located(identifier)
  DEQUAL
  e1=located(expression)
  SEMICOLON
  e2=located(expression)
{
  Define (x, e1, e2)
}
| lhs=located(expression) b=located(binop) rhs=located(expression)
{
  let op = Position.map (fun b -> Variable (Id b)) b in
  let app1 = Position.with_poss $startpos(lhs) $endpos(b) (Apply (op, lhs)) in
  Apply (app1, rhs)
}

simple_expression:
| a=located(simple_expression) b=located(very_simple_expression)
{
  Apply (a, b)
}
| e=very_simple_expression
{
  e
}

very_simple_expression:
  l=literal
{
  Literal l
}
| x=identifier
{
  Variable x
}
| LPAREN e=expression RPAREN
{
  e
}

%inline binop:
  x=INFIXID { String.(sub x 1 (length x - 2)) }
| PLUS  { "`+"  }
| MINUS { "`-"  }
| STAR  { "`*"  }
| SLASH { "`/"  }

%inline literal:
  x=INT
{
  LInt x
}

%inline identifier: x=ID {
  Id x
}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
