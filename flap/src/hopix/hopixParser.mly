%{

  open HopixAST


%}

%token VAL TYPE REC AND
%token IF THEN ELSE FI TRUE FALSE
%token PLUS MINUS STAR SLASH
%token EQUAL LTE GTE LT GT
%token LPAREN RPAREN
%token SEMICOLON DOT DEQUAL EOF
%token<int> INT
%token<string> ID INFIXID


%start<HopixAST.t> program

%%

program: ds=located(definition)* EOF
{
  ds
}

definition: vd=vdefinition
{
  vd
}

(* Definition de variable *)
vdefinition:
VAL x=located(identifier) DEQUAL e=located(expression) DOT
{
  DefineValue (x, e)
}
| REC x=located(identifier) DEQUAL e=located(expression) DOT
{
  DefineValue (x, e)
}
| REC x=located(identifier) DEQUAL e=located(expression)
					    AND y=located(identifier)
							 DEQUAL f=located(expression)
									 DOT
{
  DefineRecValue([(x,e);(y,f)])
}

expression:
s=simple_expression
{
      s
}
| lhs=located(expression) b=located(binop) rhs=located(expression)
{
  let op = Position.(map (fun x -> Variable (map (fun _ -> Id x) b))) b in
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
  l=located(literal)
{
  Literal l
}
| x=located(identifier)
{
  Variable x
}
| LPAREN e=expression RPAREN
{
  e
}

%inline binop:
  x=INFIXID { String.(sub x 0 (length x - 1)) }
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
