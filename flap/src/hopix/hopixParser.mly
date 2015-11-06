%{

  open HopixAST


%}

%token VAL TYPE REC AND EXTERN 
%token RARROW
%token LPAREN RPAREN
%token SEMICOLON DOT DDOT DEQUAL EOF
%token<int> INT
%token<string> PLUS MINUS STAR SLASH
%token<string> ID TYPE_VAR MASTER_TKN

%nonassoc PLUS
%left MINUS
%nonassoc STAR 
%nonassoc SLASH



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

(* Definition de variable/fonction *)
vdefinition:
(* val var_id :=  expr *)
VAL x=located(identifier) DEQUAL e=located(expression) DOT
{
  DefineValue (x, e)
}
(* rec var_id :=  expr { and var_id := expr } *)
| REC x=separated_list(AND,separated_pair(located(identifier),
					  DEQUAL,
					  located(expression))
		      ) DOT
{
  DefineRecValue(x)
}
(* extern var_id : type (içi 'ty') *)
| EXTERN x=located(identifier) DDOT y=located(ty)
{
  DeclareExtern(x,y)
}
(* extern var_id : type (içi 'ty') *)
| EXTERN x=located(identifier) DDOT y=located(ty)
{
  DeclareExtern(x,y)
}

(* type *)
ty:
vs=type_ty
{
  print_string("TyVar parsed\n");
  TyVar(vs)
}
| vs=type_cons
{
  print_string("TyCon parsed\n");
  TyCon (vs,[])
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
| PLUS { "`+"  }
| MINUS { "`-"  }
| STAR  { "`*"  }
| SLASH { "`/"  }

%inline literal:
  x=INT
{
  LInt x
}


%inline type_ty: str=TYPE_VAR
{
  TId str
}

%inline type_cons: str=MASTER_TKN
{
  TCon str
}

%inline identifier: x=ID |  x=MASTER_TKN
{
  Id x
}


%inline located(X): x=X 
{
  Position.with_poss $startpos $endpos x
}
