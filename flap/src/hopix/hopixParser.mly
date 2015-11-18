%{

  open HopixAST


%}

%token VAL TYPE REC AND EXTERN 
%token RARROW
%token LPAREN RPAREN
%token SEMICOLON DOT DDOT DEQUAL EOF COMMA VBAR
%token LCBRACK RCBRACK
%token LSBRACK RSBRACK
%token<Int32.t> INT
%token<string> PLUS MINUS STAR SLASH
%token<string> ID (*INFIXID*) TYPE_VAR MASTER_TKN CONSTR

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

definition: 
(* type type_cons := tdefinition *)
| TYPE x=located(type_cons) DEQUAL td=tdefinition DOT
{
  DefineType(x,[],td)
}
(* La même chose mais içi on prend en compte la partie optionnelle *)
(* type type_cons [ type_variable, type_variable, ... ] := tdefinition *)
| TYPE x=located(type_cons) LSBRACK y=separated_list(COMMA, located(type_ty)) 
	RSBRACK DEQUAL td=tdefinition DOT
{
  DefineType(x,y,td)
}
(* extern var_id : type *)
| EXTERN x=located(identifier) DDOT y=located(ty) DOT
{
  DeclareExtern(x,y)
}
(* Sinon c'est une vdefinition *)
| vd=vdefinition 
{
  vd
}


(** Definition de variable/fonction *)
vdefinition:
(* val var_id :=  expr *)
VAL x=located(identifier) option(DDOT) option(ty) DEQUAL e=located(expression) DOT
{
  (*print_string("\nval VAR_ID := EXPR parsed\n");*)
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


(** Definition de types *)
tdefinition:
(* Type enregistrement *)
(* { label_id : type { ; label_id : type } } *)
LCBRACK x=separated_list(SEMICOLON,
			 separated_pair(
			   located(lab),
			   DDOT,
			   located(ty))) RCBRACK
{
  DefineRecordType(x)
}
(* Type somme *)
(*  NE PAS SUPRR Cas complet avec la partie optionnelle *)
(* Fonctionne pour 
    > type tree := { Leaf : int * int * char | Node : int }.
 avec ou sans | *)
(* Ne marche pas avec 
    > type tree := { Leaf }.
    > type tree := { Leaf : int }.
    > type tree := { Leaf : int | Node : char }.
    > type tree := { Leaf | Node }.
    > type tree := { | Leaf | Node }.  
*)
| LCBRACK option(VBAR) x=separated_list(VBAR,
					separated_pair(located(constr),
							DDOT,
							separated_list(STAR,
							located(ty)))) RCBRACK
{
  DefineSumType(x)
}

    
(* type *)
ty:
vs=type_ty
{
  (*print_string("\nTyVar parsed\n");*)
  TyVar(vs)
}
| vs=type_cons
{
  (*print_string("\nTyCon parsed\n");*)
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

%inline constr: str=CONSTR
{
  KId str
}

%inline lab: str=MASTER_TKN
{
  LId str
}
    
%inline located(X): x=X 
{
  Position.with_poss $startpos $endpos x
}
