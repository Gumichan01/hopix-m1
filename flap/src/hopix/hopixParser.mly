%{

  open HopixAST


%}

%token VAL TYPE REC AND EXTERN 
%token RARROW LARROW EQRARROW
%token LPAREN RPAREN
%token SEMICOLON DOT DDOT DEQUAL EOF COMMA VBAR AMP QMARK UNDERSCORE
%token DO DONE
%token IF THEN ELSE
%token HASHTAG BACKSLASH
%token LCBRACK RCBRACK
%token LSBRACK RSBRACK
%token<Int32.t> INT
%token<char> CHAR
%token<string> PLUS MINUS STAR SLASH DBLAND DBLOR STRING
%token<string> EQUAL INFEQU SUPEQU INF SUP
%token<string> ID (*INFIXID*) TYPE_VAR MASTER_TKN CONSTR

(* %nonassoc PLUS *)
%left DBLOR
%left DBLAND
%left INF
%left PLUS MINUS
%left STAR SLASH



%start<HopixAST.t> program

%%

program: ds=located(definition)* EOF
{
  ds
} 


(* -----------------------DEFINITIONS DE TYPES ET DE VALEURS -------------------------------------------- *)


    
definition: 
(* type type_cons := tdefinition *)
| TYPE x=located(type_cons) DEQUAL td=tdefinition DOT
{
  DefineType(x,[],td)
}
(* La même chose mais içi on prend en compte la partie optionnelle *)
(* type type_cons [ type_variable, type_variable, ... ] := tdefinition *)
| TYPE x=located(type_cons)
    l=loption(delimited(LSBRACK, separated_list(COMMA, located(type_ty)), RSBRACK)) DEQUAL? td=tdefinition DOT
{
  DefineType(x,l,td)
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
VAL x=located(identifier) list(located(simple_pattern)) option(preceded(DDOT,located(ty))) DEQUAL e=located(expression) DOT
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
LCBRACK x=separated_nonempty_list(SEMICOLON,
			 separated_pair(
			   located(lab),
			   DDOT,
			   located(ty))) RCBRACK
{
  DefineRecordType(x)
}
(* Type somme *)
| LCBRACK option(VBAR) x=separated_list(VBAR,pair(located(constr),
					loption(preceded(DDOT,
						separated_nonempty_list(STAR,located(ty)))))) RCBRACK
{
  DefineSumType(x)
}


(* -------------------------TYPES DE DONNEES--------------------------------------- *)
    
(* type *)
ty:
vs=type_ty
{
  TyVar(vs)
}
| vs=type_cons l=loption(delimited(LSBRACK,typelist,RSBRACK))
{
  TyCon (vs,l)
}
(* | t1=separated_pair(ty,RARROW,ty) *)
(* { *)
(*   t1 *)
(* } *)


(* ---------------------------- EXPRESSIONS -------------------------------------------- *)

expression:
s=simple_expression 		(* Simple expression *)
{
      s
}
| lhs=located(expression) b=located(binop) rhs=located(expression) (* Opération binaire *)
{
  let op = Position.(map (fun x -> Variable (map (fun _ -> Id x) b))) b in
  let app1 = Position.with_poss $startpos(lhs) $endpos(b) (Apply (op, lhs)) in
  Apply (app1, rhs)
}
| x=located(identifier) 	(* Variable *)
{
  Variable(x)
}
| x=located(constr) y=separated_list(COMMA,located(expression)) (* Construction d'une donnée étiquetée *)
{
  Tagged(x,y)
}
| LCBRACK x=separated_list(SEMICOLON,separated_pair(located(lab),DEQUAL,located(expression))) RCBRACK (* Construction d'un enregistrement *)
{
  Record(x)
}
| LPAREN x=located(expression) DDOT y=located(ty) RPAREN (* Annotation de type *)
{
  TypeAnnotation(x,y)
}
(* } *)
(* | DO x=separated_list(SEMICOLON,expression) option(SEMICOLON) DONE *)
(* { *)
(*   x *)
(* } *)
(* | v=located(vdefinition) SEMICOLON e=located(expression) *)
(*     { *)

(*     } *)
| BACKSLASH p=located(pattern) EQRARROW e=located(expression) (* Fonction anonyme *)
    {
      Fun(p,e)
    }
| x=located(expression) HASHTAG y=located(lab) (* Accès à un champ *)
{
  Field(x,y)
}
| x=located(expression) HASHTAG y=located(lab) LARROW z=located(expression) (* Modification d'un champ *)
{
  ChangeField(x,y,z)
}
| IF x=located(expression) THEN y=located(expression) ELSE z=located(expression) (* Conditionnelle *)
{
  IfThenElse(x,y,z)
}
| LPAREN e=expression RPAREN (* Parenthésage *)
{
  e
}
| e=located(expression) QMARK option(VBAR) b=separated_list(VBAR,located(branch)) (* Analyse de motifs *)
    {
      Case(e,b)
    }
| e=located(expression) QMARK LCBRACK option(VBAR) b=separated_list(VBAR,located(branch)) RCBRACK (* Idem *)
    {
      Case(e,b)
    }
(* | v=located(vdefinition) COMMA x=located(expression) (\* Définition locale *\) *)
(* { *)
  


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

typelist:
t=separated_list(COMMA,located(ty))
    {
      t
    }


(* -----------------------PATTERNS------------------------------------------------- *)

pattern: sp=simple_pattern
{
  sp
}
| x=located(constr) LPAREN y=separated_list(COMMA, located(pattern)) RPAREN (* Valeurs étiquetées *)
    {
      PTaggedValue(x,y)
    }
(* | x=separated_list(AMP,located(pattern)) (\* Conjonction *\) *)
(*     { *)
(*       PAnd(x) *)
(*     } *)
(* | x=separated_nonempty_list(VBAR,located(pattern)) (\* Disjonction *\) *)
(*     { *)
(*       POr(x) *)
(*     } *)


	
simple_pattern: x=located(identifier) (* Motif universel liant *)
    {
      PVariable x
    }
| LPAREN x=located(pattern) DDOT t=located(ty) RPAREN (* Annotation de type *)
    {
      PTypeAnnotation(x,t)
    }
| LCBRACK l=separated_list(SEMICOLON,separated_pair(located(lab),EQUAL,located(pattern))) RCBRACK (* Enregistrement *)
    {
      PRecord(l)
    }
| l=located(literal)
    {
      PLiteral l
    }
| UNDERSCORE
	{
	  PWildcard
	}
| x=located(constr)
    {
      PTaggedValue(x,[])
    }

(* | x=located(constr) *)
(*     { *)
(*       x *)
(*     } *)
(* | LPAREN x=located(pattern) RPAREN (\* Parenthésage *\) *)
(*     { *)
(*       x *)
(*     } *)


(* -------------------------------OPERATEURS BINAIRES---------------------------------------- *)
    
%inline binop:
| PLUS { "`+"  }
| MINUS { "`-"  }
| STAR  { "`*"  }
| SLASH { "`/"  }
| DBLAND { "`&&" }
| DBLOR { "`||"}
| EQUAL { "`=" }
| INFEQU { "`<=" }
| SUPEQU { "`>="}
| INF { "`<"}
| SUP {"`>"}


(* ------------------------------LISTE DE CAS---------------------------------------------------- *)

branch: p=located(pattern) EQRARROW e=located(expression)
    {
      (* print_string("BRANCH"); *)
      Branch(p,e)
    }

(* branches: option(VBAR) m=separated_list(VBAR,located(branch)) *)
(*     { *)
(*       m *)
(*     } *)
(* | LCBRACK option(VBAR) m=separated_list(VBAR,located(branch)) RCBRACK *)
(*     { *)
(*       m *)
(*     } *)







    

%inline literal:
| x=INT
{
  LInt x
}
| x=CHAR
{
  LChar x
}
| x=STRING
{
  LString x
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
