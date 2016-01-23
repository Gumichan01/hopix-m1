%{

  open HopixAST


%}

%token VAL TYPE REC AND EXTERN 
%token RARROW LARROW EQRARROW
%token LPAREN RPAREN
%token SEMICOLON DOT DDOT DEQUAL EOF COMMA VBAR AMP QMARK UNDERSCORE
%token DO DONE
%token IF THEN ELSE FI
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


(* -----------------------DEFINITIONS DE TYPES ET DE VALEURS ------------------- *)


    
definition: 
(* type type_cons := tdefinition *)
| TYPE x=located(type_cons) DEQUAL td=tdefinition DOT
{
  DefineType(x,[],td)
}
(* La m�me chose mais i�i on prend en compte la partie optionnelle *)
(* type type_cons [ type_variable, type_variable, ... ] := tdefinition *)
| TYPE x=located(type_cons)
    l=loption(delimited(LSBRACK, separated_list(COMMA,located(type_ty)),
			RSBRACK)) DEQUAL? td=tdefinition DOT
{
  DefineType(x,l,td)
}
(* extern var_id : type *)
| EXTERN x=located(identifier) DDOT y=located(ty) DOT
{
  DeclareExtern(x,y)
}
(* Sinon c'est une vdefinition *)
| vd=vdefinition DOT
{
  vd
}


(** Definition de variable/fonction *)
vdefinition:
(* val var_id :=  expr *)
VAL x=located(identifier) DEQUAL e=located(expression)
    {
      DefineValue(x,e)
    }
| VAL x=located(identifier) DDOT t=located(ty) DEQUAL e=located(expression)
    {
      let te = Position.with_poss $startpos $endpos (TypeAnnotation(e,t)) in
      DefineValue (x, te)
    }
(* VAL x=located(identifier) list(located(simple_pattern)) option(preceded(DDOT, *)
(* 									located(ty))) DEQUAL e=located(expression) DOT *)
(* { *)
(*   DefineValue (x, e) *)
(* } *)
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



(* -------------------------TYPES DE DONNEES------------------------------- *)
    
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
| LPAREN t=ty RPAREN
{
  t
}
| t1=located(ty) RARROW t2=located(ty)
{
  let t=TCon "->" in
  TyCon (t, [t1;t2])
}


(* ---------------------------- EXPRESSIONS ------------------------------ *)

expression:
(* Simple expression *)
s=simple_expression
{
      s
}
(* Op�ration binaire *)
| lhs=located(expression) b=located(binop) rhs=located(expression)
{
  let op = Position.(map (fun x -> Variable (map (fun _ -> Id x) b))) b in
  let app1 = Position.with_poss $startpos(lhs) $endpos(b) (Apply (op, lhs)) in
  Apply (app1, rhs)
}

(* Variable *)
| x=located(identifier)
{
  Variable(x)
}
(* Construction d'une donn�e �tiquet�e *)
| x=located(constr) y=separated_list(COMMA,located(expression)) 
{
  Tagged(x,y)
}
(* Construction d'un enregistrement *)
|   LCBRACK x=separated_list(SEMICOLON,
			     separated_pair(located(lab),
					    DEQUAL,located(expression))) RCBRACK
{
  Record(x)
}
(* Annotation de type *)
| LPAREN x=located(expression) DDOT y=located(ty) RPAREN
    {
  TypeAnnotation(x,y)
}   
(* 
| DO x=separated_list(SEMICOLON,expression) option(SEMICOLON) DONE
{ 
   x 
} 
*)
(* D�finition locale *)
| VAL x=located(identifier) DEQUAL y=located(expression) SEMICOLON z=located(expression) 
{
  Define(x,y,z)
}
(* Fonction anonyme *)
| BACKSLASH p=located(pattern) EQRARROW e=located(expression)
{
  Fun(p,e)
}
(* Acc�s � un champ *)
| x=located(expression) HASHTAG y=located(lab) 
{
  Field(x,y)
}
(* Modification d'un champ *)
| x=located(expression) HASHTAG y=located(lab) LARROW z=located(expression)
{
  ChangeField(x,y,z)
}
(* Instruction conditionnelle *)
| IF x=located(expression) THEN y=located(expression) ELSE z=located(expression) FI 
{
  IfThenElse(x,y,z)
}
(* Parenth�sage *)
| LPAREN e=expression RPAREN
{
  e
}
(* Analyse de motifs *)
| e=located(expression) QMARK option(VBAR) b=separated_list(VBAR,located(branch))
{
  Case(e,b)
}
(* Idem *)
| e=located(expression) QMARK LCBRACK option(VBAR) b=separated_list(VBAR,
								    located(branch)) RCBRACK
{
  Case(e,b)
}
(* D�finition locale *)
(* | v=located(vdefinition) COMMA x=located(expression)*)
  

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


(* -----------------------PATTERNS---------------------------------------- *)

pattern: sp=simple_pattern
{
  sp
}
(* Valeurs �tiquett�es *)
| x=located(constr) LPAREN y=separated_list(COMMA, located(pattern)) RPAREN
{
  PTaggedValue(x,y)
}
(* | x=separated_list(AMP,located(pattern)) (\* Conjonction *\)
{ 
   PAnd(x)
}
*)
(*| x=separated_nonempty_list(VBAR,located(pattern)) (\* Disjonction *\)
{
  POr(x)
} 
*)


(* Motif universel liant *)
simple_pattern: x=located(identifier)
{
      PVariable x
}
(* Annotation de type *)
| LPAREN x=located(pattern) DDOT t=located(ty) RPAREN
{
  PTypeAnnotation(x,t)
}
(* Enregistrement *)
| LCBRACK l=separated_list(SEMICOLON,separated_pair(located(lab),
						    EQUAL,located(pattern))) RCBRACK 
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

(* | x=located(constr) 
{ 
 x
} 
*)
(* Parenth�sage *)
(* | LPAREN x=located(pattern) RPAREN 
{ 
 x 
}
*)


(* -------------------------------OPERATEURS BINAIRES----------------------- *)
    
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


(* ------------------------------LISTE DE CAS-------------------------------*)

branch: p=located(pattern) EQRARROW e=located(expression)
    {
      (* print_string("BRANCH"); *)
      Branch(p,e)
    }

(* branches: option(VBAR) m=separated_list(VBAR,located(branch))
{ 
   m
} 
*)
(* | LCBRACK option(VBAR) m=separated_list(VBAR,located(branch)) RCBRACK 
{ 
   m 
} 
*)


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
