%{

  open HopixAST

%}

%token VAL TYPE REC AND EXTERN
%token RARROW LARROW EQRARROW
%token LPAREN RPAREN
%token SEMICOLON DOT DDOT DEQUAL EOF COMMA VBAR AMP QMARK UNDERSCORE
%token DO DONE
%token IF THEN ELSE FI
%token SHARP BACKSLASH
%token LCBRACK RCBRACK
%token LSBRACK RSBRACK
%token<bool> BOOL
%token<Int32.t> INT
%token<char> CHAR
%token PLUS MINUS MULT DIVIDE BOOLAND BOOLOR
%token LT LTE EQUAL GTE GT
%token<string> ID INFIXID TYPE_VAR MASTER_TKN CONSTR STRING

(* The precedence of VAL, IF, REC, DO, DONE, DEQUAL, VBAR
   has no effect on the conflict resolutions *)
%right SEMICOLON RARROW LARROW EQRARROW
%right BOOLOR
%right BOOLAND
%nonassoc EQUAL
%left LT LTE GTE GT
%left PLUS MINUS
%left MULT DIVIDE
%left SHARP


%start<HopixAST.t> program

%%

program: ds=located(definition)* EOF
{
  ds
}

(** Type and value definition *)

definition:
| TYPE x=located(type_cons)
  l=delimited(LSBRACK, separated_list(COMMA,located(type_ty)),RSBRACK)
  DEQUAL td=tdefinition DOT
{
  DefineType(x,l,td)
}
(* type type_cons := tdefinition *)
| TYPE x=located(type_cons) DEQUAL td=tdefinition DOT
{
  DefineType(x,[],td)
}

| TYPE x=located(type_cons)
  l=delimited(LSBRACK, separated_list(COMMA,located(type_ty)),RSBRACK) DOT
{
  DefineType(x,l,HopixAST.Abstract)
}
| TYPE x=located(type_cons) DOT
{
  DefineType(x,[],HopixAST.Abstract)
}

(* extern var_id : type *)
| EXTERN x=located(identifier) DDOT y=located(ty) DOT
{
  DeclareExtern(x,y)
}
(* Sinon → vdefinition *)
| vd=vdefinition DOT
{
  vd
}


(** Definition de variable/fonction *)
vdefinition:
| VAL x=located(identifier) le=located(list_n_expr)
{
  DefineValue (x,le)
}
| VAL x=located(identifier) DDOT t=located(ty) DEQUAL e=located(expression)
{
  let te = Position.with_poss $startpos $endpos (TypeAnnotation(e,t)) in
  DefineValue (x, te)
}
(* rec var_id :=  expr { and var_id := expr } *)
| REC x=separated_list(AND,separated_pair(located(identifier),
					   DEQUAL,
					   located(expression)))
{
  DefineRecValue(x)
}

%inline list_n_expr:
| DEQUAL e=located(expression)
{
    Position.value(e)
}
| l=nonempty_list(simple_pattern) DEQUAL e=located(expression)
{
  let rec reclist = function
  | []            -> assert false (* by typing *)
  | [h]           -> Fun((Position.with_poss $startpos $endpos h), e)
  | head :: tails -> Fun((Position.with_poss $startpos $endpos head),
                         (Position.with_poss $startpos $endpos (reclist tails)))
  in reclist l
}

(** Definition de types *)
tdefinition:
(* Type enregistrement *)
(* { label_id : type { ; label_id : type } } *)
LCBRACK x=separated_nonempty_list(SEMICOLON,
          separated_pair(located(lab),DDOT,located(ty))) RCBRACK
{
  DefineRecordType(x)
}
(* Type somme *)
| LCBRACK option(VBAR) x=separated_list(VBAR,pair(located(constr),
                          loption(preceded(DDOT,
                           separated_nonempty_list(MULT,located(ty)))))) RCBRACK
{
      DefineSumType(x)
}
|
{
  Abstract
}



(** Data type *)

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
  TyCon ((TCon "->"), [t1;t2])
}


(** Expression *)

expression:
(* Simple expression *)
s=simple_expression
{
  s
}
(* Opération binaire *)
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
(* Construction d'une donnée étiquetée *)
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

| DO x=closed_expression DONE
{
  x
}

(* Définition locale *)
| VAL x=located(identifier) DEQUAL y=located(expression) SEMICOLON z=located(expression)
{
  Define(x,y,z)
}
(* Fonction anonyme *)
| BACKSLASH p=located(pattern) EQRARROW e=located(expression)
{
  Fun(p,e)
}
(* Acces à un champ *)
| x=located(expression) SHARP y=located(lab)
{
  Field(x,y)
}
(* Modification d'un champ *)
| x=located(expression) SHARP y=located(lab) LARROW z=located(expression)
{
  ChangeField(x,y,z)
}
(* Instruction conditionnelle *)
| IF x=located(expression) THEN y=located(expression) ELSE z=located(expression) FI
{
  IfThenElse(x,y,z)
}
(* Parenthésage *)
| LPAREN e=expression RPAREN
{
  e
}
(* Analyse de motifs *)
| e=located(expression) QMARK b=branches
{
      Case(e,b)
}


closed_expression:
| x=located(expression)
{
    Position.value(x)
}



simple_expression:
| e=very_simple_expression
{
  e
}
| a=located(simple_expression) b=located(very_simple_expression)
{
  Apply (a, b)
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


(** Patterns *)

pattern: sp=simple_pattern
{
  sp
}
(* Valeurs étiquettées *)
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
(* Parenthésage *)
(* | LPAREN x=located(pattern) RPAREN
{
 x
}
*)


(** Binary operations *)

%inline binop:
| PLUS      { "`+"  }
| MINUS     { "`-"  }
| MULT      { "`*"  }
| DIVIDE    { "`/"  }
| BOOLAND   { "`&&" }
| BOOLOR    { "`||" }
| EQUAL     { "`="  }
| LTE       { "`<=" }
| GTE       { "`>=" }
| LT        { "`<"  }
| GT        { "`>"  }
| x=INFIXID { String.(sub x 0 (length x - 1)) }


(** Branches *)

branch: p=located(pattern) EQRARROW e=located(expression)
{
  Branch(p,e)
}

%inline branches: option(VBAR) m=separated_list(VBAR,located(branch))
{
  m
}
| LCBRACK option(VBAR) m=separated_list(VBAR,located(branch)) RCBRACK
{
  m
}


%inline literal:
| x=BOOL
{
    LBool x
}
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

%inline identifier: x=ID | x=MASTER_TKN
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
