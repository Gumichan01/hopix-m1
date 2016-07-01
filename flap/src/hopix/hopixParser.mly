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

(* The precedence of VAL, IF, REC, DO, DONE, DEQUAL, VBAR, AMP
   has no effect on the conflict resolutions *)
%right EQRARROW
%right VBAR
%right QMARK
%right RARROW
%right SEMICOLON
%left INFIXID
%left LARROW
%left BOOLOR
%left BOOLAND
%nonassoc LT LTE EQUAL GTE GT
%left PLUS MINUS
%left MULT DIVIDE
%left SHARP

%start<HopixAST.t> program

%%

(* [hopix_parser_separated_nonempty_list(separator, X)]
   recognizes a nonempty list of [X]'s, separated with [separator]'s.
   It produces a value of type ['a list] if [X] produces a value of type ['a].
   The front element of the list is the first element that was parsed.
   This rule is based on separated_nonempty_list provided by menhir
   and adapted for this hopix parser *)

%public hopix_parser_separated_nonempty_list(separator, X):
x = X %prec VBAR
    { [ x ] }
| x = X; separator; xs = hopix_parser_separated_nonempty_list(separator, X)
    { x :: xs }
    
program: ds=located(definition)* EOF
{
  ds
}


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
| REC i=located(identifier) le=located(list_n_expr)
  o=option(preceded(AND,separated_list(AND,separated_pair(located(identifier),
					    DEQUAL,
					    located(expression)))))
{
  (function
    | None   -> DefineRecValue([(i,le)])
    | Some x -> DefineRecValue((i,le)::x)) o
}


%inline list_n_expr:
| DEQUAL e=located(expression)
{
  Position.value(e)
}

| l=nonempty_list(simple_pattern) o=option(preceded(DDOT,located(ty)))
  DEQUAL e=located(expression)
{
  let e' =
    (function
      | None   -> e
      | Some t -> Position.with_poss $startpos $endpos (TypeAnnotation(e,t))) o
  in
  let rec gen_fun = function
    | []            -> assert false (* by typing *)
    | [h]           -> Fun((Position.with_poss $startpos $endpos h), e')
    | head :: tails -> Fun((Position.with_poss $startpos $endpos head),
                           (Position.with_poss $startpos $endpos (gen_fun tails)))
  in gen_fun l
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
| LCBRACK option(VBAR) x=separated_nonempty_list(VBAR,pair(located(constr),
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
(* Construction d'une donnée étiquetée de type somme *)
| x=located(constr) y=delimited(LPAREN,
                                separated_list(COMMA,located(expression)),
                                RPAREN)
{
  Tagged(x,y)
}
| x=located(constr)
{
  Tagged(x,[])
}

(* Construction d'un enregistrement *)
|   LCBRACK x=separated_nonempty_list(SEMICOLON,
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

| e=delimited(DO,expression,DONE)
{
  e
}

(* Définition locale *)
| VAL x=located(identifier) DEQUAL y=located(expression)
  SEMICOLON z=located(expression)
{
  Define(x,y,z)
}
(* Fonction anonyme *)
| BACKSLASH pl=nonempty_list(simple_pattern) EQRARROW e=located(expression)
{
  let rec anon_fun = function
  | []            -> assert false (* by typing *)
  | [h]           -> Fun((Position.with_poss $startpos $endpos h), e)
  | head :: tails -> Fun((Position.with_poss $startpos $endpos head),
                         (Position.with_poss $startpos $endpos (anon_fun tails)))
  in anon_fun pl
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
| IF x=located(expression)
  THEN y=located(expression)
  ELSE z=located(expression) FI
{
  IfThenElse(x,y,z)
}
(* Analyse de motifs *)
| e=located(expression) QMARK b=branches
{
  Case(e,b)
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
(* Variable *)
| x=located(identifier)
{
  Variable(x)
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
| x=located(constr) LPAREN y=separated_nonempty_list(COMMA, located(pattern)) RPAREN
{
  PTaggedValue(x,y)
}
| x=located(simple_pattern) AMP y=located(pattern)
{
  PAnd([x;y])
}

| x=located(simple_pattern) VBAR y=located(pattern)
{
  POr([x;y])
}


(* Motif universel liant *)
simple_pattern: x=located(identifier)
{
  PVariable x
}
(* Annotation de type ou bien pattern entre parenthèses *)
| LPAREN x=located(pattern) o=option(pair(DDOT,located(ty))) RPAREN
{
  (function
    | None -> Position.value x
    | Some (_,t) -> PTypeAnnotation(x,t)) o
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

%inline branches: VBAR m=hopix_parser_separated_nonempty_list(VBAR,located(branch))
{
  m
}
| b=hopix_parser_separated_nonempty_list(VBAR,located(branch))
{
  b
}
| LCBRACK VBAR m=hopix_parser_separated_nonempty_list(VBAR,located(branch)) RCBRACK
{
  m
}
| LCBRACK m=hopix_parser_separated_nonempty_list(VBAR,located(branch)) RCBRACK
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
