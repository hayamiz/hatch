/* File parser.mly */

%{

  open Syntax

  let rec make_let bs body =
	match bs with
		b1 :: b2 :: bs -> ExpLet (b1, (make_let (b2 :: bs) body))
	  | b :: [] -> ExpLet (b, body)
	  | [] -> raise (Failure "invalid argument for make_let")
  ;;

  let make_bind bs =
	match bs with
		(id, v) :: [] -> ExpBind (id, v)
	  | [] -> raise (Failure "invalid argument for make_let")
	  | _ -> ExpSeq (List.map (fun (id, v) -> ExpBind (id, v)) bs)
  ;;

  let make_comp_expr e se =
	match se with
		(ExpSeq (es)) ->
		  begin
			match es with
			  (e1 :: erest) -> 
				ExpSeq ((e :: es))
			  | [] -> e
		  end
	  | _ -> ExpSeq ([e; se])
  ;;

%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> IDENT
%token UNDEF
%token TRUE FALSE
%token LPAREN RPAREN
%token LBRAKET RBRAKET
%token LBRACE RBRACE
%token COMMA SEMICOLON DOT
%token PLUS MINUS MUL DIV
%token EQ LE GE LT GT
%token LAND LOR LNOT
%token LAMBDA
%token IF ELSE
%token BIND RARROW LET IN
%token EOF
%left LAND LOR          /* 5th precedence */
%left EQ LE GE LT GT    /* 4th precedence */
%left PLUS MINUS        /* 3rd precedence */
%left MUL DIV           /* 2nd precedence */
%right prec_prefix
%start main             /* the entry point */
%type <Syntax.egg_expr> main
%%
main:
    compound_expr EOF                { $1 }
;
literal:
    IDENT  { ExpLiteral (LitIdent $1) }
  | INT    { ExpLiteral (LitInt $1) }
  | FLOAT  { ExpLiteral (LitFloat $1) }
  | STRING { ExpLiteral (LitString $1) }
  | TRUE   { ExpLiteral (LitBool true) }
  | FALSE  { ExpLiteral (LitBool false) }
  | UNDEF  { ExpLiteral (LitUndef) }

expr:
    closure_expr        { $1 }
  | apply_expr          { $1 }
  | noparen_apply_expr  { $1 }
  | bind_expr           { $1 }
  | let_expr            { $1 }
  | infix_expr          { $1 }
  | block_expr          { $1 }
  | if_expr             { $1 }

primary_expr:
    literal             { $1 }
  | apply_expr          { $1 }
  | LPAREN expr RPAREN  { $2 }

closure_expr:
    LAMBDA LPAREN param_list RPAREN expr  { ExpLambda ($3, $5)}

argument_list:
    expr COMMA argument_list  { $1 :: $3 }
  | expr                      { [$1] }
  |                           { [] }

apply_expr:
    primary_expr LPAREN argument_list RPAREN               { ExpApply ($1, $3) }

noparen_argument_list:
    primary_expr COMMA noparen_argument_list { $1 :: $3 }
  | primary_expr                             { [$1] }

noparen_apply_expr:
    primary_expr noparen_argument_list                     { ExpApply ($1, $2) }

binding:
    IDENT RARROW expr                    { ($1, $3) }

bindings:
    binding COMMA bindings  { $1 :: $3 }
  | binding                 { [$1] }

bind_expr:
    BIND bindings  { make_bind ($2) }

let_expr:
    LET bindings IN expr     { make_let $2 $4 }
  | LET bindings LBRACE compound_expr RBRACE
                             { make_let $2 $4 }

prefix_expr:
    primary_expr                    { $1 }
  | PLUS  primary_expr %prec prec_prefix  { ExpPrefix (PrefixPlus,  $2) }
  | MINUS primary_expr %prec prec_prefix  { ExpPrefix (PrefixMinus, $2) }
  | LNOT  primary_expr              { ExpPrefix (PrefixLnot,  $2) }

infix_expr:
    prefix_expr                    { $1 }
  | infix_expr PLUS  infix_expr  { ExpInfix (InfixPlus,  $1, $3) }
  | infix_expr MINUS infix_expr  { ExpInfix (InfixMinus, $1, $3) }
  | infix_expr MUL   infix_expr  { ExpInfix (InfixMul,   $1, $3) }
  | infix_expr DIV   infix_expr  { ExpInfix (InfixDiv,   $1, $3) }
  | infix_expr EQ    infix_expr  { ExpInfix (InfixEq,    $1, $3) }
  | infix_expr LE    infix_expr  { ExpInfix (InfixLe,    $1, $3) }
  | infix_expr GE    infix_expr  { ExpInfix (InfixGe,    $1, $3) }
  | infix_expr LT    infix_expr  { ExpInfix (InfixLt,    $1, $3) }
  | infix_expr GT    infix_expr  { ExpInfix (InfixGt,    $1, $3) }
  | infix_expr LAND  infix_expr  { ExpInfix (InfixLand,  $1, $3) }
  | infix_expr LOR   infix_expr  { ExpInfix (InfixLor,   $1, $3) }

param_list:
    IDENT COMMA param_list   { $1 :: $3 }
  | IDENT                    { [$1] }
  |                          { [] }

block_expr:
    LBRACE compound_expr RBRACE   { $2 }
compound_expr:
    expr SEMICOLON compound_expr  { make_comp_expr $1 $3 }
  | expr                          { make_comp_expr $1 (ExpSeq ([]))}
  |                               { ExpSeq ([]) }

cond_expr:
    primary_expr        { $1 }
  | infix_expr          { $1 }

if_expr:
    IF cond_expr expr ELSE expr  { ExpIf ($2, $3, $5) }
  | IF cond_expr expr            { ExpIf ($2, $3, ExpNop) }
;

%%

  (* | LPAREN expr RPAREN      { $2 } *)
  (* | expr PLUS expr          { $1 + $3 } *)
  (* | expr MINUS expr         { $1 - $3 } *)
  (* | expr MUL expr           { $1 * $3 } *)
  (* | expr DIV expr           { $1 / $3 } *)
  (* | MINUS expr %prec UMINUS { - $2 } *)
