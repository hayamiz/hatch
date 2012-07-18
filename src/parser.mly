/* File parser.mly */

%{

  open Tree

  let make_lit_id (str, loc) =
	ExpLiteral (LitIdent (str), loc)

  let make_lit_int (int, loc) =
	ExpLiteral (LitInt (int), loc)

  let make_lit_float (flt, loc) =
	ExpLiteral (LitFloat (flt), loc)

  let make_lit_str (str, loc) =
	ExpLiteral (LitString (str), loc)

  let make_lit_bool b loc =
	ExpLiteral (LitBool b, loc)

  let make_comp_expr e se =
	match se with
		(ExpSeq (es, loc)) ->
		  begin
			match es with
			  (e1 :: erest) -> 
				ExpSeq ((e :: es), loc)
			  | [] -> e
		  end
	  | _ -> ExpSeq ([e; se], noloc)
  ;;

%}

%token <int * Tree.location> INT
%token <float * Tree.location> FLOAT
%token <string * Tree.location> STRING
%token <string * Tree.location> IDENT
%token <Tree.location> UNDEF
%token <Tree.location> TRUE FALSE
%token <Tree.location> LPAREN RPAREN
%token <Tree.location> LBRAKET RBRAKET
%token <Tree.location> LBRACE RBRACE
%token <Tree.location> COMMA SEMICOLON DOT
%token <Tree.location> PLUS MINUS MUL DIV
%token <Tree.location> EQ LEQ GEQ LE GE
%token <Tree.location> LAMBDA
%token <Tree.location> IF ELSEIF ELSE
%token <Tree.location> BIND RARROW
%token <Tree.location> EOF
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
%start main             /* the entry point */
%type <Tree.egg_expr> main
%%
main:
    compound_expr EOF                { $1 }
;
literal:
    IDENT  { make_lit_id $1 }
  | INT    { make_lit_int $1 }
  | FLOAT  { make_lit_float $1 }
  | STRING { make_lit_str $1 }
  | TRUE   { make_lit_bool true $1 }
  | FALSE  { make_lit_bool false $1 }
  | UNDEF  { ExpLiteral (LitUndef, $1) }

expr:
    primary_expr      { $1 }
  | closure_expr      { $1 }
  | apply_expr        { $1 }
  | bind_expr         { $1 }
  | block_expr        { $1 }

primary_expr:
    literal             { $1 }
  | LPAREN expr RPAREN  { $2 }

closure_expr:
    LAMBDA LPAREN param_list RPAREN expr  { ExpClosure ($3, $5, noloc)}

argument_list:
    expr COMMA argument_list  { $1 :: $3 }
  | expr                      { [$1] }
  |                           { [] }
apply_expr:
    primary_expr LPAREN argument_list RPAREN   { ExpApply ($1, $3, noloc) }

bind_expr:
    BIND IDENT RARROW expr   { let (id,_) = $2 in ExpBind (id, $4, noloc) }

param_list:
    IDENT COMMA param_list   { let (id,_) = $1 in id :: $3 }
  | IDENT                    { let (id,_) = $1 in [id] }
  |                          { [] }

block_expr:
    LBRACE compound_expr RBRACE   { $2 }
compound_expr:
    expr SEMICOLON compound_expr  { make_comp_expr $1 $3 }
  | expr                          { make_comp_expr $1 (ExpSeq ([], noloc))}
  |                               { ExpSeq ([], noloc) }

;

%%

  (* | LPAREN expr RPAREN      { $2 } *)
  (* | expr PLUS expr          { $1 + $3 } *)
  (* | expr MINUS expr         { $1 - $3 } *)
  (* | expr MUL expr           { $1 * $3 } *)
  (* | expr DIV expr           { $1 / $3 } *)
  (* | MINUS expr %prec UMINUS { - $2 } *)
