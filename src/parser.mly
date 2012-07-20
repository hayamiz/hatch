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
%token <Tree.location> EQ LE GE LT GT
%token <Tree.location> LAND LOR LNOT
%token <Tree.location> LAMBDA
%token <Tree.location> IF ELSE
%token <Tree.location> BIND RARROW LET IN
%token <Tree.location> RETURN
%token <Tree.location> EOF
%left LAND LOR          /* 5th precedence */
%left EQ LE GE LT GT    /* 4th precedence */
%left PLUS MINUS        /* 3rd precedence */
%left MUL DIV           /* 2nd precedence */
%nonassoc UPLUS UMINUS  /* highest precedence */
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
  | let_expr          { $1 }
  | infix_expr        { $1 }
  | block_expr        { $1 }
  | if_expr           { $1 }
  | return_expr       { $1 }

primary_expr:
    literal             { $1 }
  | apply_expr          { $1 }
  | LPAREN expr RPAREN  { $2 }

closure_expr:
    LAMBDA LPAREN param_list RPAREN expr  { ExpClosure ($3, $5, noloc)}

argument_list:
    expr COMMA argument_list  { $1 :: $3 }
  | expr                      { [$1] }
  |                           { [] }
apply_expr:
    primary_expr LPAREN argument_list RPAREN   { ExpApply ($1, $3, noloc) }

assignments:
    IDENT RARROW expr COMMA assignments  { let (id, _) = $1 in (id, $3) :: $5 }
  | IDENT RARROW expr                    { let (id, _) = $1 in [(id, $3)] }

bind_expr:
    BIND assignments  { ExpBind ($2, noloc) }

let_expr:
    LET assignments IN expr  { ExpLet ($2, $4, noloc) }
  | LET assignments LBRACE compound_expr RBRACE
                             { ExpLet ($2, $4, noloc) }

prefix_expr:
    primary_expr                    { $1 }
  | PLUS  primary_expr %prec UPLUS  { ExpPrefix (PrefixPlus,  $2, noloc) }
  | MINUS primary_expr %prec UMINUS { ExpPrefix (PrefixMinus, $2, noloc) }
  | LNOT  primary_expr              { ExpPrefix (PrefixLnot,  $2, noloc) }

infix_expr:
    prefix_expr                    { $1 }
  | infix_expr PLUS  infix_expr  { ExpInfix (InfixPlus,  $1, $3, noloc) }
  | infix_expr MINUS infix_expr  { ExpInfix (InfixMinus, $1, $3, noloc) }
  | infix_expr MUL   infix_expr  { ExpInfix (InfixMul,   $1, $3, noloc) }
  | infix_expr DIV   infix_expr  { ExpInfix (InfixDiv,   $1, $3, noloc) }
  | infix_expr EQ    infix_expr  { ExpInfix (InfixEq,    $1, $3, noloc) }
  | infix_expr LE    infix_expr  { ExpInfix (InfixLe,    $1, $3, noloc) }
  | infix_expr GE    infix_expr  { ExpInfix (InfixGe,    $1, $3, noloc) }
  | infix_expr LT    infix_expr  { ExpInfix (InfixLt,    $1, $3, noloc) }
  | infix_expr GT    infix_expr  { ExpInfix (InfixGt,    $1, $3, noloc) }
  | infix_expr LAND  infix_expr  { ExpInfix (InfixLand,  $1, $3, noloc) }
  | infix_expr LOR   infix_expr  { ExpInfix (InfixLor,   $1, $3, noloc) }

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

cond_expr:
    primary_expr        { $1 }
  | infix_expr          { $1 }

if_expr:
    IF cond_expr expr ELSE expr  { ExpIf ($2, $3, $5, noloc) }
  | IF cond_expr expr            { ExpIf ($2, $3, ExpNop, noloc) }
;

return_expr:
    RETURN expr  { ExpReturn ($2, noloc) }
  | RETURN       { ExpReturn (ExpLiteral (LitUndef, noloc), noloc) }

%%

  (* | LPAREN expr RPAREN      { $2 } *)
  (* | expr PLUS expr          { $1 + $3 } *)
  (* | expr MINUS expr         { $1 - $3 } *)
  (* | expr MUL expr           { $1 * $3 } *)
  (* | expr DIV expr           { $1 / $3 } *)
  (* | MINUS expr %prec UMINUS { - $2 } *)
