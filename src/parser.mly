/* File parser.mly */

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
%token <Tree.location> EOL
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
%start main             /* the entry point */
%type <int * Tree.location> main
%%
main:
    expr EOL                { $1 }
;
literal:
    INT  { $1 }

expr:
    literal                 { $1 }
;

%%

  (* | LPAREN expr RPAREN      { $2 } *)
  (* | expr PLUS expr          { $1 + $3 } *)
  (* | expr MINUS expr         { $1 - $3 } *)
  (* | expr MUL expr           { $1 * $3 } *)
  (* | expr DIV expr           { $1 / $3 } *)
  (* | MINUS expr %prec UMINUS { - $2 } *)
