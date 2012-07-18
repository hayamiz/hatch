(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
open Tree
exception Eof

let unquote_str str =
  (Str.global_replace
	 (Str.regexp_string "\\t") "\t"
	 (Str.global_replace
		(Str.regexp_string "\\n") "\n"
		(Str.global_replace
		   (Str.regexp_string "\\\"") "\""
		   (String.sub str 1 ((String.length str) - 2)))))

(* TODO: maintainance of line number and offset *)

}
rule token = parse
    [' ' '\t']     { token lexbuf }     (* skip blanks *)
  | '#' [^ '\n']*  { token lexbuf }		(* comment *)
  | "undefined"    { UNDEF noloc }
  | "lambda"       { LAMBDA noloc }
  | "bind"         { BIND noloc }
  | "->"           { RARROW noloc }
  | "if"           { IF noloc }
  | "elseif"       { ELSEIF noloc }
  | "else"         { ELSE noloc }
  | "true"         { TRUE noloc }
  | "false"        { FALSE noloc }
  | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9']*
                   { IDENT (Lexing.lexeme lexbuf, noloc) }
  | ['0'-'9']+ '.' ['0'-'9']* (['e' 'E'] ['+' '-']? ['0'-'9']+)?
                   { FLOAT(float_of_string(Lexing.lexeme lexbuf), noloc) }
  | ['0'-'9']+     { INT(int_of_string(Lexing.lexeme lexbuf), noloc) }
  | '"' ("\\\""|[^ '"'])* '"'        { STRING(unquote_str (Lexing.lexeme lexbuf), noloc) }
  | '('            { LPAREN noloc }
  | ')'            { RPAREN noloc }
  | '{'            { LBRACE noloc }
  | '}'            { RBRACE noloc }
  | '['            { LBRAKET noloc }
  | ']'            { RBRAKET noloc }
  | ','            { COMMA noloc }
  | ';'            { SEMICOLON noloc }
  | '.'            { DOT  noloc }
  | '+'            { PLUS noloc }
  | '-'            { MINUS noloc }
  | '*'            { MUL noloc }
  | '/'            { DIV noloc }
  | "<="           { LEQ noloc }
  | ">="           { GEQ noloc }
  | '<'            { LE noloc }
  | '>'            { GE noloc }
  | '='            { EQ noloc }
  | eof            { EOF noloc }
