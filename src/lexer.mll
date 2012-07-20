(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
open Syntax
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
  | "undefined"    { UNDEF }
  | "lambda"       { LAMBDA }
  | "bind"         { BIND }
  | "let"          { LET }
  | "in"           { IN }
  | "return"       { RETURN }
  | "->"           { RARROW }
  | "if"           { IF }
  | "else"         { ELSE }
  | "true"         { TRUE }
  | "false"        { FALSE }
  | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9']*
                   { IDENT (Lexing.lexeme lexbuf) }
  | ['0'-'9']+ '.' ['0'-'9']* (['e' 'E'] ['+' '-']? ['0'-'9']+)?
                   { FLOAT(float_of_string(Lexing.lexeme lexbuf)) }
  | ['0'-'9']+     { INT(int_of_string(Lexing.lexeme lexbuf)) }
  | '"' ("\\\""|[^ '"'])* '"'        { STRING(unquote_str (Lexing.lexeme lexbuf)) }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '{'            { LBRACE }
  | '}'            { RBRACE }
  | '['            { LBRAKET }
  | ']'            { RBRAKET }
  | ','            { COMMA }
  | ';'            { SEMICOLON }
  | '.'            { DOT  }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { MUL }
  | '/'            { DIV }
  | "<="           { LE }
  | ">="           { GE }
  | '<'            { LT }
  | '>'            { GT }
  | '=' '='?       { EQ }
  | "&&"           { LAND }
  | "||"           { LOR }
  | '!'            { LNOT }
  | eof            { EOF }
