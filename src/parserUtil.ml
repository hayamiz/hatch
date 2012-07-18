
open Parser
open Tree

let quote_string str =
  (Str.global_replace
	 (Str.regexp "\t") "\\t"
	 (Str.global_replace
		(Str.regexp "\n") "\\n"
		(Str.global_replace (Str.regexp "\"") "\\\"" str)))

let string_of_token tok =
  match tok with
	  INT (x, noloc) -> "INT " ^ (string_of_int x)
	| FLOAT (x, noloc) -> "FLOAT " ^ (string_of_float x)
	| STRING (str, noloc) -> "STRING \"" ^ (quote_string str) ^ "\""
	| TRUE noloc -> "TRUE"
	| FALSE noloc -> "FALSE"
	| UNDEF noloc -> "UNDEF"
	| PLUS noloc -> "PLUS"
	| MINUS noloc -> "MINUS"
	| MUL noloc -> "MUL"
	| DIV noloc -> "DIV"
	| LPAREN noloc -> "LPAREN"
	| RPAREN noloc -> "RPAREN"
	| LBRAKET noloc -> "LBRAKET"
	| RBRAKET noloc -> "RBRAKET"
	| LBRACE noloc -> "LBRACE"
	| RBRACE noloc -> "RBRACE"
	| COMMA noloc -> "COMMA"
	| SEMICOLON noloc -> "SEMICOLON"
	| DOT noloc -> "DOT"
	| EQ noloc -> "EQ"
	| LEQ noloc -> "LEQ"
	| GEQ noloc -> "GEQ"
	| LE noloc -> "LE"
	| GE noloc -> "GE"
	| IDENT (id, noloc) -> "IDENT \"" ^ id ^ "\""
	| EOL noloc -> "EOL"
	(* | _ -> "UNKNOWN_TOKEN" *)
;;

let string_of_tokens toks =
  match (List.rev toks) with
	  tok :: rest ->
		List.fold_left (fun a b ->
						  (string_of_token b) ^ "; " ^ a)
		  (string_of_token tok) rest
	| [] -> ""
;;
