
open Parser

let string_of_token tok =
  match tok with
	  INT x -> "INT " ^ (string_of_int x)
	| PLUS -> "PLUS"
	| MINUS -> "MINUS"
	| MUL -> "MUL"
	| DIV -> "DIV"
	| LPAREN -> "LPAREN"
	| RPAREN -> "RPAREN"
	| LBRAKET -> "LBRAKET"
	| RBRAKET -> "RBRAKET"
	| LBRACE -> "LBRACE"
	| RBRACE -> "RBRACE"
	| EOL -> "EOL"
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
