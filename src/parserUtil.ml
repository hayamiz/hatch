
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
	| LAND noloc -> "LAND"
	| LOR  noloc -> "LOR"
	| LNOT noloc -> "LNOT"
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
	| LAMBDA _ -> "LAMBDA"
	| IF _ -> "IF"
	| ELSEIF _ -> "ELSEIF"
	| ELSE _ -> "ELSE"
	| BIND _ -> "BIND"
	| RARROW _ -> "RARROW"
	| EOF noloc -> "EOF"
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

let string_of_literal lit =
  match lit with
	  LitInt (x) -> "Int " ^ (string_of_int x)
	| LitFloat (x) -> "Float " ^ (string_of_float x)
	| LitString (x) -> "String \"" ^ (quote_string x) ^ "\""
	| LitIdent (x) -> "Ident " ^ x
	| LitBool (true) -> "True"
	| LitBool (false) -> "False"
	| LitUndef -> "Undef"
;;

let string_of_infix_oper op =
  match op with
	  InfixPlus -> "+"
	| InfixMinus -> "-"
	| InfixMul -> "*"
	| InfixDiv -> "/"

let rec string_of_expr e =
  match e with
	  ExpLiteral (lit, loc) -> "Literal (" ^ (string_of_literal lit) ^ ")"
	| ExpClosure (params, body_expr, _) ->
		"Closure (" ^ (String.concat "," params) ^ ") { " ^ (string_of_expr body_expr) ^ " }"
	| ExpApply (f, args, _) ->
		"Apply " ^ (string_of_expr f) ^ "(" ^ (String.concat "," (List.map string_of_expr args)) ^ ")"
	| ExpBind (id, e, _) -> "Bind " ^ id ^ " -> " ^ (string_of_expr e)
	| ExpInfix (op, e1, e2, _) -> "(" ^ (string_of_expr e1) ^ (string_of_infix_oper op) ^ (string_of_expr e2)  ^ ")"
	| ExpSeq (es, _) ->
		"Seq (" ^ (String.concat "; " (List.map string_of_expr es)) ^ ")"
	| _ -> raise (Failure "string_of_expr not implemented")
;;


let egg_literal_equal lit1 lit2 =
  match (lit1, lit2) with 
	  (LitIdent x, LitIdent y) -> (String.compare x y) == 0
	| (LitInt x, LitInt y) -> x == y
	| (LitFloat x, LitFloat y) -> x == y
	| (LitString x, LitString y) -> (String.compare x y) == 0
	| (LitBool x, LitBool y) -> x == y
	| (LitUndef, LitUndef) -> false
	| _ -> false

let egg_expr_equal e1 e2 =
  match (e1, e2) with
	  (ExpLiteral (lit1, _), ExpLiteral (lit2, _)) ->
		egg_literal_equal lit1 lit2
	| _ ->
		raise (Failure "egg_expr_equal not implemented")
