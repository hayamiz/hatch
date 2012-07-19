
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
	| TRUE noloc  -> "TRUE"
	| FALSE noloc -> "FALSE"
	| UNDEF noloc -> "UNDEF"
	| PLUS noloc  -> "PLUS"
	| MINUS noloc -> "MINUS"
	| MUL noloc  -> "MUL"
	| DIV noloc  -> "DIV"
	| LAND noloc -> "LAND"
	| LOR  noloc -> "LOR"
	| LNOT noloc -> "LNOT"
	| LPAREN noloc  -> "LPAREN"
	| RPAREN noloc  -> "RPAREN"
	| LBRAKET noloc -> "LBRAKET"
	| RBRAKET noloc -> "RBRAKET"
	| LBRACE noloc  -> "LBRACE"
	| RBRACE noloc  -> "RBRACE"
	| COMMA noloc   -> "COMMA"
	| SEMICOLON noloc -> "SEMICOLON"
	| DOT noloc -> "DOT"
	| EQ noloc -> "EQ"
	| LE noloc -> "LE"
	| GE noloc -> "GE"
	| LT noloc -> "LT"
	| GT noloc -> "GT"
	| IDENT (id, noloc) -> "IDENT \"" ^ id ^ "\""
	| LAMBDA _   -> "LAMBDA"
	| IF _       -> "IF"
	| ELSE _     -> "ELSE"
	| BIND _     -> "BIND"
	| LET _      -> "LET"
	| RARROW _   -> "RARROW"
	| IN _       -> "IN"
	| EOF noloc  -> "EOF"
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

let string_of_prefix_oper op =
  match op with
	  PrefixPlus  -> "+"
	| PrefixMinus -> "-"
	| PrefixLnot  -> "!"

let string_of_infix_oper op =
  match op with
	  InfixPlus  -> "+"
	| InfixMinus -> "-"
	| InfixMul   -> "*"
	| InfixDiv   -> "/"
	| InfixEq    -> "="
	| InfixLe    -> "<="
	| InfixGe    -> ">="
	| InfixLt    -> "<"
	| InfixGt    -> ">"
	| InfixLand  -> "&&"
	| InfixLor   -> "||"

let rec string_of_expr ?(indent = 0) e =
  let is = String.make indent ' '
  in
  match e with
	  ExpLiteral (lit, loc) -> is ^ "Literal " ^ (string_of_literal lit)
	| ExpClosure (params, body_expr, _) ->
		is ^ "Closure (" ^ (String.concat "," params) ^ ") {\n" ^
		  (string_of_expr body_expr ~indent:(indent+2)) ^ "\n" ^
		  is ^ "}"
	| ExpApply (f, args, _) ->
		"Apply " ^ (string_of_expr f) ^ "(" ^ (String.concat "," (List.map string_of_expr args)) ^ ")"
	| ExpBind (bindings, _) ->
		is ^ "Bind" ^ "\n" ^
		  (String.concat "\n"
			 (List.map (fun (id, e) ->
						  is ^ "  " ^ id ^ " ->\n" ^
							(string_of_expr e ~indent:(indent+4)))
				bindings))
	| ExpLet (bindings, body, _) ->
		is ^ "Let" ^ "\n" ^
		  (String.concat "\n"
			 (List.map (fun (id, e) ->
						  is ^ "  " ^ id ^ " ->\n" ^
							(string_of_expr e ~indent:(indent+4)))
				bindings)) ^ "\n" ^
		  (string_of_expr body ~indent:(indent+2))
	| ExpPrefix (op, e, _) ->
		is ^ "Prefix: " ^ (string_of_prefix_oper op) ^ "\n" ^
		  (string_of_expr e ~indent:(indent+2))
	| ExpInfix (op, e1, e2, _) ->
		is ^ "Infix: " ^ (string_of_infix_oper op) ^ "\n" ^
		  (string_of_expr e1 ~indent:(indent+2)) ^ "\n" ^
		  (string_of_expr e2 ~indent:(indent+2))
	| ExpSeq (es, _) ->
		is ^ "Seq " ^ "\n" ^
		  (String.concat "\n" (List.map (fun e -> string_of_expr e ~indent:(indent+2)) es ))
	| ExpIf  (cond, if_clause, ExpNop, _) ->
		is ^ "If" ^ "\n" ^
		  (string_of_expr cond ~indent:(indent+2)) ^ "\n" ^
		  (string_of_expr if_clause ~indent:(indent+2))
	| ExpIf  (cond, if_clause, else_clause, _) ->
		is ^ "If" ^ "\n" ^
		  (string_of_expr cond ~indent:(indent+2)) ^ "\n" ^
		  (string_of_expr if_clause ~indent:(indent+2)) ^ "\n" ^
		  (string_of_expr else_clause ~indent:(indent+2))
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
