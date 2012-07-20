
open Parser
open Syntax

let quote_string str =
  (Str.global_replace
	 (Str.regexp "\t") "\\t"
	 (Str.global_replace
		(Str.regexp "\n") "\\n"
		(Str.global_replace (Str.regexp "\"") "\\\"" str)))

let string_of_token tok =
  match tok with
	  INT (x) -> "INT " ^ (string_of_int x)
	| FLOAT (x) -> "FLOAT " ^ (string_of_float x)
	| STRING (str) -> "STRING \"" ^ (quote_string str) ^ "\""
	| TRUE  -> "TRUE"
	| FALSE -> "FALSE"
	| UNDEF -> "UNDEF"
	| PLUS  -> "PLUS"
	| MINUS -> "MINUS"
	| MUL  -> "MUL"
	| DIV  -> "DIV"
	| LAND -> "LAND"
	| LOR  -> "LOR"
	| LNOT -> "LNOT"
	| LPAREN  -> "LPAREN"
	| RPAREN  -> "RPAREN"
	| LBRAKET -> "LBRAKET"
	| RBRAKET -> "RBRAKET"
	| LBRACE  -> "LBRACE"
	| RBRACE  -> "RBRACE"
	| COMMA   -> "COMMA"
	| SEMICOLON -> "SEMICOLON"
	| DOT -> "DOT"
	| EQ -> "EQ"
	| LE -> "LE"
	| GE -> "GE"
	| LT -> "LT"
	| GT -> "GT"
	| IDENT (id) -> "IDENT \"" ^ id ^ "\""
	| LAMBDA _   -> "LAMBDA"
	| IF _       -> "IF"
	| ELSE _     -> "ELSE"
	| BIND _     -> "BIND"
	| LET _      -> "LET"
	| RARROW _   -> "RARROW"
	| IN _       -> "IN"
	| EOF  -> "EOF"
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
	  ExpLiteral (lit) -> is ^ "Literal " ^ (string_of_literal lit)
	| ExpLambda (params, body_expr) ->
		is ^ "Closure (" ^ (String.concat "," params) ^ ") {\n" ^
		  (string_of_expr body_expr ~indent:(indent+2)) ^ "\n" ^
		  is ^ "}"
	| ExpApply (f, args) ->
		"Apply " ^ (string_of_expr f) ^ "(" ^ (String.concat "," (List.map string_of_expr args)) ^ ")"
	| ExpBind ((id, v)) ->
		is ^ "Bind: " ^ id ^ " ->\n" ^
		  (string_of_expr v ~indent:(indent+4)) ^ "\n"
	| ExpLet (id, v, body) ->
		is ^ "Let" ^ "\n" ^
		  is ^ "  " ^ id ^ " ->\n" ^
		  (string_of_expr v ~indent:(indent+4)) ^ "\n" ^
		  (string_of_expr body ~indent:(indent+2))
	| ExpPrefix (op, e) ->
		is ^ "Prefix: " ^ (string_of_prefix_oper op) ^ "\n" ^
		  (string_of_expr e ~indent:(indent+2))
	| ExpInfix (op, e1, e2) ->
		is ^ "Infix: " ^ (string_of_infix_oper op) ^ "\n" ^
		  (string_of_expr e1 ~indent:(indent+2)) ^ "\n" ^
		  (string_of_expr e2 ~indent:(indent+2))
	| ExpSeq (es) ->
		is ^ "Seq " ^ "\n" ^
		  (String.concat "\n" (List.map (fun e -> string_of_expr e ~indent:(indent+2)) es ))
	| ExpIf  (cond, if_clause, ExpNop) ->
		is ^ "If" ^ "\n" ^
		  (string_of_expr cond ~indent:(indent+2)) ^ "\n" ^
		  (string_of_expr if_clause ~indent:(indent+2))
	| ExpIf  (cond, if_clause, else_clause) ->
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
	  (ExpLiteral (lit1), ExpLiteral (lit2)) ->
		egg_literal_equal lit1 lit2
	| _ ->
		raise (Failure "egg_expr_equal not implemented")
