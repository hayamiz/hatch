
open OUnit
open Tree
open Parser
open ParserUtil

let id x = x

let suites: OUnit.test list ref = ref []
let add_suites suite =
  suites := suite :: !suites

(* test of paserUtil.ml *)

let test_string_of_token _ =
  assert_equal ~printer:id "INT 1" (ParserUtil.string_of_token (INT (1, noloc)));
  assert_equal ~printer:id ("FLOAT " ^ (string_of_float 1.23)) (ParserUtil.string_of_token (FLOAT (1.23, noloc)));
  assert_equal ~printer:id "PLUS" (ParserUtil.string_of_token (PLUS noloc));
  assert_equal ~printer:id "MINUS" (ParserUtil.string_of_token (MINUS noloc));
  assert_equal ~printer:id "IDENT \"foo\"" (ParserUtil.string_of_token (IDENT ("foo", noloc)));
  assert_equal ~printer:id "STRING \"foo\"" (ParserUtil.string_of_token (STRING ("foo", noloc)));
  assert_equal ~printer:id "STRING \"f\\\"oo\"" (ParserUtil.string_of_token (STRING ("f\"oo", noloc)));
  assert_equal ~printer:id "STRING \"f\\noo\"" (ParserUtil.string_of_token (STRING ("f\noo", noloc)));
  assert_equal ~printer:id "STRING \"f\\too\"" (ParserUtil.string_of_token (STRING ("f\too", noloc)));
  assert_equal ~printer:id "TRUE" (ParserUtil.string_of_token (TRUE noloc));
  assert_equal ~printer:id "FALSE" (ParserUtil.string_of_token (FALSE noloc));
  assert_equal ~printer:id "UNDEF" (ParserUtil.string_of_token (UNDEF noloc));
  ()

let test_string_of_tokens _ =
  assert_equal ~printer:id "INT 1; PLUS; INT 2" (ParserUtil.string_of_tokens [INT (1, noloc); PLUS noloc; INT (2, noloc)]);
  assert_equal ~printer:id "INT 1; PLUS; INT 2; SEMICOLON"
	(ParserUtil.string_of_tokens [INT (1, noloc); PLUS noloc; INT (2, noloc); SEMICOLON noloc]);
  ()

let otherloc = {
  file = "(none)";
  line = noloc.line + 10;
  offset = noloc.offset + 20;
  byte = noloc.byte + 30;
}

let test_egg_expr_equal _ =
  assert_equal true
	(ParserUtil.egg_expr_equal
	   (ExpLiteral (LitIdent "foo", noloc))
	   (ExpLiteral (LitIdent "foo", otherloc)));

  assert_equal false
	(ParserUtil.egg_expr_equal
	   (ExpLiteral (LitIdent "foo", noloc))
	   (ExpLiteral (LitIdent "bar", otherloc)));

  assert_equal true
	(ParserUtil.egg_expr_equal
	   (ExpLiteral (LitString "foo", noloc))
	   (ExpLiteral (LitString "foo", otherloc)));

  assert_equal false
	(ParserUtil.egg_expr_equal
	   (ExpLiteral (LitString "foo", noloc))
	   (ExpLiteral (LitString "bar", otherloc)));

  assert_equal true
	(ParserUtil.egg_expr_equal
	   (ExpLiteral (LitInt 1, noloc))
	   (ExpLiteral (LitInt 1, otherloc)));

  assert_equal false
	(ParserUtil.egg_expr_equal
	   (ExpLiteral (LitInt 2, noloc))
	   (ExpLiteral (LitInt 3, otherloc)));

  assert_equal false
	(ParserUtil.egg_expr_equal
	   (ExpLiteral (LitUndef, noloc))
	   (ExpLiteral (LitUndef, otherloc)));
  ()

let _ = add_suites begin "ParserUtil" >::: [
  "string_of_token" >:: test_string_of_token;
  "string_of_tokens" >:: test_string_of_tokens;
  "egg_expr_equal" >:: test_egg_expr_equal;
] end


(* test of lexer.ml *)

let tokens_from_string str =
  let lexbuf = Lexing.from_string str
  in
  let rec read_toks toks lexbuf =
	match (Lexer.token lexbuf) with 
		EOF _ -> List.rev toks
	  | tok -> read_toks (tok :: toks) lexbuf
  in
	read_toks [] lexbuf

let assert_eq_tokens expected actual =
  assert_equal ~printer:string_of_tokens expected actual

let test_lex_float_lit _ =
  assert_eq_tokens [FLOAT (1.23, noloc)] (tokens_from_string "1.23");
  assert_eq_tokens [FLOAT (1.23e2 , noloc)] (tokens_from_string "1.23e2");
  assert_eq_tokens [FLOAT (2.34e+2, noloc)] (tokens_from_string "2.34e+2");
  assert_eq_tokens [FLOAT (3.45e-2, noloc)] (tokens_from_string "3.45e-2");
  ()

let test_lex_string_lit _ =
  assert_eq_tokens [STRING ("foo"  , noloc)] (tokens_from_string "\"foo\"");
  assert_eq_tokens [STRING ("f\"oo", noloc)] (tokens_from_string "\"f\\\"oo\"");
  assert_eq_tokens [STRING ("f\noo", noloc)] (tokens_from_string "\"f\\noo\"");
  assert_eq_tokens [STRING ("f\too", noloc)] (tokens_from_string "\"f\\too\"");
  ()

let test_lex_bool_lit _ =
  assert_eq_tokens [TRUE noloc] (tokens_from_string "true");
  assert_eq_tokens [FALSE noloc] (tokens_from_string "false");
  ()

let test_lex_undef_lit _ =
  assert_eq_tokens [UNDEF noloc] (tokens_from_string "undefined");
  ()

let test_simple_arith_exp _ =
  assert_eq_tokens [INT (1, noloc); PLUS  noloc; INT (1, noloc)] (tokens_from_string "1+1");
  assert_eq_tokens [INT (1, noloc); MINUS noloc; INT (2, noloc)] (tokens_from_string "1 - 2");
  assert_eq_tokens [INT (3, noloc); MUL   noloc; INT (4, noloc)] (tokens_from_string "3 * 4");
  assert_eq_tokens [INT (1, noloc); DIV   noloc; INT (2, noloc)] (tokens_from_string "1 / 2");
  assert_eq_tokens [FLOAT (1.0, noloc); DIV noloc; INT (2, noloc)] (tokens_from_string "1.0 / 2");
  ()

let test_arith_exp _ =
  assert_eq_tokens [INT (1, noloc); PLUS noloc; INT (2, noloc); MUL noloc; INT (3, noloc)]
	(tokens_from_string "1+2*3");
  assert_eq_tokens
	[LPAREN noloc; INT (1, noloc); PLUS noloc; INT (2, noloc); RPAREN noloc; MUL noloc; INT (3, noloc)]
	(tokens_from_string "(1+2)*3");
  ()

let test_comp_exp _ =
  assert_eq_tokens [INT (1, noloc); LE  noloc; INT (2, noloc)] (tokens_from_string "1<2");
  assert_eq_tokens [INT (1, noloc); GE  noloc; INT (2, noloc)] (tokens_from_string "1 > 2");
  assert_eq_tokens [INT (1, noloc); LEQ noloc; INT (2, noloc)] (tokens_from_string "1 <= 2");
  assert_eq_tokens [INT (1, noloc); GEQ noloc; INT (2, noloc)] (tokens_from_string "1 >= 2");
  ()

let test_delim_exp _ =
  assert_eq_tokens [COMMA noloc; SEMICOLON noloc; DOT noloc] (tokens_from_string ",;.");
  ()

let test_comment _ =
  assert_eq_tokens [INT (1, noloc)] (tokens_from_string "1 # foo");
  ()

let test_lex_id _ =
  assert_eq_tokens [IDENT ("foo"   , noloc)] (tokens_from_string "foo");
  assert_eq_tokens [IDENT ("fOo"   , noloc)] (tokens_from_string "fOo");
  assert_eq_tokens [IDENT ("foo123", noloc)] (tokens_from_string "foo123");
  assert_eq_tokens [INT (1, noloc); PLUS noloc; IDENT ("foo", noloc)] (tokens_from_string "1+foo");
  assert_eq_tokens [IDENT ("foo", noloc); INT (1, noloc)] (tokens_from_string "foo 1");
  ()

let test_lex_arith_with_loc _ =
  noloc

let _ = add_suites begin "Lexer" >::: [
  "lex_float_lit" >:: test_lex_float_lit;
  "lex_string_lit" >:: test_lex_string_lit;
  "lex_bool_lit" >:: test_lex_bool_lit;
  "ex_undef_lit" >:: test_lex_undef_lit;
  "simple arithmetic exp" >:: test_simple_arith_exp;
  "arithmetic exp" >:: test_arith_exp;
  "comp_exp" >:: test_comp_exp;
  "delim_exp" >:: test_delim_exp;
  "comment" >:: test_comment;
  "lex_id" >:: test_lex_id;
] end


(* test of parser.ml *)

let parse_string str =
  let lexbuf = Lexing.from_string str in
	Parser.main Lexer.token lexbuf

let assert_eq_egg_expr expected actual =
  assert_equal ~printer:id (string_of_expr expected) (string_of_expr actual)

let test_parser_lit_id _ =
  assert_eq_egg_expr
	(ExpLiteral (LitIdent "hoge", noloc))
	(parse_string "hoge");
  ()

let test_parser_lit_int _ =
  assert_eq_egg_expr
	(ExpLiteral (LitInt 1, noloc))
	(parse_string "1");
  ()

let test_parser_lit_float _ =
  assert_eq_egg_expr
	(ExpLiteral (LitFloat 2.3, noloc))
	(parse_string "2.3");

  ()

let test_parser_lit_string _ =
  assert_eq_egg_expr
	(ExpLiteral (LitString "hoge", noloc))
	(parse_string "\"hoge\"");

  assert_eq_egg_expr
	(ExpLiteral (LitString "ho\"ge", noloc))
	(parse_string "\"ho\\\"ge\"");

  ()

let test_parser_lit_bool _ =
  assert_eq_egg_expr
	(ExpLiteral (LitBool true, noloc))
	(parse_string "true");

  assert_eq_egg_expr
	(ExpLiteral (LitBool false, noloc))
	(parse_string "false");

  ()

let test_parser_lit_undef _ =
  assert_eq_egg_expr
	(ExpLiteral (LitUndef, noloc))
	(parse_string "undefined");

  ()

let test_parser_primary_expr _ =
  assert_eq_egg_expr
	(ExpLiteral (LitInt 1, noloc))
	(parse_string "(1)");

  ()

let test_parser_closure_expr _ =
  assert_eq_egg_expr
	(ExpClosure (["a"; "b"],
				 ExpSeq ([ExpLiteral (LitIdent "a", noloc);
						  ExpLiteral (LitIdent "b", noloc)], noloc), noloc))
	(parse_string "lambda (a, b) { a; b }");

  assert_eq_egg_expr
	(ExpClosure (["a"], ExpLiteral (LitIdent "a", noloc), noloc))
	(parse_string "lambda (a) { a }");
  ()

let test_parser_apply_expr _ =
  assert_eq_egg_expr
	(ExpApply (ExpLiteral (LitIdent "a", noloc),
			   [],
			   noloc))
	(parse_string "a()");

  assert_eq_egg_expr
	(ExpApply (ExpLiteral (LitIdent "a", noloc),
			   [ExpLiteral (LitInt 1, noloc); ExpLiteral (LitInt 3, noloc); ExpLiteral (LitInt 4, noloc)],
			   noloc))
	(parse_string "a(1,3,4)");

  ()

let test_parser_bind_expr _ =
  assert_eq_tokens
	[BIND (noloc); IDENT ("foo", noloc); RARROW noloc; INT (1, noloc)]
	(tokens_from_string "bind foo -> 1");

  assert_eq_egg_expr
	(ExpBind ("foo", ExpLiteral (LitInt 1, noloc), noloc))
	(parse_string "bind foo -> 1");

  assert_eq_egg_expr
	(ExpBind ("foo",
			  ExpClosure (["a"; "b"; "c"],
						 ExpSeq ([ExpLiteral (LitIdent "a", noloc);
								  ExpLiteral (LitIdent "b", noloc);
								  ExpLiteral (LitIdent "c", noloc)], noloc),
						 noloc),
			  noloc))
	(parse_string "bind foo -> lambda (a,b,c) { a; b; c }");

  ()

let test_parser_compound_expr _ =
  assert_eq_egg_expr
	(ExpSeq ([ExpLiteral (LitIdent "a", noloc);
			  ExpLiteral (LitIdent "b", noloc);
			  ExpLiteral (LitInt   1, noloc)], noloc))
	(parse_string "a;b; 1");

  ()

let test_parser_block_expr _ =
  assert_eq_egg_expr
	(ExpSeq ([ExpLiteral (LitIdent "a", noloc);
			  ExpLiteral (LitIdent "b", noloc);
			  ExpLiteral (LitInt   1, noloc)], noloc))
	(parse_string "{ a;b; 1}");

  ()


let _ = add_suites begin "Parser" >::: [
  "parser_lit_id" >:: test_parser_lit_id;
  "parser_lit_int" >:: test_parser_lit_int;
  "parser_lit_float" >:: test_parser_lit_float;
  "parser_lit_string" >:: test_parser_lit_string;
  "parser_lit_bool" >:: test_parser_lit_bool;
  "parser_lit_undef" >:: test_parser_lit_undef;
  "parser_primary_expr" >:: test_parser_primary_expr;
  "parser_closure_expr" >:: test_parser_closure_expr;
  "parser_apply_expr" >:: test_parser_apply_expr;
  "parser_bind_expr" >:: test_parser_bind_expr;
  "parser_compound_expr" >:: test_parser_compound_expr;
  "parser_block_expr" >:: test_parser_block_expr;
] end



let _ =
  List.iter
	(fun suite ->
	   let _ = run_test_tt ~verbose:false suite
	   in ()
	)
	!suites
