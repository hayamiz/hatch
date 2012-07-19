
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
  assert_equal ~printer:id "LAND" (ParserUtil.string_of_token (LAND noloc));
  assert_equal ~printer:id "LOR" (ParserUtil.string_of_token (LOR noloc));
  assert_equal ~printer:id "LNOT" (ParserUtil.string_of_token (LNOT noloc));
  assert_equal ~printer:id "LET" (ParserUtil.string_of_token (LET noloc));
  assert_equal ~printer:id "IN" (ParserUtil.string_of_token (IN noloc));
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

let test_lex_logical_op _ =
  assert_eq_tokens [LAND noloc] (tokens_from_string "&&");
  assert_eq_tokens [LOR noloc] (tokens_from_string "||");
  assert_eq_tokens [LNOT noloc] (tokens_from_string "!");
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
  assert_eq_tokens [INT (1, noloc); EQ noloc; INT (2, noloc)] (tokens_from_string "1=2");
  assert_eq_tokens [INT (1, noloc); EQ noloc; INT (2, noloc)] (tokens_from_string "1==2");
  assert_eq_tokens [INT (1, noloc); LT noloc; INT (2, noloc)] (tokens_from_string "1<2");
  assert_eq_tokens [INT (1, noloc); GT noloc; INT (2, noloc)] (tokens_from_string "1 > 2");
  assert_eq_tokens [INT (1, noloc); LE noloc; INT (2, noloc)] (tokens_from_string "1 <= 2");
  assert_eq_tokens [INT (1, noloc); GE noloc; INT (2, noloc)] (tokens_from_string "1 >= 2");
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
  "lex_logical_op" >:: test_lex_logical_op;
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

let assert_eq_egg_expr expected exp_str =
  try
	let tree = parse_string exp_str
  in
	assert_equal ~printer:id ~msg:("parsing: " ^ exp_str)
	  (string_of_expr expected)
	  (string_of_expr tree)
  with Parsing.Parse_error ->
	print_string ("Parse error: " ^ exp_str ^ "\n")

let test_parser_lit_id _ =
  assert_eq_egg_expr
	(ExpLiteral (LitIdent "hoge", noloc))
	("hoge");
  ()

let test_parser_lit_int _ =
  assert_eq_egg_expr
	(ExpLiteral (LitInt 1, noloc))
	("1");
  ()

let test_parser_lit_float _ =
  assert_eq_egg_expr
	(ExpLiteral (LitFloat 2.3, noloc))
	("2.3");

  ()

let test_parser_lit_string _ =
  assert_eq_egg_expr
	(ExpLiteral (LitString "hoge", noloc))
	("\"hoge\"");

  assert_eq_egg_expr
	(ExpLiteral (LitString "ho\"ge", noloc))
	("\"ho\\\"ge\"");

  ()

let test_parser_lit_bool _ =
  assert_eq_egg_expr
	(ExpLiteral (LitBool true, noloc))
	("true");

  assert_eq_egg_expr
	(ExpLiteral (LitBool false, noloc))
	("false");

  ()

let test_parser_lit_undef _ =
  assert_eq_egg_expr
	(ExpLiteral (LitUndef, noloc))
	("undefined");

  ()

let test_parser_primary_expr _ =
  assert_eq_egg_expr
	(ExpLiteral (LitInt 1, noloc))
	("(1)");

  ()

let test_parser_closure_expr _ =
  assert_eq_egg_expr
	(ExpClosure (["a"; "b"],
				 ExpSeq ([ExpLiteral (LitIdent "a", noloc);
						  ExpLiteral (LitIdent "b", noloc)], noloc), noloc))
	("lambda (a, b) { a; b }");

  assert_eq_egg_expr
	(ExpClosure (["a"], ExpLiteral (LitIdent "a", noloc), noloc))
	("lambda (a) { a }");
  ()

let test_parser_apply_expr _ =
  assert_eq_egg_expr
	(ExpApply (ExpLiteral (LitIdent "a", noloc),
			   [],
			   noloc))
	("a()");

  assert_eq_egg_expr
	(ExpApply (ExpLiteral (LitIdent "a", noloc),
			   [ExpLiteral (LitInt 1, noloc); ExpLiteral (LitInt 3, noloc); ExpLiteral (LitInt 4, noloc)],
			   noloc))
	("a(1,3,4)");

  ()

let test_parser_bind_expr _ =
  assert_eq_tokens
	[BIND (noloc); IDENT ("foo", noloc); RARROW noloc; INT (1, noloc)]
	(tokens_from_string "bind foo -> 1");

  assert_eq_egg_expr
	(ExpBind ([("foo", ExpLiteral (LitInt 1, noloc))],
			  noloc))
	("bind foo -> 1");

  assert_eq_egg_expr
	(ExpBind ([("foo", ExpLiteral (LitInt 1, noloc));
			   ("bar", ExpLiteral (LitInt 2, noloc))],
			  noloc))
	("bind foo -> 1, bar -> 2");

  assert_eq_egg_expr
	(ExpBind ([("foo",
				ExpClosure (["a"; "b"; "c"],
							ExpSeq ([ExpLiteral (LitIdent "a", noloc);
									 ExpLiteral (LitIdent "b", noloc);
									 ExpLiteral (LitIdent "c", noloc)], noloc),
							noloc))],
			  noloc))
	("bind foo -> lambda (a,b,c) { a; b; c }");

  ()

let test_parser_let_expr _ =
  assert_eq_tokens
	[LET (noloc); IDENT ("foo", noloc); RARROW noloc; INT (1, noloc); IN (noloc); INT (2, noloc)]
	(tokens_from_string "let foo -> 1 in 2");

  assert_eq_egg_expr
	(ExpLet ([("foo", ExpLiteral (LitString "bar", noloc))],
			 ExpLiteral (LitIdent "c", noloc),
			 noloc))
	("let foo -> \"bar\" in c");

  assert_eq_egg_expr
	(ExpLet ([("foo", ExpLiteral (LitString "bar", noloc));
			  ("hoge", ExpLiteral (LitInt 1, noloc))],
			 ExpLiteral (LitIdent "c", noloc),
			 noloc))
	("let foo -> \"bar\", hoge -> 1 in c");

  assert_eq_egg_expr
	(ExpLet ([("foo", ExpLiteral (LitString "bar", noloc));
			  ("hoge", ExpLiteral (LitInt 1, noloc))],
			 ExpLiteral (LitIdent "c", noloc),
			 noloc))
	("let foo -> \"bar\", hoge -> 1 { c }");

  ()


let test_parser_prefix_expr _ =
  assert_eq_egg_expr
	(ExpPrefix (PrefixPlus, ExpLiteral (LitInt 1, noloc), noloc))
	("+1");

  assert_eq_egg_expr
	(ExpPrefix (PrefixMinus, ExpLiteral (LitInt 2, noloc), noloc))
	("-2");

  assert_eq_egg_expr
	(ExpPrefix (PrefixLnot, ExpLiteral (LitBool true, noloc), noloc))
	("! true");

  assert_eq_egg_expr
	(ExpPrefix (PrefixPlus, ExpLiteral (LitInt 1, noloc), noloc))
	("+ (1)");

  ()

let test_parser_infix_expr _ =
  assert_eq_egg_expr
	(ExpInfix (InfixPlus, ExpLiteral (LitInt 1, noloc), ExpLiteral (LitInt 2, noloc), noloc))
	("1 + 2");

  assert_eq_egg_expr
	(ExpInfix (InfixMinus, ExpLiteral (LitInt 1, noloc), ExpLiteral (LitInt 2, noloc), noloc))
	("1 - 2");

  assert_eq_egg_expr
	(ExpInfix (InfixMul, ExpLiteral (LitInt 3, noloc), ExpLiteral (LitInt 2, noloc), noloc))
	("3 * 2");
  
  assert_eq_egg_expr
	(ExpInfix (InfixDiv, ExpLiteral (LitInt 3, noloc), ExpLiteral (LitFloat 2.0, noloc), noloc))
	("3 / 2.0");
  
  assert_eq_egg_expr
	(ExpInfix (InfixMinus, (ExpInfix (InfixPlus, ExpLiteral (LitInt 1, noloc), ExpLiteral (LitInt 2, noloc), noloc)), ExpLiteral (LitInt 3, noloc), noloc))
	("1 + 2 - 3");

  assert_eq_egg_expr
	(ExpInfix (InfixMinus,
			   (ExpInfix (InfixMinus,
						  ExpLiteral (LitInt 1, noloc),
						  ExpLiteral (LitInt 2, noloc), noloc)),
			   ExpLiteral (LitInt 3, noloc), noloc))
	("1 - 2 - 3");

  assert_eq_egg_expr
	(ExpInfix (InfixMinus,
		  ExpInfix (InfixPlus,
						  ExpLiteral (LitInt 1, noloc),
						  ExpLiteral (LitInt 2, noloc), noloc),
		  ExpInfix (InfixMul,
					ExpInfix (InfixDiv,
							  ExpLiteral (LitInt 3, noloc),
							  ExpLiteral (LitInt 4, noloc), noloc),
					ExpInfix (InfixPlus,
							  ExpLiteral (LitInt 5, noloc),
							  ExpLiteral (LitInt 6, noloc), noloc),
					noloc),
		  noloc))
	("1 + 2 - 3 / 4 * (5 + 6)");

  assert_eq_egg_expr
	(ExpInfix (InfixPlus,
			   ExpLiteral (LitInt 1, noloc),
			   (parse_string "foo(2,3)"), noloc))
	("1 + foo(2,3)");

  ()

let test_parser_infix_expr_mul_div _ =
  assert_eq_egg_expr
	(ExpInfix (InfixMul,
			   ExpLiteral (LitInt 1, noloc),
			   ExpLiteral (LitInt 2, noloc), noloc))
	("1 * 2");

  assert_eq_egg_expr
	(ExpInfix (InfixMinus,
			   (ExpInfix (InfixMinus,
						  ExpLiteral (LitInt 1, noloc),
						  (ExpInfix (InfixMul,
									 ExpLiteral (LitInt 2, noloc),
									 ExpLiteral (LitInt 3, noloc), noloc)),
						  noloc)),
			   ExpLiteral (LitInt 4, noloc),
			   noloc))
	("1 - 2 * 3 - 4");

  assert_eq_egg_expr
	(ExpInfix (InfixMinus, (ExpInfix (InfixPlus, ExpLiteral (LitInt 1, noloc), ExpLiteral (LitInt 2, noloc), noloc)), ExpLiteral (LitInt 3, noloc), noloc))
	("1 + 2 - 3");
  ()

let test_parser_infix_comp_expr _ =
  assert_eq_egg_expr
	(ExpInfix (InfixEq,
			   ExpLiteral (LitInt 1, noloc),
			   ExpLiteral (LitInt 2, noloc), noloc))
	("1 = 2");

  assert_eq_egg_expr
	(ExpInfix (InfixEq,
			   ExpLiteral (LitInt 1, noloc),
			   ExpLiteral (LitInt 2, noloc), noloc))
	("1 == 2");

  assert_eq_egg_expr
	(ExpInfix (InfixLt,
			   ExpLiteral (LitInt 1, noloc),
			   ExpLiteral (LitInt 2, noloc), noloc))
	("1 < 2");

  assert_eq_egg_expr
	(ExpInfix (InfixGt,
			   ExpLiteral (LitInt 1, noloc),
			   ExpLiteral (LitInt 2, noloc), noloc))
	("1 > 2");

  assert_eq_egg_expr
	(ExpInfix (InfixLe,
			   ExpLiteral (LitInt 1, noloc),
			   ExpLiteral (LitInt 2, noloc), noloc))
	("1 <= 2");

  assert_eq_egg_expr
	(ExpInfix (InfixGe,
			   ExpLiteral (LitInt 1, noloc),
			   ExpLiteral (LitInt 2, noloc), noloc))
	("1 >= 2");

  assert_eq_egg_expr
	(ExpInfix (InfixLt,
			   (ExpInfix (InfixLt,
						  ExpLiteral (LitInt 1, noloc),
						  ExpLiteral (LitInt 2, noloc), noloc)),
			   ExpLiteral (LitInt 3, noloc),
			   noloc))
	("1 < 2 < 3");

  ()

let test_parser_infix_comp_arith_expr _ =
  assert_eq_egg_expr
	(ExpInfix (InfixEq,
			   (parse_string "1+2"),
			   (parse_string "-2"), noloc))
	("1+2 = -2");

  assert_eq_egg_expr
	(ExpInfix (InfixLt,
			   (parse_string "1+2"),
			   (parse_string "-2"), noloc))
	("1+2 < -2");

  assert_eq_egg_expr
	(ExpInfix (InfixGt,
			   (parse_string "3+4"),
			   (parse_string "5*6"), noloc))
	("3+4 > 5*6");

  assert_eq_egg_expr
	(ExpInfix (InfixLe,
			   (parse_string "3+ -4"),
			   (parse_string "5/6"), noloc))
	("3+ -4 <= 5/6");

  ()

let test_parser_infix_logi_expr _ =
  assert_eq_egg_expr
	(ExpInfix (InfixLand,
			   ExpLiteral (LitBool true, noloc),
			   ExpLiteral (LitBool false, noloc), noloc))
	("true && false");

  assert_eq_egg_expr
	(ExpInfix (InfixLor,
			   ExpLiteral (LitBool true, noloc),
			   ExpLiteral (LitBool false, noloc), noloc))
	("true || false");

  assert_eq_egg_expr
	(ExpInfix (InfixLand,
			   (ExpInfix (InfixLand,
						  ExpLiteral (LitInt 1, noloc),
						  ExpLiteral (LitInt 2, noloc), noloc)),
			   ExpLiteral (LitBool false, noloc),
			   noloc))
	("1 && 2 && false");

  ()

let test_parser_infix_logi_comp_expr _ =
  assert_eq_egg_expr
	(ExpInfix (InfixLand,
			   (parse_string "1 < 2"),
			   (parse_string "2 > 3"), noloc))
	("1 < 2 && 2 > 3");

  assert_eq_egg_expr
	(ExpInfix (InfixLand,
			   (parse_string "1+2 < 3*4"),
			   (parse_string "5/6 > -7"), noloc))
	("1+2 < 3*4 && 5/6 > -7");

  ()

let test_parser_prefix_infix_expr _ =
  assert_eq_egg_expr
	(ExpInfix (InfixPlus,
			   ExpPrefix (PrefixPlus, ExpLiteral (LitInt 1, noloc), noloc),
			   ExpPrefix (PrefixMinus, ExpLiteral (LitInt 2, noloc), noloc),
			   noloc))
	("+1 + -2");

  assert_eq_egg_expr
	(ExpInfix (InfixPlus,
			   ExpLiteral (LitInt 3, noloc),
			   ExpInfix (InfixMul,
						 ExpPrefix (PrefixMinus, ExpLiteral (LitInt 4, noloc), noloc),
						 ExpPrefix (PrefixMinus, ExpLiteral (LitInt 2, noloc), noloc),
						 noloc),
			   noloc))
	("3 + -4 * -2");

  ()

let test_parser_compound_expr _ =
  assert_eq_egg_expr
	(ExpSeq ([ExpLiteral (LitIdent "a", noloc);
			  ExpLiteral (LitIdent "b", noloc);
			  ExpLiteral (LitInt   1, noloc)], noloc))
	("a;b; 1");

  ()

let test_parser_block_expr _ =
  assert_eq_egg_expr
	(ExpSeq ([ExpLiteral (LitIdent "a", noloc);
			  ExpLiteral (LitIdent "b", noloc);
			  ExpLiteral (LitInt   1, noloc)], noloc))
	("{ a;b; 1}");

  ()

let test_parser_if_expr _ =
  assert_eq_egg_expr
	(ExpIf (ExpLiteral (LitIdent "a", noloc),
			ExpLiteral (LitIdent "b", noloc),
			ExpNop,
			noloc))
	"if a { b }";

  assert_eq_egg_expr
	(ExpIf (ExpLiteral (LitIdent "a", noloc),
			ExpLiteral (LitIdent "b", noloc),
			ExpNop,
			noloc))
	"if a b";

  assert_eq_egg_expr
	(ExpIf (ExpLiteral (LitIdent "a", noloc),
			ExpSeq ([ExpLiteral (LitIdent "b", noloc);
					 ExpLiteral (LitIdent "c", noloc);],
					noloc),
			ExpNop,
			noloc))
	"if a { b; c }";

  assert_eq_egg_expr
	(ExpIf (ExpLiteral (LitIdent "a", noloc),
			ExpLiteral (LitIdent "b", noloc),
			ExpLiteral (LitIdent "c", noloc),
			noloc))
	"if a { b } else { c }";

  assert_eq_egg_expr
	(ExpIf (ExpLiteral (LitIdent "a", noloc),
			ExpLiteral (LitIdent "b", noloc),
			ExpIf (ExpLiteral (LitIdent "c", noloc),
				   ExpLiteral (LitIdent "d", noloc),
				   ExpLiteral (LitIdent "f", noloc),
				   noloc),
			noloc))
	"if a { b } else if c { d } else f";

  assert_eq_egg_expr
	(ExpIf (parse_string "1+2 == 3",
			ExpLiteral (LitIdent "b", noloc),
			ExpNop,
			noloc))
	"if 1+2 == 3 { b }";

  assert_eq_egg_expr
	(ExpIf (parse_string "1+2 == 3",
			ExpLiteral (LitIdent "b", noloc),
			ExpIf (ExpLiteral (LitIdent "c", noloc),
				   ExpLiteral (LitIdent "d", noloc),
				   ExpLiteral (LitIdent "f", noloc),
				   noloc),
			noloc))
	"if 1+2 == 3 { b } else if c { d } else f";

  assert_eq_egg_expr
	(ExpIf (parse_string "1+2 == 3",
			ExpLiteral (LitIdent "b", noloc),
			ExpIf (ExpLiteral (LitIdent "c", noloc),
				   ExpLiteral (LitIdent "d", noloc),
				   ExpLiteral (LitIdent "f", noloc),
				   noloc),
			noloc))
	"if (1+2 == 3) { b } else if c { d } else f";

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
  "parser_let_expr" >:: test_parser_let_expr;
  "parser_prefix_expr" >:: test_parser_prefix_expr;
  "parser_infix_expr" >:: test_parser_infix_expr;
  "parser_infix_expr_mul_div" >:: test_parser_infix_expr_mul_div;
  "parser_prefix_infix_expr" >:: test_parser_prefix_infix_expr;
  "parser_infix_comp_expr" >:: test_parser_infix_comp_expr;
  "parser_infix_comp_arith_expr" >:: test_parser_infix_comp_arith_expr;
  "parser_infix_logi_expr" >:: test_parser_infix_logi_expr;
  "parser_infix_logi_comp_expr" >:: test_parser_infix_logi_comp_expr;
  "parser_compound_expr" >:: test_parser_compound_expr;
  "parser_block_expr" >:: test_parser_block_expr;
  "parser_if_expr" >:: test_parser_if_expr;
] end



(* test of semantic.ml *)
open Semantic

let assert_sem_type typ exp_str =
  assert_equal typ (Semantic.solve_type (parse_string exp_str))
	~msg:("Solving type of expr: " ^ exp_str)

let test_sem_solve_type _ =
  assert_sem_type TypUnsolved "foo";
  assert_sem_type TypUnsolved "1 + foo";
  assert_sem_type TypUnsolved "undefined";

  assert_sem_type TypInt "1";
  assert_sem_type TypInt "-1";
  assert_sem_type TypInt "+2";
  assert_sem_type TypInt "1 + 2";
  assert_sem_type TypInt "1 + 2 * 3 - 4";
  assert_sem_type TypInt "1 + 2 * (3 - 4) / 5";

  assert_sem_type TypFloat "1.0";
  assert_sem_type TypFloat "1.0e2";
  assert_sem_type TypFloat "1.0e2 + 2.0";
  assert_sem_type TypFloat "1.0e2 + 1";
  assert_sem_type TypFloat "3 * 1.0e2";
  assert_sem_type TypFloat "(3 * 1.0e2)";

  assert_sem_type TypString "\"hoge\"";
  assert_sem_type TypString "\"hoge\" + \"foo\"";
  assert_sem_type TypString "\"hoge\" * 3";

  assert_sem_type TypBool "true";
  assert_sem_type TypBool "false";
  assert_sem_type TypBool "! false";
  assert_sem_type TypBool "! 1";

  assert_sem_type TypInt "1 < 2";
  assert_sem_type TypInt "1 = 2";
  assert_sem_type TypInt "1 >= 2";

  assert_sem_type TypFloat "1.0 < 2.0";

  assert_sem_type TypBool "\"foo\" = \"bar\"";

  assert_sem_type TypClosure "lambda (a) { b }";

  ()


let _ = add_suites begin "Semantic" >::: [
  "sem_solve_type" >:: test_sem_solve_type;
] end


let _ =
  List.iter
	(fun suite ->
	   let _ = run_test_tt ~verbose:false suite
	   in ()
	)
	!suites
