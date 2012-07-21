
open OUnit
open Syntax
open Parser
open ParserUtil

let id x = x

let suites: OUnit.test list ref = ref []
let add_suites suite =
  suites := suite :: !suites

(* test of paserUtil.ml *)

let test_string_of_token _ =
  assert_equal ~printer:id "INT 1" (ParserUtil.string_of_token (INT (1)));
  assert_equal ~printer:id ("FLOAT " ^ (string_of_float 1.23)) (ParserUtil.string_of_token (FLOAT (1.23)));
  assert_equal ~printer:id "PLUS" (ParserUtil.string_of_token (PLUS));
  assert_equal ~printer:id "MINUS" (ParserUtil.string_of_token (MINUS));
  assert_equal ~printer:id "IDENT \"foo\"" (ParserUtil.string_of_token (IDENT ("foo")));
  assert_equal ~printer:id "STRING \"foo\"" (ParserUtil.string_of_token (STRING ("foo")));
  assert_equal ~printer:id "STRING \"f\\\"oo\"" (ParserUtil.string_of_token (STRING ("f\"oo")));
  assert_equal ~printer:id "STRING \"f\\noo\"" (ParserUtil.string_of_token (STRING ("f\noo")));
  assert_equal ~printer:id "STRING \"f\\too\"" (ParserUtil.string_of_token (STRING ("f\too")));
  assert_equal ~printer:id "TRUE" (ParserUtil.string_of_token (TRUE));
  assert_equal ~printer:id "FALSE" (ParserUtil.string_of_token (FALSE));
  assert_equal ~printer:id "UNDEF" (ParserUtil.string_of_token (UNDEF));
  assert_equal ~printer:id "LAND" (ParserUtil.string_of_token (LAND));
  assert_equal ~printer:id "LOR" (ParserUtil.string_of_token (LOR));
  assert_equal ~printer:id "LNOT" (ParserUtil.string_of_token (LNOT));
  assert_equal ~printer:id "LET" (ParserUtil.string_of_token (LET));
  assert_equal ~printer:id "IN" (ParserUtil.string_of_token (IN));
  ()

let test_string_of_tokens _ =
  assert_equal ~printer:id "INT 1; PLUS; INT 2" (ParserUtil.string_of_tokens [INT (1); PLUS; INT (2)]);
  assert_equal ~printer:id "INT 1; PLUS; INT 2; SEMICOLON"
	(ParserUtil.string_of_tokens [INT (1); PLUS; INT (2); SEMICOLON]);
  ()

let test_egg_expr_equal _ =
  assert_equal true
	(ParserUtil.egg_expr_equal
	   (ExpLiteral (LitIdent "foo"))
	   (ExpLiteral (LitIdent "foo")));

  assert_equal false
	(ParserUtil.egg_expr_equal
	   (ExpLiteral (LitIdent "foo"))
	   (ExpLiteral (LitIdent "bar")));

  assert_equal true
	(ParserUtil.egg_expr_equal
	   (ExpLiteral (LitString "foo"))
	   (ExpLiteral (LitString "foo")));

  assert_equal false
	(ParserUtil.egg_expr_equal
	   (ExpLiteral (LitString "foo"))
	   (ExpLiteral (LitString "bar")));

  assert_equal true
	(ParserUtil.egg_expr_equal
	   (ExpLiteral (LitInt 1))
	   (ExpLiteral (LitInt 1)));

  assert_equal false
	(ParserUtil.egg_expr_equal
	   (ExpLiteral (LitInt 2))
	   (ExpLiteral (LitInt 3)));

  assert_equal false
	(ParserUtil.egg_expr_equal
	   (ExpLiteral (LitUndef))
	   (ExpLiteral (LitUndef)));
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
  assert_eq_tokens [FLOAT (1.23)] (tokens_from_string "1.23");
  assert_eq_tokens [FLOAT (1.23e2 )] (tokens_from_string "1.23e2");
  assert_eq_tokens [FLOAT (2.34e+2)] (tokens_from_string "2.34e+2");
  assert_eq_tokens [FLOAT (3.45e-2)] (tokens_from_string "3.45e-2");
  ()

let test_lex_string_lit _ =
  assert_eq_tokens [STRING ("foo"  )] (tokens_from_string "\"foo\"");
  assert_eq_tokens [STRING ("f\"oo")] (tokens_from_string "\"f\\\"oo\"");
  assert_eq_tokens [STRING ("f\noo")] (tokens_from_string "\"f\\noo\"");
  assert_eq_tokens [STRING ("f\too")] (tokens_from_string "\"f\\too\"");
  ()

let test_lex_bool_lit _ =
  assert_eq_tokens [TRUE] (tokens_from_string "true");
  assert_eq_tokens [FALSE] (tokens_from_string "false");
  ()

let test_lex_undef_lit _ =
  assert_eq_tokens [UNDEF] (tokens_from_string "undefined");
  ()

let test_lex_logical_op _ =
  assert_eq_tokens [LAND] (tokens_from_string "&&");
  assert_eq_tokens [LOR] (tokens_from_string "||");
  assert_eq_tokens [LNOT] (tokens_from_string "!");
  ()

let test_simple_arith_exp _ =
  assert_eq_tokens [INT (1); PLUS ; INT (1)] (tokens_from_string "1+1");
  assert_eq_tokens [INT (1); MINUS; INT (2)] (tokens_from_string "1 - 2");
  assert_eq_tokens [INT (3); MUL  ; INT (4)] (tokens_from_string "3 * 4");
  assert_eq_tokens [INT (1); DIV  ; INT (2)] (tokens_from_string "1 / 2");
  assert_eq_tokens [FLOAT (1.0); DIV; INT (2)] (tokens_from_string "1.0 / 2");
  ()

let test_arith_exp _ =
  assert_eq_tokens [INT (1); PLUS; INT (2); MUL; INT (3)]
	(tokens_from_string "1+2*3");
  assert_eq_tokens
	[LPAREN; INT (1); PLUS; INT (2); RPAREN; MUL; INT (3)]
	(tokens_from_string "(1+2)*3");
  ()

let test_comp_exp _ =
  assert_eq_tokens [INT (1); EQ; INT (2)] (tokens_from_string "1=2");
  assert_eq_tokens [INT (1); EQ; INT (2)] (tokens_from_string "1==2");
  assert_eq_tokens [INT (1); LT; INT (2)] (tokens_from_string "1<2");
  assert_eq_tokens [INT (1); GT; INT (2)] (tokens_from_string "1 > 2");
  assert_eq_tokens [INT (1); LE; INT (2)] (tokens_from_string "1 <= 2");
  assert_eq_tokens [INT (1); GE; INT (2)] (tokens_from_string "1 >= 2");
  ()

let test_delim_exp _ =
  assert_eq_tokens [COMMA; SEMICOLON; DOT] (tokens_from_string ",;.");
  ()

let test_comment _ =
  assert_eq_tokens [INT (1)] (tokens_from_string "1 # foo");
  ()

let test_lex_id _ =
  assert_eq_tokens [IDENT ("foo"   )] (tokens_from_string "foo");
  assert_eq_tokens [IDENT ("fOo"   )] (tokens_from_string "fOo");
  assert_eq_tokens [IDENT ("foo123")] (tokens_from_string "foo123");
  assert_eq_tokens [INT (1); PLUS; IDENT ("foo")] (tokens_from_string "1+foo");
  assert_eq_tokens [IDENT ("foo"); INT (1)] (tokens_from_string "foo 1");
  ()


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
	(ExpLiteral (LitIdent "hoge"))
	("hoge");
  ()

let test_parser_lit_int _ =
  assert_eq_egg_expr
	(ExpLiteral (LitInt 1))
	("1");
  ()

let test_parser_lit_float _ =
  assert_eq_egg_expr
	(ExpLiteral (LitFloat 2.3))
	("2.3");

  ()

let test_parser_lit_string _ =
  assert_eq_egg_expr
	(ExpLiteral (LitString "hoge"))
	("\"hoge\"");

  assert_eq_egg_expr
	(ExpLiteral (LitString "ho\"ge"))
	("\"ho\\\"ge\"");

  ()

let test_parser_lit_bool _ =
  assert_eq_egg_expr
	(ExpLiteral (LitBool true))
	("true");

  assert_eq_egg_expr
	(ExpLiteral (LitBool false))
	("false");

  ()

let test_parser_lit_undef _ =
  assert_eq_egg_expr
	(ExpLiteral (LitUndef))
	("undefined");

  ()

let test_parser_primary_expr _ =
  assert_eq_egg_expr
	(ExpLiteral (LitInt 1))
	("(1)");

  ()

let test_parser_closure_expr _ =
  assert_eq_egg_expr
	(ExpLambda (["a"; "b"],
				 ExpSeq ([ExpLiteral (LitIdent "a");
						  ExpLiteral (LitIdent "b")])))
	("lambda (a, b) { a; b }");

  assert_eq_egg_expr
	(ExpLambda (["a"], ExpLiteral (LitIdent "a")))
	("lambda (a) { a }");
  ()

let test_parser_apply_expr _ =
  assert_eq_egg_expr
	(ExpApply (ExpLiteral (LitIdent "a"),
			   []))
	("a()");

  assert_eq_egg_expr
	(ExpApply (ExpLiteral (LitIdent "a"),
			   [ExpLiteral (LitInt 1); ExpLiteral (LitInt 3); ExpLiteral (LitInt 4)]))
	("a(1,3,4)");

  assert_eq_egg_expr
	(ExpApply (ExpLiteral (LitIdent "print"),
			   [ExpLiteral (LitInt 1);
				ExpLiteral (LitInt 2)]))
	("print 1, 2");

  ()

let test_parser_bind_expr _ =
  assert_eq_tokens
	[BIND; IDENT ("foo"); RARROW; INT (1)]
	(tokens_from_string "bind foo -> 1");

  assert_eq_egg_expr
	(ExpBind (("foo", ExpLiteral (LitInt 1))))
	("bind foo -> 1");

  assert_eq_egg_expr
	(ExpBind (("foo",
			   ExpLambda (["a"; "b"; "c"],
						  ExpSeq ([ExpLiteral (LitIdent "a");
								   ExpLiteral (LitIdent "b");
								   ExpLiteral (LitIdent "c")])))))
	("bind foo -> lambda (a,b,c) { a; b; c }");

  assert_eq_egg_expr
	(ExpSeq [(ExpBind (("foo", ExpLiteral (LitInt 1))));
			 (ExpBind (("bar", ExpLiteral (LitInt 2))))])
	("bind foo -> 1, bar -> 2");

  ()

let test_parser_let_expr _ =
  assert_eq_tokens
	[LET; IDENT ("foo"); RARROW; INT (1); IN; INT (2)]
	(tokens_from_string "let foo -> 1 in 2");

  assert_eq_egg_expr
	(ExpLet ("foo", ExpLiteral (LitString "bar"),
			 ExpLiteral (LitIdent "c")))
	("let foo -> \"bar\" in c");

  assert_eq_egg_expr
	(ExpLet ("foo", ExpLiteral (LitString "bar"),
			 ExpLet ("hoge", ExpLiteral (LitInt 1),
					 ExpLiteral (LitIdent "c"))))
	("let foo -> \"bar\", hoge -> 1 in c");

  assert_eq_egg_expr
	(ExpLet ("foo", ExpLiteral (LitString "bar"),
			 ExpLet ("hoge", ExpLiteral (LitInt 1),
					 ExpLiteral (LitIdent "c"))))
	("let foo -> \"bar\", hoge -> 1 { c }");

  ()


let test_parser_prefix_expr _ =
  assert_eq_egg_expr
	(ExpPrefix (PrefixPlus, ExpLiteral (LitInt 1)))
	("+1");

  assert_eq_egg_expr
	(ExpPrefix (PrefixMinus, ExpLiteral (LitInt 2)))
	("-2");

  assert_eq_egg_expr
	(ExpPrefix (PrefixLnot, ExpLiteral (LitBool true)))
	("! true");

  assert_eq_egg_expr
	(ExpPrefix (PrefixPlus, ExpLiteral (LitInt 1)))
	("+ (1)");

  ()

let test_parser_infix_expr _ =
  assert_eq_egg_expr
	(ExpInfix (InfixPlus, ExpLiteral (LitInt 1), ExpLiteral (LitInt 2)))
	("1 + 2");

  assert_eq_egg_expr
	(ExpInfix (InfixMinus, ExpLiteral (LitInt 1), ExpLiteral (LitInt 2)))
	("1 - 2");

  assert_eq_egg_expr
	(ExpInfix (InfixMul, ExpLiteral (LitInt 3), ExpLiteral (LitInt 2)))
	("3 * 2");
  
  assert_eq_egg_expr
	(ExpInfix (InfixDiv, ExpLiteral (LitInt 3), ExpLiteral (LitFloat 2.0)))
	("3 / 2.0");
  
  assert_eq_egg_expr
	(ExpInfix (InfixMinus, (ExpInfix (InfixPlus, ExpLiteral (LitInt 1), ExpLiteral (LitInt 2))), ExpLiteral (LitInt 3)))
	("1 + 2 - 3");

  assert_eq_egg_expr
	(ExpInfix (InfixMinus,
			   (ExpInfix (InfixMinus,
						  ExpLiteral (LitInt 1),
						  ExpLiteral (LitInt 2))),
			   ExpLiteral (LitInt 3)))
	("1 - 2 - 3");

  assert_eq_egg_expr
	(ExpInfix (InfixMinus,
		  ExpInfix (InfixPlus,
						  ExpLiteral (LitInt 1),
						  ExpLiteral (LitInt 2)),
		  ExpInfix (InfixMul,
					ExpInfix (InfixDiv,
							  ExpLiteral (LitInt 3),
							  ExpLiteral (LitInt 4)),
					ExpInfix (InfixPlus,
							  ExpLiteral (LitInt 5),
							  ExpLiteral (LitInt 6)))))
	("1 + 2 - 3 / 4 * (5 + 6)");

  assert_eq_egg_expr
	(ExpInfix (InfixPlus,
			   ExpLiteral (LitInt 1),
			   (parse_string "foo(2,3)")))
	("1 + foo(2,3)");

  ()

let test_parser_infix_expr_mul_div _ =
  assert_eq_egg_expr
	(ExpInfix (InfixMul,
			   ExpLiteral (LitInt 1),
			   ExpLiteral (LitInt 2)))
	("1 * 2");

  assert_eq_egg_expr
	(ExpInfix (InfixMinus,
			   (ExpInfix (InfixMinus,
						  ExpLiteral (LitInt 1),
						  (ExpInfix (InfixMul,
									 ExpLiteral (LitInt 2),
									 ExpLiteral (LitInt 3))))),
			   ExpLiteral (LitInt 4)))
	("1 - 2 * 3 - 4");

  assert_eq_egg_expr
	(ExpInfix (InfixMinus, (ExpInfix (InfixPlus, ExpLiteral (LitInt 1), ExpLiteral (LitInt 2))), ExpLiteral (LitInt 3)))
	("1 + 2 - 3");
  ()

let test_parser_infix_comp_expr _ =
  assert_eq_egg_expr
	(ExpInfix (InfixEq,
			   ExpLiteral (LitInt 1),
			   ExpLiteral (LitInt 2)))
	("1 = 2");

  assert_eq_egg_expr
	(ExpInfix (InfixEq,
			   ExpLiteral (LitInt 1),
			   ExpLiteral (LitInt 2)))
	("1 == 2");

  assert_eq_egg_expr
	(ExpInfix (InfixLt,
			   ExpLiteral (LitInt 1),
			   ExpLiteral (LitInt 2)))
	("1 < 2");

  assert_eq_egg_expr
	(ExpInfix (InfixGt,
			   ExpLiteral (LitInt 1),
			   ExpLiteral (LitInt 2)))
	("1 > 2");

  assert_eq_egg_expr
	(ExpInfix (InfixLe,
			   ExpLiteral (LitInt 1),
			   ExpLiteral (LitInt 2)))
	("1 <= 2");

  assert_eq_egg_expr
	(ExpInfix (InfixGe,
			   ExpLiteral (LitInt 1),
			   ExpLiteral (LitInt 2)))
	("1 >= 2");

  assert_eq_egg_expr
	(ExpInfix (InfixLt,
			   (ExpInfix (InfixLt,
						  ExpLiteral (LitInt 1),
						  ExpLiteral (LitInt 2))),
			   ExpLiteral (LitInt 3)))
	("1 < 2 < 3");

  ()

let test_parser_infix_comp_arith_expr _ =
  assert_eq_egg_expr
	(ExpInfix (InfixEq,
			   (parse_string "1+2"),
			   (parse_string "-2")))
	("1+2 = -2");

  assert_eq_egg_expr
	(ExpInfix (InfixLt,
			   (parse_string "1+2"),
			   (parse_string "-2")))
	("1+2 < -2");

  assert_eq_egg_expr
	(ExpInfix (InfixGt,
			   (parse_string "3+4"),
			   (parse_string "5*6")))
	("3+4 > 5*6");

  ()

let test_parser_infix_logi_expr _ =
  assert_eq_egg_expr
	(ExpInfix (InfixLand,
			   ExpLiteral (LitBool true),
			   ExpLiteral (LitBool false)))
	("true && false");

  assert_eq_egg_expr
	(ExpInfix (InfixLor,
			   ExpLiteral (LitBool true),
			   ExpLiteral (LitBool false)))
	("true || false");

  assert_eq_egg_expr
	(ExpInfix (InfixLand,
			   (ExpInfix (InfixLand,
						  ExpLiteral (LitInt 1),
						  ExpLiteral (LitInt 2))),
			   ExpLiteral (LitBool false)))
	("1 && 2 && false");

  ()

let test_parser_infix_logi_comp_expr _ =
  assert_eq_egg_expr
	(ExpInfix (InfixLand,
			   (parse_string "1 < 2"),
			   (parse_string "2 > 3")))
	("1 < 2 && 2 > 3");

  assert_eq_egg_expr
	(ExpInfix (InfixLand,
			   (parse_string "1+2 < 3*4"),
			   (parse_string "5/6 > -7")))
	("1+2 < 3*4 && 5/6 > -7");

  ()

let test_parser_prefix_infix_expr _ =
  assert_eq_egg_expr
	(ExpInfix (InfixPlus,
			   ExpPrefix (PrefixPlus, ExpLiteral (LitInt 1)),
			   ExpLiteral (LitInt 2)))
	("+1 + 2");

  assert_eq_egg_expr
	(ExpInfix (InfixPlus,
			   ExpPrefix (PrefixMinus, ExpLiteral (LitInt 3)),
			   ExpInfix (InfixMul,
						 ExpLiteral (LitInt 4),
						 ExpPrefix (PrefixMinus, ExpLiteral (LitInt 2)))))
	("-3 + 4 * -2");

  ()

let test_parser_compound_expr _ =
  assert_eq_egg_expr
	(ExpSeq ([ExpLiteral (LitIdent "a");
			  ExpLiteral (LitIdent "b");
			  ExpLiteral (LitInt   1)]))
	("a;b; 1");

  ()

let test_parser_block_expr _ =
  assert_eq_egg_expr
	(ExpSeq ([ExpLiteral (LitIdent "a");
			  ExpLiteral (LitIdent "b");
			  ExpLiteral (LitInt   1)]))
	("{ a;b; 1}");

  ()

let test_parser_if_expr _ =
  assert_eq_tokens
	[IF; IDENT ("a"); IDENT ("b"); ELSE; IDENT ("c")]
	(tokens_from_string "if a b else c");

  assert_eq_egg_expr
	(ExpIf (ExpLiteral (LitIdent "a"),
			ExpLiteral (LitIdent "b"),
			ExpLiteral (LitIdent "c")))
	"if a { b } else { c }";

  assert_eq_egg_expr
	(ExpIf (ExpLiteral (LitIdent "a"),
			ExpLiteral (LitIdent "b"),
			ExpIf (ExpLiteral (LitIdent "c"),
				   ExpLiteral (LitIdent "d"),
				   ExpLiteral (LitIdent "f"))))
	"if a { b } else if c { d } else f";

  assert_eq_egg_expr
	(ExpIf (parse_string "1+2 == 3",
			ExpLiteral (LitIdent "b"),
			ExpLiteral (LitIdent "d")))
	"if 1+2 == 3 { b } else { d }";

  assert_eq_egg_expr
	(ExpIf (parse_string "1+2 == 3",
			ExpLiteral (LitIdent "b"),
			ExpIf (ExpLiteral (LitIdent "c"),
				   ExpLiteral (LitIdent "d"),
				   ExpLiteral (LitIdent "f"))))
	"if 1+2 == 3 { b } else if c { d } else f";

  assert_eq_egg_expr
	(ExpIf (parse_string "1+2 == 3",
			ExpLiteral (LitIdent "b"),
			ExpIf (ExpLiteral (LitIdent "c"),
				   ExpLiteral (LitIdent "d"),
				   ExpLiteral (LitIdent "f"))))
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



(* test of translator.ml *)

(* open Translator *)
(* open Vm *)

(* let assert_eq_vminsns expected exp_str = *)
(*   assert_equal ~printer:string_of_vminsns expected *)
(* 	(translate (parse_string exp_str)) *)

(* let test_trans_simple_exp _ = *)

(*   () *)

(* let _ = add_suites begin "Translator" >::: [ *)
(*   "trans_simple_exp" >:: test_trans_simple_exp; *)
(* ] end *)


(* test of normal.ml *)

open Normal

let assert_eq_normal_expr expected exp_str =
  assert_equal ~msg:("Normalizing: " ^ exp_str)
	~cmp:normal_expr_equal
	~printer:(fun ne -> "\n" ^ (string_of_normal_expr ne) ^ "\n")
	expected (normalize (parse_string exp_str))

let assert_eq_letreduced_normal_expr expected exp_str =
  assert_equal ~msg:("Normalizing: " ^ exp_str)
	~cmp:normal_expr_equal
	~printer:(fun ne -> "\n" ^ (string_of_normal_expr ne) ^ "\n")
	expected (reduce_let (normalize (parse_string exp_str)))

let test_norm_simple_exp _ =
  assert_eq_normal_expr
	NexpUndef
	"undefined";

  assert_eq_normal_expr
	(NexpInt 1)
	"1";

  assert_eq_normal_expr
	(NexpFloat 1.0)
	"1.0";

  assert_eq_normal_expr
	(NexpBool false)
	"false";

  assert_eq_normal_expr
	(NexpString "foo")
	"\"foo\"";

  assert_eq_normal_expr
	(NexpVar "foo")
	"foo";

  assert_eq_normal_expr
	(NexpLambda (["x"], NexpVar "y"))
	"lambda (x) { y }";

  assert_eq_normal_expr
	(NexpLet ("tmp", (NexpInt 1),
			  (NexpPrefix (PrefixMinus, "tmp"))))
	"-1";

  assert_eq_normal_expr
	(NexpApply ("foo", ["x"; "y"]))
	"foo(x, y)";

  assert_eq_normal_expr
	(NexpLet ("tmp", NexpInt 1,
			  (NexpApply ("foo", ["tmp"; "y"]))))
	"foo(1, y)";

  assert_eq_normal_expr
	(NexpLet ("tmp1", NexpInt 1,
			  (NexpLet ("tmp2", NexpInt 2,
						(NexpApply ("foo", ["tmp1"; "tmp2"]))))))
	"foo(1, 2)";

  assert_eq_normal_expr
	(NexpBind ("foo", NexpInt 1))
	"bind foo -> 1";

  assert_eq_normal_expr
	(NexpLet ("foo", NexpInt 1, NexpVar "foo"))
	"let foo -> 1 in foo";

  assert_eq_normal_expr
	(NexpPrefix (PrefixPlus, "x"))
	"+ x";

  assert_eq_normal_expr
	(NexpPrefix (PrefixMinus, "x"))
	"- x";

  assert_eq_normal_expr
	(NexpInfix (InfixPlus, "x", "y"))
	"x + y";

  assert_eq_normal_expr
	(NexpLet ("a", NexpInt 1,
			  (NexpInfix (InfixPlus, "a", "y"))))
	"1 + x";

  assert_eq_normal_expr
	(NexpLet ("tmp1", NexpVar "x",
			  (NexpLet ("tmp2", NexpVar "y",
						(NexpVar "z")))))
	"x; y; z";

  assert_eq_normal_expr
	(NexpLet ("tmp1", NexpInt 1,
			  (NexpLet ("tmp2", NexpInt 2,
						(NexpInt 3)))))
	"1; 2; 3";

  ()

let test_norm_arith_exp _ =
  assert_eq_normal_expr
	(NexpLet ("t1",
			  NexpLet ("t2", NexpInt 1,
					   NexpLet ("t3",
								NexpLet ("t4", NexpInt 2,
										 NexpLet ("t5", NexpInt 3,
												  NexpInfix (InfixMul, "t4", "t5"))),
								NexpInfix (InfixPlus, "t2", "t3"))),
			  NexpLet ("t6",
					   NexpInt 4,
					   NexpInfix (InfixPlus, "t1", "t6"))))
	"1 + 2 * 3 + 4";

  ()

let test_letreduced_norm_arith_exp _ =
  assert_eq_letreduced_normal_expr
	(NexpLet ("t2",
			  NexpInt 1,
			  NexpLet ("t4",
					   NexpInt 2,
					   NexpLet ("t5",
								NexpInt 3,
								NexpLet ("t3",
										 NexpInfix (InfixMul, "t4", "t5"),
										 NexpLet ("t1",
												  NexpInfix (InfixPlus, "t2", "t3"),
												  NexpLet ("t6",
														   NexpInt 4,
														   NexpInfix (InfixPlus, "t1", "t6"))))))))
	"1 + 2 * 3 + 4";

  ()


let _ = add_suites begin "Normal" >::: [
  "norm_simple_exp" >:: test_norm_simple_exp;
  "norm_arith_exp" >:: test_norm_arith_exp;
  "letreduced_norm_arith_exp" >:: test_letreduced_norm_arith_exp;
] end


(* test of alpha.ml *)

let assert_eq_alpha expected actual =
  assert_equal ~msg:("Alpha-converting: " ^ (string_of_normal_expr actual))
	~cmp:(fun x y -> normal_expr_equal ~exact:true x y)
	~printer:(fun ne -> "\n" ^ (string_of_normal_expr ne) ^ "\n")
	expected actual

let test_normal_expr_equal _ =
  assert_eq_alpha
	(NexpLambda (["sym#x2"],
			NexpVar "sym#x2"))
	(NexpLambda (["sym#x12"],
				 NexpVar "sym#x12"))
  ;

  assert_eq_alpha
	(NexpLet ("sym#x1",
			  NexpVar "x",
			  (NexpLet ("sym#y",
						NexpVar "sym#x1",
						NexpLambda (["sym#x2"],
									NexpVar "sym#x2")))))
	(NexpLet ("sym#x11",
			  NexpVar "x",
			  (NexpLet ("sym#y",
						NexpVar "sym#x11",
						NexpLambda (["sym#x12"],
									NexpVar "sym#x12")))))
  ;

  ()

let test_alpha_convert _ =
  assert_eq_alpha
	(NexpLet ("sym#x",
			  NexpVar "x",
			  NexpVar "sym#x"))
	(Alpha.alpha_convert
	   (NexpLet ("x",
				 NexpVar "x",
				 NexpVar "x")))
	;

  assert_eq_alpha
	(NexpLet ("sym#x",
			  NexpInt 1,
			  NexpLet ("sym#y",
					   NexpVar "sym#x",
					   NexpVar "sym#y")))
	(Alpha.alpha_convert
	   (NexpLet ("x",
				 NexpInt 1,
				 NexpLet ("y",
						  NexpVar "x",
						  NexpVar "y"))))
	;

  assert_eq_alpha
	(NexpLet ("sym#x1",
			  NexpInt 1,
			  NexpLet ("sym#x2",
					   NexpVar "sym#x1",
					   NexpVar "sym#x2")))
	(Alpha.alpha_convert
	   (NexpLet ("x",
				 NexpInt 1,
				 NexpLet ("x",
						  NexpVar "x",
						  NexpVar "x"))))
	;

  assert_eq_alpha
	(NexpLet ("sym#x2",
			  NexpVar "x",
			  (NexpLet ("sym#y",
						NexpVar "sym#x2",
						NexpLambda (["sym#x3"],
									NexpVar "sym#x3")))))
	(Alpha.alpha_convert
	   (NexpLet ("x",
				 NexpVar "x",
				 (NexpLet ("y",
						   NexpVar "x",
						   NexpLambda (["x"],
									   NexpVar "x"))))))
  ;

  assert_eq_alpha
	(NexpLet ("sym#y",
			  NexpVar "x",
			  NexpLambda (["sym#x3"],
						  NexpVar "sym#x3")))
	(Alpha.alpha_convert
	   (NexpLet ("y",
				 NexpVar "x",
				 NexpLambda (["x"],
							 NexpVar "x"))))
  ;

  assert_eq_alpha
	(NexpLet ("sym#x1",
				 NexpInt 1,
				 NexpLet ("sym#x2",
						  NexpVar "sym#x1",
						  (NexpLet ("sym#y",
									NexpVar "sym#x2",
									NexpApply ("f", ["sym#x2"]))))))
	(Alpha.alpha_convert
	   (NexpLet ("x",
				 NexpInt 1,
				 NexpLet ("x",
						  NexpVar "x",
						  (NexpLet ("y",
									NexpVar "x",
									NexpApply ("f", ["x"])))))))
  ;

  assert_eq_alpha
	(NexpLet ("sym#x1",
			  NexpInt 1,
			  NexpLet ("sym#x2",
					   NexpVar "sym#x1",
					   NexpBind ("x", NexpInt 1))))
	(Alpha.alpha_convert
	   (NexpLet ("x",
			  NexpInt 1,
			  NexpLet ("x",
					   NexpVar "x",
					   NexpBind ("x", NexpInt 1)))))
  ;

  assert_eq_alpha
	(NexpLet ("sym#x1",
			  NexpInt 1,
			  NexpLet ("sym#x2",
					   NexpVar "sym#x1",
					   NexpBind ("y", NexpVar "sym#x2"))))
	(Alpha.alpha_convert
	   (NexpLet ("x",
			  NexpInt 1,
			  NexpLet ("x",
					   NexpVar "x",
					   NexpBind ("y", NexpVar "x")))))
  ;

  assert_eq_alpha
	(NexpLet ("sym#x1",
			  NexpInt 1,
			  NexpLet ("sym#x2",
					   NexpVar "sym#x1",
					   NexpPrefix (PrefixPlus, "sym#x2"))))
	(Alpha.alpha_convert
	   (NexpLet ("x",
				 NexpInt 1,
				 NexpLet ("x",
						  NexpVar "x",
						  NexpPrefix (PrefixPlus, "x")))))
  ;

  assert_eq_alpha
	(NexpLet ("sym#x1",
			  NexpInt 1,
			  NexpLet ("sym#x2",
					   NexpVar "sym#x1",
					   NexpInfix (InfixPlus, "sym#x2", "y"))))
	(Alpha.alpha_convert
	   (NexpLet ("x",
				 NexpInt 1,
				 NexpLet ("x",
						  NexpVar "x",
						  NexpInfix (InfixPlus, "x", "y")))))
  ;

  assert_eq_alpha
	(NexpLet ("sym#x",
			  NexpVar "x",
			  NexpIf ("y", NexpVar "z", NexpVar "w")))
	(Alpha.alpha_convert
	   (NexpLet ("x",
				 NexpVar "x",
				 NexpIf ("y", NexpVar "z", NexpVar "w"))))
  ;

  assert_eq_alpha
	(NexpLet ("sym#x",
			  NexpVar "x",
			  NexpIf ("sym#x", NexpVar "sym#x", NexpVar "sym#x")))
	(Alpha.alpha_convert
	   (NexpLet ("x",
				 NexpVar "x",
				 NexpIf ("x", NexpVar "x", NexpVar "x"))))
  ;


  ()

let _ = add_suites begin "Alpha" >::: [
  "normal_expr_equal" >:: test_normal_expr_equal;
  "alpha_convert" >:: test_alpha_convert;
] end



(* test of beta.ml *)

let assert_eq_beta expected actual =
  assert_equal ~msg:("Beta-reducing: " ^ (string_of_normal_expr actual))
	~cmp:(fun x y -> normal_expr_equal ~exact:true x y)
	~printer:(fun ne -> "\n" ^ (string_of_normal_expr ne) ^ "\n")
	expected actual

let test_beta_reduce _ =
  assert_eq_beta
	  (NexpLet ("x",
				NexpInt 1,
				NexpVar "x"))
	(Beta.beta_reduce
	   (NexpLet ("x",
				 NexpInt 1,
				 NexpLet ("y",
						  NexpVar "x",
						  NexpVar "x"))))
  ;

  assert_eq_beta
	(NexpLet ("x",
			  NexpInt 1,
			  NexpLet ("y",
					   NexpInt 2,
					   NexpVar "x")))
	(Beta.beta_reduce
	   (NexpLet ("x",
				 NexpInt 1,
				 NexpLet ("y",
						  NexpInt 2,
						  NexpLet ("z",
								   NexpVar "x",
								   NexpVar "z")))))
  ;

  assert_eq_beta
	(NexpLet ("x",
			  NexpInt 1,
			  NexpLet ("y",
					   NexpInt 2,
					   NexpVar "w")))
	(Beta.beta_reduce
	   (NexpLet ("x",
				 NexpInt 1,
				 NexpLet ("y",
						  NexpInt 2,
						  NexpLet ("z",
								   NexpVar "x",
								   NexpVar "w")))))
  ;

  assert_eq_beta
	(NexpLet ("x",
			  NexpInt 1,
			  NexpLambda (["z"],
						  NexpVar "x")))
	(Beta.beta_reduce
	   (NexpLet ("x",
				 NexpInt 1,
				 NexpLet ("y",
						  NexpVar "x",
						  NexpLambda (["z"],
									  NexpVar "y")))))
  ;

  assert_eq_beta
	(NexpLet ("x",
			  NexpInt 1,
			  NexpApply ("x", ["z"])))
	(Beta.beta_reduce
	   (NexpLet ("x",
				 NexpInt 1,
				 NexpLet ("y",
						  NexpVar "x",
						  NexpApply ("y", ["z"])))))
  ;

  assert_eq_beta
	(NexpLet ("x",
			  NexpInt 1,
			  NexpApply ("z", ["x"])))
	(Beta.beta_reduce
	   (NexpLet ("x",
				 NexpInt 1,
				 NexpLet ("y",
						  NexpVar "x",
						  NexpApply ("z", ["x"])))))
  ;

  assert_eq_beta
	(NexpLet ("x",
			  NexpInt 1,
			  NexpBind ("y", NexpVar"z")))
	(Beta.beta_reduce
	   (NexpLet ("x",
				 NexpInt 1,
				 NexpLet ("y",
						  NexpVar "x",
						  NexpBind ("y", NexpVar"z")))))
  ;

  assert_eq_beta
	(NexpLet ("x",
			  NexpInt 1,
			  NexpBind ("z", NexpVar"x")))
	(Beta.beta_reduce
	   (NexpLet ("x",
				 NexpInt 1,
				 NexpLet ("y",
						  NexpVar "x",
						  NexpBind ("z", NexpVar"y")))))
  ;

  assert_eq_beta
	(NexpLet ("x",
			  NexpInt 1,
			  NexpLet ("z", NexpInt 1,
								   NexpVar"x")))
	(Beta.beta_reduce
	   (NexpLet ("x",
				 NexpInt 1,
				 NexpLet ("y",
						  NexpVar "x",
						  NexpLet ("z", NexpInt 1,
								   NexpVar"y")))))
  ;

  assert_eq_beta
	(NexpLet ("x",
			  NexpInt 1,
			  NexpPrefix (PrefixPlus, "x")))
	(Beta.beta_reduce
	   (NexpLet ("x",
				 NexpInt 1,
				 NexpLet ("y",
						  NexpVar "x",
						  NexpPrefix (PrefixPlus, "y")))))
  ;

  assert_eq_beta
	(NexpLet ("x",
			  NexpInt 1,
			  NexpInfix (InfixPlus, "x", "z")))
	(Beta.beta_reduce
	   (NexpLet ("x",
				 NexpInt 1,
				 NexpLet ("y",
						  NexpVar "x",
						  NexpInfix (InfixPlus, "y", "z")))))
  ;

  assert_eq_beta
	(NexpLet ("x",
			  NexpInt 1,
			  NexpIf ("z", NexpVar "v", NexpVar "w")))
	(Beta.beta_reduce
	   (NexpLet ("x",
				 NexpInt 1,
				 NexpLet ("y",
						  NexpVar "x",
						  NexpIf ("z", NexpVar "v", NexpVar "w")))))
  ;

  assert_eq_beta
	(NexpLet ("x",
			  NexpInt 1,
			  NexpIf ("x", NexpVar "x", NexpVar "x")))
	(Beta.beta_reduce
	   (NexpLet ("x",
				 NexpInt 1,
				 NexpLet ("y",
						  NexpVar "x",
						  NexpIf ("y", NexpVar "y", NexpVar "y")))))
  ;
  ()

let _ = add_suites begin "Beta" >::: [
  "beta_reduce" >:: test_beta_reduce;
] end



(* test of constfold.ml *)

let assert_eq_constfold expected actual =
  assert_equal ~msg:("Folding const: " ^ (string_of_normal_expr actual))
	~cmp:(fun x y -> normal_expr_equal ~exact:true x y)
	~printer:(fun ne -> "\n" ^ (string_of_normal_expr ne) ^ "\n")
	expected actual

let test_const_fold_simple _ =
  (* assert_eq_constfold *)
  (* 	(NexpLet ("x", *)
  (* 				 NexpInt 1, *)
  (* 				 NexpLet ("y", *)
  (* 						  NexpInt 2, *)
  (* 						  NexpInt 3))) *)
  (* 	(Constfold.const_fold *)
  (* 	   (NexpLet ("x", *)
  (* 				 NexpInt 1, *)
  (* 				 NexpLet ("y", *)
  (* 						  NexpInt 2, *)
  (* 						  (NexpInfix (InfixPlus, "x", "y")))))) *)
  (* ; *)

  (* assert_eq_constfold *)
  (* 	(NexpLet ("t2", *)
  (* 			  NexpInt 1, *)
  (* 			  NexpLet ("t4", *)
  (* 					   NexpInt 2, *)
  (* 					   NexpLet ("t5", *)
  (* 								NexpInt 3, *)
  (* 								NexpLet ("t3", *)
  (* 										 NexpInfix (InfixMul, "t4", "t5"), *)
  (* 										 NexpLet ("t1", *)
  (* 												  NexpInfix (InfixPlus, "t2", "t3"), *)
  (* 												  NexpLet ("t6", *)
  (* 														   NexpInt 4, *)
  (* 														   NexpInfix (InfixPlus, "t1", "t6")))))))) *)
  (* 	(Constfold.const_fold *)
  (* 	   (NexpLet ("t2", *)
  (* 			  NexpInt 1, *)
  (* 			  NexpLet ("t4", *)
  (* 					   NexpInt 2, *)
  (* 					   NexpLet ("t5", *)
  (* 								NexpInt 3, *)
  (* 								NexpLet ("t3", *)
  (* 										 NexpInt 6, *)
  (* 										 NexpLet ("t1", *)
  (* 												  NexpInt 7, *)
  (* 												  NexpLet ("t6", *)
  (* 														   NexpInt 4, *)
  (* 														   NexpInt 11)))))))) *)
  (* ; *)

  ()

let _ = add_suites begin "Constfold" >::: [
  "const_fold_simple" >:: test_const_fold_simple;
] end



let _ =
  List.iter
	(fun suite ->
	   let _ = run_test_tt ~verbose:false suite
	   in ()
	)
	(List.rev !suites)
