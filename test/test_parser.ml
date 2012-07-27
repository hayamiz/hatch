
(* test of parser.ml *)

include Testutil

let assert_eq_egg_expr expected exp_str =
  try
	let tree = parse_string exp_str
  in
	assert_equal ~printer:id ~msg:("parsing: " ^ exp_str)
	  (string_of_expr expected)
	  (string_of_expr tree)
  with Parsing.Parse_error ->
	print_string ("Parse error: " ^ exp_str ^ "\n");
	raise (Failure ("Parse error: " ^ exp_str ^ "\n"))

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


  assert_eq_egg_expr
	(ExpInfix (InfixPlus,
			   (parse_string "bar(1)"),
			   (parse_string "foo(2,3)")))
	("bar(1) + foo(2,3)");

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


let test_parser_infix_expr_apply _ =
  assert_eq_egg_expr
	(ExpInfix (InfixMul,
			   ExpLiteral (LitInt 1),
			   ExpApply (ExpLiteral (LitIdent "foo"),
						 [ExpLiteral (LitInt 2)])))
	("1 + foo(2)");

  assert_eq_egg_expr
	(ExpInfix (InfixMul,
			   ExpApply (ExpLiteral (LitIdent "bar"),
						 [ExpLiteral (LitInt 1)]),
			   ExpApply (ExpLiteral (LitIdent "foo"),
						 [ExpLiteral (LitInt 2)])))
	("bar(1) + foo(2)");

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

let test_parser_multiline _ =
  assert_eq_egg_expr
	(ExpSeq ([ExpLiteral (LitIdent "a");
			  ExpLiteral (LitIdent "b");
			  ExpLiteral (LitInt   1)]))
	("a;\nb;\n 1");

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

let test_parser_if_nobrace_expr _ =
  assert_eq_egg_expr
	(ExpIf (ExpLiteral (LitIdent "a"),
			ExpLiteral (LitIdent "b"),
			ExpLiteral (LitIdent "c")))
	"if a b else c";

  assert_eq_egg_expr
	(ExpIf (ExpLiteral (LitIdent "a"),
			ExpLiteral (LitIdent "b"),
			ExpIf (ExpLiteral (LitIdent "c"),
				   ExpLiteral (LitIdent "d"),
				   ExpLiteral (LitIdent "f"))))
	"if a b else if c d else f";

  assert_eq_egg_expr
	(ExpIf (parse_string "1+2 == 3",
			ExpLiteral (LitIdent "b"),
			ExpLiteral (LitIdent "d")))
	"if 1+2 == 3 b else d ";

  assert_eq_egg_expr
	(ExpIf (parse_string "1+2 == 3",
			ExpLiteral (LitIdent "b"),
			ExpIf (ExpLiteral (LitIdent "c"),
				   ExpLiteral (LitIdent "d"),
				   ExpLiteral (LitIdent "f"))))
	"if 1+2 == 3 b else if c d else f";

  assert_eq_egg_expr
	(ExpIf (parse_string "1+2 == 3",
			ExpLiteral (LitIdent "b"),
			ExpIf (ExpLiteral (LitIdent "c"),
				   ExpLiteral (LitIdent "d"),
				   ExpLiteral (LitIdent "f"))))
	"if 1+2 == 3 b else if c d else f";

  assert_eq_egg_expr
	(ExpIf (parse_string "1+2 == 3",
			parse_string "n + 1",
			parse_string "m + 2"))
	"if 1+2 == 3 \n n + 1 else m + 2";

  assert_eq_egg_expr
	(ExpIf (parse_string "1+2 == 3",
			parse_string "n + 1",
			parse_string "1 + foo(2)"))
	"if 1+2 == 3 \n n + 1 else 1 + foo(2)";

  assert_eq_egg_expr
	(ExpIf (parse_string "1+2 == 3",
			parse_string "n + 1",
			parse_string "bar(2) + foo(2)"))
	"if 1+2 == 3 \n n + 1 else bar(2) + foo(2)";

  ()
;;

let add_suites _ =
  add_suites begin "Parser" >::: [
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
	"parser_multiline" >:: test_parser_multiline;
	"parser_block_expr" >:: test_parser_block_expr;
	"parser_if_expr" >:: test_parser_if_expr;
	"parser_if_nobrace_expr" >:: test_parser_if_nobrace_expr;
  ] end
