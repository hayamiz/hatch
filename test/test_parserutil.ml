(* test of paserUtil.ml *)

open Testutil

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

let add_suites _ =
  add_suites begin "ParserUtil" >::: [
	"string_of_token" >:: test_string_of_token;
	"string_of_tokens" >:: test_string_of_tokens;
	"egg_expr_equal" >:: test_egg_expr_equal;
  ] end
;;

