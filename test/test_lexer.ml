(* test of lexer.ml *)

open Testutil

let test_lex_int_lit _ =
  assert_eq_tokens [INT (1)] (tokens_from_string "1");
  assert_eq_tokens [INT (16)] (tokens_from_string "0x10");
  ()

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
  assert_eq_tokens [IDENT ("fo_o"); INT (1)] (tokens_from_string "fo_o 1");
  ()

let add_suites _ =
  add_suites begin "Lexer" >::: [
	"lex_int_lit" >:: test_lex_int_lit;
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
