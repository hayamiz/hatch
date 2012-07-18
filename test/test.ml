
open OUnit
open Parser
open ParserUtil

let id x = x

let tokens_from_string str =
  let lexbuf = Lexing.from_string str
  and toks = ref [] in
	try
	  while true do
		toks := (Lexer.token lexbuf) :: !toks;
	  done;
	  !toks
	with Lexer.Eof ->
	  List.rev !toks

let test_simple_arith_exp _ =
  assert_equal ~printer:string_of_tokens [INT 1; PLUS; INT 1] (tokens_from_string "1+1");
  assert_equal ~printer:string_of_tokens [INT 1; MINUS; INT 2] (tokens_from_string "1 - 2");
  assert_equal ~printer:string_of_tokens [INT 3; MUL; INT 4] (tokens_from_string "3 * 4");
  assert_equal ~printer:string_of_tokens [INT 1; DIV; INT 2] (tokens_from_string "1 / 2")

let test_arith_exp _ =
  assert_equal [INT 1; PLUS; INT 2; MUL; INT 3] (tokens_from_string "1+2*3");
  assert_equal [LPAREN; INT 1; PLUS; INT 2; RPAREN; MUL; INT 3] (tokens_from_string "(1+2)*3")

let test_string_of_token _ =
  assert_equal ~printer:id "INT 1" (ParserUtil.string_of_token (INT 1));
  assert_equal ~printer:id "PLUS" (ParserUtil.string_of_token PLUS);
  assert_equal ~printer:id "MINUS" (ParserUtil.string_of_token MINUS)

let test_string_of_tokens _ =
  assert_equal "INT 1; PLUS; INT 2" (ParserUtil.string_of_tokens [INT 1; PLUS; INT 2])

let suite = "Parser" >::: [
  "string_of_token" >:: test_string_of_token;
  "string_of_tokens" >:: test_string_of_tokens
]

let _ = run_test_tt_main suite

let suite = "Lexer" >::: [
  "simple arithmetic exp" >:: test_simple_arith_exp;
  "arithmetic exp" >:: test_arith_exp;
]

let _ = run_test_tt_main suite
