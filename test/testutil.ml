
include OUnit
include Syntax
include Parser
include ParserUtil
include Normal
include Lambda

let suites: OUnit.test list ref = ref []

let add_suites suite =
  suites := suite :: !suites

let parse_string str =
  let lexbuf = Lexing.from_string str in
	Parser.main Lexer.token lexbuf

let assert_eq_tokens expected actual =
  assert_equal ~printer:string_of_tokens expected actual

let tokens_from_string str =
  let lexbuf = Lexing.from_string str
  in
  let rec read_toks toks lexbuf =
	match (Lexer.token lexbuf) with 
		EOF _ -> List.rev toks
	  | tok -> read_toks (tok :: toks) lexbuf
  in
	read_toks [] lexbuf

let id x = x

