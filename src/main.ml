
let hoge _ =
  let lexbuf = Lexing.from_channel stdin in
	print_string
	  (Lambda.string_of_ll_program
		 (Lambda.lambda_lift
			(Constfold.const_fold
			   (Beta.beta_reduce
				  (Normal.reduce_let
					 (Alpha.alpha_convert
						(Normal.normalize
						   (Parser.main Lexer.token lexbuf))))))));
    exit 0

let step1 _ = 
  let lexbuf = Lexing.from_channel stdin in
	print_string
	  (ParserUtil.string_of_expr
		 (Parser.main Lexer.token lexbuf));
	exit 0

let step2 _ =
  let lexbuf = Lexing.from_channel stdin in
	print_string
	  (Normal.string_of_normal_expr
		 (Normal.normalize
			(Parser.main Lexer.token lexbuf)));
	exit 0

let step3 _ =
  let lexbuf = Lexing.from_channel stdin in
	print_string
	  (Normal.string_of_normal_expr
		 (Alpha.alpha_convert
			(Normal.normalize
			   (Parser.main Lexer.token lexbuf))));
	exit 0

let step4 _ =
  let lexbuf = Lexing.from_channel stdin in
	print_string
	  (Normal.string_of_normal_expr
		 (Normal.reduce_let
			(Alpha.alpha_convert
			   (Normal.normalize
				  (Parser.main Lexer.token lexbuf)))));
	exit 0

let step5 _ =
  let lexbuf = Lexing.from_channel stdin in
	print_string
	  (Normal.string_of_normal_expr
		 (Beta.beta_reduce
			(Normal.reduce_let
			   (Alpha.alpha_convert
				  (Normal.normalize
					 (Parser.main Lexer.token lexbuf))))));
	exit 0

let step6 _ =
  let lexbuf = Lexing.from_channel stdin in
	print_string
	  (Lambda.string_of_ll_program
		 (Lambda.lambda_lift
			(Constfold.const_fold
			   (Beta.beta_reduce
				  (Normal.reduce_let
					 (Alpha.alpha_convert
						(Normal.normalize
						   (Parser.main Lexer.token lexbuf))))))));
	exit 0

let _ =
  step6 ()
