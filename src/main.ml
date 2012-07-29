
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

let step7 _ =
  let lexbuf = Lexing.from_channel stdin in
	print_string
	  (Highlevel.string_of_hlvminsts
		 (Highlevel.compile
			(Lambda.lambda_lift
			   (Constfold.const_fold
				  (Beta.beta_reduce
					 (Normal.reduce_let
						(Alpha.alpha_convert
						   (Normal.normalize
							  (Parser.main Lexer.token lexbuf)))))))));
	exit 0

let step8 _ =
  let lexbuf = Lexing.from_channel stdin in
	print_string
	  (Vm.string_of_vminsts
		 (Vm.compile
			(Highlevel.compile
			   (Lambda.lambda_lift
				  (Constfold.const_fold
					 (Beta.beta_reduce
						(Normal.reduce_let
						   (Alpha.alpha_convert
							  (Normal.normalize
								 (Parser.main Lexer.token lexbuf))))))))));
	exit 0

let step9 _ =
  let lexbuf = Lexing.from_channel stdin in
  let ret = Vm.run_vminsts (Translator.translate lexbuf)
  in
	if Hvalue.h_is_true ret then
	  exit 0
	else
	  exit 1

let opt_lambda = ref false
let opt_disasm = ref false
let opt_show_step = ref false

let run_file filepath =
  let lexbuf = Lexing.from_channel (open_in filepath) in
  if !opt_lambda then
    begin
      print_endline
	(Lambda.string_of_ll_program
	   (Lambda.lambda_lift
	      (Constfold.const_fold
		 (Beta.beta_reduce
		    (Normal.reduce_let
		       (Alpha.alpha_convert
			  (Normal.normalize
			     (Parser.main Lexer.token lexbuf))))))));
      exit 0
    end
  else if !opt_disasm then
    begin
      print_endline
	(Highlevel.string_of_hlvminsts
	   (Highlevel.compile
	      (Lambda.lambda_lift
		 (Constfold.const_fold
		    (Beta.beta_reduce
		       (Normal.reduce_let
			  (Alpha.alpha_convert
			     (Normal.normalize
				(Parser.main Lexer.token lexbuf)))))))));
      exit 0
    end
  else
    let _ = Vm.run_vminsts (Translator.translate lexbuf)
    in exit 0

let _ =
  Arg.parse_argv
	Sys.argv
	[("-D", Arg.Set opt_disasm, "disassemble program");
	 ("--disasm", Arg.Set opt_disasm, "disassemble program");
	 ("-L", Arg.Set opt_lambda, "show lambda-lifted program");
	 ("--lambda", Arg.Set opt_lambda, "show lambda-lifted program");
	 ("--show-step", Arg.Set opt_show_step, "show each step")]
	run_file ""
