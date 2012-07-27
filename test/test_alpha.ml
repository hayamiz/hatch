(* test of alpha.ml *)

include Testutil

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

let add_suites _ =
  add_suites begin "Alpha" >::: [
	"normal_expr_equal" >:: test_normal_expr_equal;
	"alpha_convert" >:: test_alpha_convert;
  ] end
