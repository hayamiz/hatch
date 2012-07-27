(* test of beta.ml *)

include Testutil

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

let add_suites _ =
  add_suites begin "Beta" >::: [
	"beta_reduce" >:: test_beta_reduce;
  ] end
;;
