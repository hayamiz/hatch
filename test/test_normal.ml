
(* test of normal.ml *)

open Testutil

let assert_eq_normal_expr expected exp_str =
  assert_equal ~msg:("Normalizing: " ^ exp_str)
	~cmp:normal_expr_equal
	~printer:(fun ne -> "\n" ^ (string_of_normal_expr ne) ^ "\n")
	expected (normalize (parse_string exp_str))

let assert_eq_letreduced_normal_expr expected exp_str =
  assert_equal ~msg:("Normalizing: " ^ exp_str)
	~cmp:normal_expr_equal
	~printer:(fun ne -> "\n" ^ (string_of_normal_expr ne) ^ "\n")
	expected (serialize_let (normalize (parse_string exp_str)))

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

let test_norm_lambda_if _ =
  assert_eq_normal_expr
	(NexpLambda (["x"],
				 NexpIf ("a",
						 NexpInt 1,
						 NexpVar "b")))
	"lambda(x){ if a 1 else b }";

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

let test_letreduced_norm_seq _ =
  assert_eq_letreduced_normal_expr
	(NexpLet ("sym#5",
			  (NexpLambda (["n"],
						   NexpLet ("sym#0",
									NexpInt 0,
									NexpLet ("sym#1",
											 NexpInfix (InfixEq, "n", "sym#0"),
											 NexpLet ("sym#2",
													  NexpInt 1,
													  NexpInfix (InfixLor, "sym#1", "sym#2")))))),
			  NexpBool true))
	"lambda (n) { n = 0 || 1 }; true";
  ()

let add_suites _ =
  add_suites
	begin "Normal" >::: [
	  "norm_simple_exp" >:: test_norm_simple_exp;
	  "norm_arith_exp" >:: test_norm_arith_exp;
	  "norm_lambda_if" >:: test_norm_lambda_if;
	  "letreduced_norm_arith_exp" >:: test_letreduced_norm_arith_exp;
	  "letreduced_norm_seq" >:: test_letreduced_norm_seq;
	] end
;;
