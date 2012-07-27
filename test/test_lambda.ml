(* test of lambda.ml *)

open Testutil

let rec ll_expr_equal ?(exact=false) expected actual =
  let comp_sym x y =
	if exact then
	  if (String.length x >= 4 && (String.sub x 0 4) = "sym#") &&
		(String.length y >= 4 && (String.sub y 0 4) = "sym#") then
		  true
	  else
		x = y
	else
	   true
  in
  let rec comp_syms ss1 ss2 =
	match (ss1, ss2) with
		([], []) -> true
	  | (_ :: _, [])
	  | ([], _ :: _) -> false
	  | (s1 :: ss1, s2 :: ss2) ->
		  if comp_sym s1 s2 then
			comp_syms ss1 ss2
		  else false
  in
	match (expected, actual) with
		(LLVar x, LLVar y) -> comp_sym x y
	  | (LLInt x, LLInt y) when x = y -> true
	  | (LLFloat x, LLFloat y) when x = y -> true
	  | (LLString x, LLString y) when x = y -> true
	  | (LLBool x, LLBool y) when x = y -> true
	  | (LLUndef , LLUndef ) -> true
	  | (LLMakeCls (f1, fvar_vals1), LLMakeCls (f2, fvar_vals2)) ->
		  (comp_sym f1 f2) &&
			(comp_syms fvar_vals1 fvar_vals2)
	  | (LLFunApply (f1, args1), LLFunApply (f2, args2)) ->
		  (comp_sym f1 f2) &&
			(comp_syms args1 args2)
	  | (LLClsApply (f1, fv_vals1, args1), LLClsApply (f2, fv_vals2, args2)) ->
		  (comp_sym f1 f2) &&
			(comp_syms fv_vals1 fv_vals2) &&
			(comp_syms args1 args2)
	  | (LLBind (id1, v1), LLBind (id2, v2)) ->
		  (comp_sym id1 id2) && (ll_expr_equal ~exact:exact v1 v2)
	  | (LLLet (id1, v1, body1), LLLet (id2, v2, body2)) ->
		  (comp_sym id1 id2) && 
			(ll_expr_equal ~exact:exact v1 v2) &&
			(ll_expr_equal ~exact:exact body1 body2)
	  | (LLPrefix (op1, v1), LLPrefix (op2, v2)) ->
		  (op1 = op2) && (comp_sym v1 v2)
	  | (LLInfix (op1, lv1, rv1), LLInfix (op2, lv2, rv2)) ->
		  (op1 = op2) && (comp_sym lv1 lv2) && (comp_sym rv1 rv2)
	  | (LLIf (c1, if1, else1), LLIf (c2, if2, else2)) -> 
		  (comp_sym c1 c2) && (ll_expr_equal ~exact:exact if1 if2) && (ll_expr_equal ~exact:exact else1 else2)
	  | _ ->
		  false

let equal_ll_program llp1 llp2 =
  let rec equal_ll_funcs funcs1 funcs2 =
	match (funcs1, funcs2) with
		([], []) -> true
	  | ((LLFlatFun (f1, params1, body1)) :: funcs1,
		 (LLFlatFun (f2, params2, body2)) :: funcs2) ->
		  ((List.length params1) == (List.length params2)) &&
			(ll_expr_equal body1 body2) &&
			(equal_ll_funcs funcs1 funcs2)
	  | ((LLClsFun (f1, fvars1, params1, body1)) :: funcs1,
		 (LLClsFun (f2, fvars2, params2, body2)) :: funcs2) ->
		  ((List.length params1) == (List.length params2)) &&
			((List.length fvars1) == (List.length fvars2)) &&
			(ll_expr_equal body1 body2) &&
			(equal_ll_funcs funcs1 funcs2)
	  | _ ->
		  false
  in
	(equal_ll_funcs llp1.funcs llp2.funcs) &&
	  (ll_expr_equal llp1.main llp2.main)


let assert_equal_lambda_shift expected actual =
  assert_equal ~msg:("Lambda-Lifting: " ^ (string_of_normal_expr actual))
	~cmp:(fun x y -> equal_ll_program x y)
	~printer:(fun lle -> "\n" ^ (string_of_ll_program lle) ^ "\n")
	expected (lambda_lift actual)


let test_lambda_simple _ =
  assert_equal_lambda_shift
	({ funcs = []; main = LLVar "x" })
	 (NexpVar "x");

  assert_equal_lambda_shift
	({ funcs = [];
	   main = (LLInt 1) })
	(NexpInt 1);
  
  assert_equal_lambda_shift
	({ funcs = [];
	   main = (LLFloat 1.23) })
	(NexpFloat 1.23);
  
  assert_equal_lambda_shift
	({ funcs = [];
	   main = (LLString "piyo") })
	 (NexpString "piyo");

  assert_equal_lambda_shift
	({ funcs = [];
	   main = (LLBool false) })
	(NexpBool false);
  assert_equal_lambda_shift
	({ funcs = [];
	   main = (LLBool true) })
	(NexpBool true);

  assert_equal_lambda_shift
	({ funcs = [];
	   main = (LLUndef) })
	(NexpUndef);

  assert_equal_lambda_shift
	({ funcs = [];
	   main = (LLFunApply ("samefunc", ["arg1"; "arg2"])) })
	(NexpApply ("samefunc", ["arg1"; "arg2"]));

  assert_equal_lambda_shift
	({ funcs = [];
	   main = (LLBind ("hoge", LLInt 5)) })
	(NexpBind ("hoge", NexpInt 5));

  assert_equal_lambda_shift
	({ funcs = [];
	   main = (LLLet ("sym#x", LLInt 1, LLVar "sym#x")) })
	(NexpLet ("sym#x", NexpInt 1, NexpVar "sym#x"));

  assert_equal_lambda_shift
	({ funcs = [];
	   main = (LLPrefix (PrefixMinus, "hoge")) })
	(NexpPrefix (PrefixMinus, "hoge"));

  assert_equal_lambda_shift
	({ funcs = [];
	   main = (LLInfix (InfixMul, "hoge", "fuga")) })
	(NexpInfix (InfixMul, "hoge", "fuga"));

  assert_equal_lambda_shift
	({ funcs = [];
	   main = (LLInfix (InfixMul, "hoge", "fuga")) })
	(NexpInfix (InfixMul, "hoge", "fuga"));

  assert_equal_lambda_shift
	({ funcs = [];
	   main = (LLLet ("x",
						LLInt 1,
						LLIf ("x", LLVar "x", LLVar "x"))) })
	(NexpLet ("x",
			  NexpInt 1,
			  NexpIf ("x", NexpVar "x", NexpVar "x")));

  ()

let test_lambda_flatfun _ =
  assert_equal_lambda_shift
	({ funcs = [LLFlatFun ("sym#fun1", ["hoge"],
						   LLVar "hoge")];
	   main =  (LLLet ("x",
					   LLVar "sym#fun1",
					   LLUndef))})
	(NexpLet ("x",
			  NexpLambda (["hoge"],
						  NexpVar "hoge"),
			  NexpUndef));

  assert_equal_lambda_shift
	({ funcs = [LLFlatFun ("sym#fun1", ["hoge"],
						   LLVar "hoge")];
	   main =  (LLLet ("x",
					   LLVar "sym#fun1",
					   LLUndef))})
	(NexpLet ("x",
			  NexpLambda (["hoge"],
						  NexpVar "hoge"),
			  NexpUndef));
  
  ()

let test_lambda_closure _ =
  assert_equal_lambda_shift
	({ funcs = [LLClsFun ("sym#fun1",
						  ["x"],
						  ["hoge"],
						  LLVar "x")];
	   main =  (LLLet ("x",
					   LLInt 1,
					   LLMakeCls ("sym#fun1", ["x"])))})
	(NexpLet ("x",
			  NexpInt 1,
			  NexpLambda (["hoge"],
						  NexpVar "x")));

  assert_equal_lambda_shift
	({ funcs = [LLClsFun ("sym#fun2",
						  ["x"],
						  ["a"],
						  LLMakeCls ("sym#fun1", ["x"; "a"]));
			   LLClsFun ("sym#fun1",
						  ["x"; "a"],
						  ["b"],
						 LLInfix (InfixPlus, "x", "a"))];
	   main =  (LLLet ("x",
					   LLInt 1,
					   LLMakeCls ("sym#fun2", ["x"])))})
	(NexpLet ("x",
			  NexpInt 1,
			  NexpLambda (["a"],
						  NexpLambda (["b"],
									  NexpInfix (InfixPlus, "x", "a")))));

  ()

let add_suites _ =
  add_suites begin "Lambda" >::: [
	"lambda_simple" >:: test_lambda_simple;
	"lambda_flatfun" >:: test_lambda_flatfun;
	"lambda_closure" >:: test_lambda_closure;
  ] end
