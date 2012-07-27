
(* test of constfold.ml *)

open Testutil

let assert_eq_constfold expected actual =
  assert_equal ~msg:("Folding const: " ^ (string_of_normal_expr actual))
	~cmp:(fun x y -> normal_expr_equal ~exact:true x y)
	~printer:(fun ne -> "\n" ^ (string_of_normal_expr ne) ^ "\n")
	expected actual

let test_const_fold_simple _ =
  (* assert_eq_constfold *)
  (* 	(NexpLet ("x", *)
  (* 				 NexpInt 1, *)
  (* 				 NexpLet ("y", *)
  (* 						  NexpInt 2, *)
  (* 						  NexpInt 3))) *)
  (* 	(Constfold.const_fold *)
  (* 	   (NexpLet ("x", *)
  (* 				 NexpInt 1, *)
  (* 				 NexpLet ("y", *)
  (* 						  NexpInt 2, *)
  (* 						  (NexpInfix (InfixPlus, "x", "y")))))) *)
  (* ; *)

  (* assert_eq_constfold *)
  (* 	(NexpLet ("t2", *)
  (* 			  NexpInt 1, *)
  (* 			  NexpLet ("t4", *)
  (* 					   NexpInt 2, *)
  (* 					   NexpLet ("t5", *)
  (* 								NexpInt 3, *)
  (* 								NexpLet ("t3", *)
  (* 										 NexpInfix (InfixMul, "t4", "t5"), *)
  (* 										 NexpLet ("t1", *)
  (* 												  NexpInfix (InfixPlus, "t2", "t3"), *)
  (* 												  NexpLet ("t6", *)
  (* 														   NexpInt 4, *)
  (* 														   NexpInfix (InfixPlus, "t1", "t6")))))))) *)
  (* 	(Constfold.const_fold *)
  (* 	   (NexpLet ("t2", *)
  (* 			  NexpInt 1, *)
  (* 			  NexpLet ("t4", *)
  (* 					   NexpInt 2, *)
  (* 					   NexpLet ("t5", *)
  (* 								NexpInt 3, *)
  (* 								NexpLet ("t3", *)
  (* 										 NexpInt 6, *)
  (* 										 NexpLet ("t1", *)
  (* 												  NexpInt 7, *)
  (* 												  NexpLet ("t6", *)
  (* 														   NexpInt 4, *)
  (* 														   NexpInt 11)))))))) *)
  (* ; *)

  ()

let add_suites _ =
  add_suites begin "Constfold" >::: [
	"const_fold_simple" >:: test_const_fold_simple;
  ] end
;;
