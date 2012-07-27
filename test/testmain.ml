
open Testutil

let _ =
  Test_parserutil.add_suites ();
  Test_lexer.add_suites ();
  Test_parser.add_suites ();
  Test_normal.add_suites ();
  Test_alpha.add_suites ();
  Test_beta.add_suites ();
  Test_constfold.add_suites ();
  Test_lambda.add_suites ();
;;


let _ =
  List.iter
	(fun suite ->
	   let _ = run_test_tt ~verbose:false suite
	   in ()
	)
	(List.rev !suites)
