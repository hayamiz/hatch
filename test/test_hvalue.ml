
(* test of hvalue.ml *)

open Testutil
open Hvalue
open Hnative

let test_h_make_sym _ =
  assert_equal (h_make_sym ("f" ^ "oo")) (h_make_sym ("fo" ^ "o"));
  assert_equal false ((h_make_sym "foo") == (h_make_sym "bar"));
  ()

let test_hval_h_is_true _ =
  assert_equal false (h_is_true HV_undef);
  assert_equal false (h_is_true (HV_bool false));

  assert_equal true (h_is_true (HV_bool true));
  assert_equal true (h_is_true (HV_int 0));
  assert_equal true (h_is_true (HV_int 1));
  assert_equal true (h_is_true (HV_int (-1)));
  assert_equal true (h_is_true (HV_float 0.0));
  assert_equal true (h_is_true (HV_float 1.0));
  assert_equal true (h_is_true (HV_float (-1.0)));
  assert_equal true (h_is_true (HV_function (HF_user ("hogefun", 1, 1))));
  assert_equal true (h_is_true (HV_function (HF_native ("print", h_native_print))));
  assert_equal true (h_is_true (HV_closure (HF_user ("hogefun", 1, 1), [HV_int 1])));
  ()

let test_hval_h_add _ =
  assert_equal (HV_int 3) (h_add (HV_int 1) (HV_int 2));
  assert_equal (HV_float 3.0) (h_add (HV_int 1) (HV_float 2.0));
  assert_equal (HV_float 3.0) (h_add (HV_float 2.0) (HV_int 1));

  assert_equal (HV_string "foobar") (h_add (HV_string "foo") (HV_string "bar"));

  assert_equal HV_undef (h_add (HV_string "foo") (HV_int 1));
  assert_equal HV_undef (h_add (HV_string "foo") (HV_float 1.0));
  ()

let test_hval_h_mul _ =
  assert_equal (HV_int 2) (h_mul (HV_int 1) (HV_int 2));
  assert_equal (HV_float 2.0) (h_mul (HV_int 1) (HV_float 2.0));
  assert_equal (HV_float 2.0) (h_mul (HV_float 2.0) (HV_int 1));

  assert_equal (HV_string "foofoofoo") (h_mul (HV_string "foo") (HV_int 3));

  assert_equal HV_undef (h_mul (HV_string "foo") (HV_float 1.0));
  ()

let test_hval_h_sub _ =
  assert_equal (HV_int (-1)) (h_sub (HV_int 1) (HV_int 2));
  assert_equal (HV_float (-1.0)) (h_sub (HV_int 1) (HV_float 2.0));
  assert_equal (HV_float 1.0) (h_sub (HV_float 2.0) (HV_int 1));

  assert_equal HV_undef (h_sub (HV_string "foo") (HV_float 1.0));
  ()

let test_hval_h_div _ =
  assert_equal (HV_int 0) (h_div (HV_int 1) (HV_int 2));
  assert_equal (HV_int 2) (h_div (HV_int 5) (HV_int 2));
  assert_equal (HV_int 3) (h_div (HV_int 6) (HV_int 2));
  assert_equal (HV_float 0.5) (h_div (HV_int 1) (HV_float 2.0));
  assert_equal (HV_float 2.0) (h_div (HV_float 2.0) (HV_int 1));

  assert_equal HV_undef (h_div (HV_string "foo") (HV_float 1.0));
  ()

let add_suites _ =
  add_suites begin "Hvalue" >::: [
	"h_make_sym" >:: test_h_make_sym;
	"hval_h_is_true" >:: test_hval_h_is_true;
	"hval_h_add" >:: test_hval_h_add;
	"hval_h_mul" >:: test_hval_h_mul;
	"hval_h_sub" >:: test_hval_h_sub;
	"hval_h_div" >:: test_hval_h_div;
  ] end
;;
