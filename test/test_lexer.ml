
open OUnit

let _ = run_test_tt_main begin "lexer.ml" >::: [
  "dummy" >:: begin fun () ->
    assert_equal 1 (0 + 1)
  end
] end
