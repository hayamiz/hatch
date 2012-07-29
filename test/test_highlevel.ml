(* test of highlevel.ml *)

open Testutil
open Highlevel

let assert_eq_hlvminsts expected llprog =
  let msg = "Compiling: " ^ (string_of_ll_program llprog) ^ "\n"
  in
  try
    assert_equal ~msg:msg
      ~cmp:hlvminsts_equal
      ~printer:(fun insts -> "\n" ^ (string_of_hlvminsts insts) ^ "\n")
      expected (compile llprog)
  with
    _ as ex ->
      begin
        print_string msg;
        raise ex
      end
;;

let test_highlevel_compile_simple _ =
  assert_eq_hlvminsts
    [HL_GOTO "*MAIN*";
     HL_LABEL "*MAIN*";
     HL_GREF_PUSH "x";
     HL_HALT;]
    ({ funcs = []; main = LLVar "x" });

  assert_eq_hlvminsts
    [HL_GOTO "*MAIN*";
     HL_LABEL "*MAIN*";
     HL_PUSH (HV_int 1);
     HL_HALT]
    ({ funcs = [];
       main = (LLInt 1) });

  assert_eq_hlvminsts
    [HL_GOTO "*MAIN*";
     HL_LABEL "*MAIN*";
     HL_PUSH (HV_string "piyo");
     HL_HALT]
    ({ funcs = [];
       main = (LLString "piyo") });

  assert_eq_hlvminsts
    [HL_GOTO "*MAIN*";
     HL_LABEL "*MAIN*";
     HL_PUSH_FRAME;
     HL_GREF_PUSH "arg1";
     HL_GREF_PUSH "arg2";
     HL_GREF_PUSH "somefunc";
     HL_CALL;
     HL_HALT;]
    ({ funcs = [];
       main = (LLFunApply ("somefunc", ["arg1"; "arg2"])) });

  assert_eq_hlvminsts
    [HL_GOTO "*MAIN*";
     HL_LABEL "*MAIN*";
     HL_PUSH HV_undef;
     HL_PUSH HV_undef;
     HL_PUSH (HV_int 1);
     HL_LSET 0;
     HL_PUSH (HV_int 4);
     HL_LSET 1;
     HL_LREF_PUSH 0;
     HL_LREF_PUSH 1;
     HL_ADD;
     HL_HALT]
    ({ funcs = [];
       main = (LLLet ("sym#x", LLInt 1,
                      (LLLet ("sym#y", LLInt 4,
                              LLInfix (InfixPlus, "sym#x", "sym#y"))))) });
  ()

let test_highlevel_ifexpr _ =
  assert_eq_hlvminsts
    [HL_GOTO "*MAIN*";
     HL_LABEL "*MAIN*";
     HL_PUSH HV_undef;
     HL_PUSH (HV_int 1);
     HL_LSET 0;
     HL_LREF_PUSH 0;
     HL_BIFN "sym#else";
     HL_PUSH (HV_string "if-clause");
     HL_GOTO "sym#fin";
     HL_LABEL "sym#else";
     HL_PUSH (HV_string "else-clause");
     HL_LABEL "sym#fin";
     HL_HALT]
    ({ funcs = [];
       main = (LLLet ("sym#x", LLInt 1,
                      (LLIf ("sym#x", LLString "if-clause", LLString "else-clause"))))});
  ()

let test_highlevel_compile_bind _ =
  assert_eq_hlvminsts
    [HL_GOTO "*MAIN*";
     HL_LABEL "*MAIN*";
     HL_PUSH (HV_int 1);
     HL_DUP;
     HL_GSET "x";
     HL_HALT;]
    ({ funcs = []; main = LLBind ("x", LLInt 1) });
  ()

let test_highlevel_compile_flatfun _ =
  assert_eq_hlvminsts
    [HL_GOTO "*MAIN*";
     HL_LABEL "sym#fun1";
     HL_LREF_PUSH 0;
     HL_RET;
     HL_LABEL "*MAIN*";
     HL_PUSH HV_undef;
     HL_PUSH (HV_function (HF_user_highlevel ("sym#fun1", "sym#fun1", 1)));
     HL_LSET 0;
     HL_PUSH HV_undef;
     HL_HALT]
    ({ funcs = [LLFlatFun ("sym#fun1", ["hoge"],
                           LLVar "hoge")];
       main =  (LLLet ("x",
                       LLVar "sym#fun1",
                       LLUndef))});
  ()

let test_highlevel_compile_clsfun _ =
  assert_eq_hlvminsts
    [HL_GOTO "*MAIN*";
     HL_LABEL "sym#fun1";
     HL_LREF_PUSH 0;
     HL_LREF_PUSH 1;
     HL_ADD;
     HL_RET;
     HL_LABEL "*MAIN*";
     HL_PUSH HV_undef;
     HL_PUSH (HV_int 1);
     HL_LSET 0;
     HL_PUSH_FRAME;
     HL_PUSH (HV_function (HF_user_highlevel ("sym#fun1", "sym#fun1", 2)));
     HL_LREF_PUSH 0;
     HL_PUSH (h_native_get "make_closure");
     HL_CALL;
     HL_HALT]
    ({ funcs = [LLClsFun ("sym#fun1", ["sym#arg1"], ["hoge"],
                          LLInfix (InfixPlus, "sym#arg1", "hoge"))];
       main =  (LLLet ("x",
                       LLInt 1,
                       LLMakeCls ("sym#fun1", ["x"])))});

  assert_eq_hlvminsts
    [HL_GOTO "*MAIN*";
     HL_LABEL "sym#fun1";
     HL_LREF_PUSH 0;
     HL_LREF_PUSH 1;
     HL_ADD;
     HL_RET;
     HL_LABEL "*MAIN*";
     HL_PUSH HV_undef;
     HL_PUSH HV_undef;
     HL_PUSH HV_undef;
     HL_PUSH (HV_int 1);
     HL_LSET 0;
     HL_PUSH_FRAME;
     HL_PUSH (HV_function (HF_user_highlevel ("sym#add1", "sym#add1", 2)));
     HL_LREF_PUSH 0;
     HL_PUSH (h_native_get "make_closure");
     HL_CALL;
     HL_LSET 1;
     HL_PUSH (HV_int 2);
     HL_LSET 2;
     HL_PUSH_FRAME;
     HL_LREF_PUSH 2;
     HL_LREF_PUSH 1;
     HL_CALL;
     HL_HALT]
    ({ funcs = [LLClsFun ("sym#add1", ["sym#arg1"], ["hoge"],
                          LLInfix (InfixPlus, "sym#arg1", "hoge"))];
       main =  (LLLet ("x",
                       LLInt 1,
                       LLLet ("f", LLMakeCls ("sym#add1", ["x"]),
                              LLLet ("y", LLInt 2,
                                     LLFunApply ("f", ["y"])))))});
  ()

let add_suites _ =
  add_suites begin "Highlevel" >::: [
    "highlevel_compile_simple" >:: test_highlevel_compile_simple;
    "highlevel_ifexpr" >:: test_highlevel_ifexpr;
    "highlevel_compile_bind" >:: test_highlevel_compile_bind;
    "highlevel_compile_flatfun" >:: test_highlevel_compile_flatfun;
    "highlevel_compile_clsfun" >:: test_highlevel_compile_clsfun;
  ] end
;;

