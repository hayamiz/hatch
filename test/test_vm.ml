(* test of vm.ml *)

open Testutil
open Vm

let assert_eq_vminsts expected llprog =
  let msg = "Compiling: " ^ (string_of_ll_program llprog) ^ "\n"
  in
  try
    assert_equal ~msg:msg
      ~cmp:vminsts_equal
      ~printer:(fun insts -> "\n" ^ (string_of_vminsts insts) ^ "\n")
      expected (compile (Highlevel.compile llprog))
  with
    _ as ex ->
      begin
        print_string msg;
        raise ex
      end
;;

let test_vm_compile_simple _ =
  assert_eq_vminsts
    [VM_GOTO 1;
     VM_GREF_PUSH "x";
     VM_HALT]
    ({ funcs = []; main = LLVar "x" });

  assert_eq_vminsts
    [VM_GOTO 1;
     VM_PUSH (HV_int 1);
     VM_HALT]
    ({ funcs = [];
       main = (LLInt 1) });

  assert_eq_vminsts
    [VM_GOTO 1;
     VM_PUSH (HV_string "piyo");
     VM_HALT]
    ({ funcs = [];
       main = (LLString "piyo") });

  assert_eq_vminsts
    [VM_GOTO 1;
     VM_PUSH_FRAME;
     VM_GREF_PUSH "arg1";
     VM_GREF_PUSH "arg2";
     VM_GREF_PUSH "somefunc";
     VM_CALL;
     VM_HALT]
    ({ funcs = [];
       main = (LLFunApply ("somefunc", ["arg1"; "arg2"])) });

  assert_eq_vminsts
    [VM_GOTO 1;
     VM_PUSH HV_undef;
     VM_PUSH HV_undef;
     VM_PUSH (HV_int 1);
     VM_LSET 0;
     VM_PUSH (HV_int 4);
     VM_LSET 1;
     VM_LREF_PUSH 0;
     VM_LREF_PUSH 1;
     VM_ADD;
     VM_HALT]
    ({ funcs = [];
       main = (LLLet ("sym#x", LLInt 1,
                      (LLLet ("sym#y", LLInt 4,
                              LLInfix (InfixPlus, "sym#x", "sym#y"))))) });
  ()

let test_vm_ifexpr _ =
  assert_eq_vminsts
    [VM_GOTO 1;
     VM_PUSH HV_undef;
     VM_PUSH (HV_int 1);
     VM_LSET 0;
     VM_LREF_PUSH 0;
     VM_BIFN 8;
     VM_PUSH (HV_string "if-clause");
     VM_GOTO 9;
     VM_PUSH (HV_string "else-clause");
     VM_HALT]
    ({ funcs = [];
       main = (LLLet ("sym#x", LLInt 1,
                      (LLIf ("sym#x", LLString "if-clause", LLString "else-clause"))))});
  ()

let test_vm_compile_bind _ =
  assert_eq_vminsts
    [VM_GOTO 1;
     VM_PUSH (HV_int 1);
     VM_DUP;
     VM_GSET "x";
     VM_HALT;]
    ({ funcs = []; main = LLBind ("x", LLInt 1) });
  ()

let test_vm_compile_flatfun _ =
  assert_eq_vminsts
    [VM_GOTO 3;
     VM_LREF_PUSH 0;
     VM_RET;
     VM_PUSH HV_undef;
     VM_PUSH (HV_function (HF_user ("sym#fun1", 1, 1)));
     VM_LSET 0;
     VM_PUSH HV_undef;
     VM_HALT]
    ({ funcs = [LLFlatFun ("sym#fun1", ["hoge"],
                           LLVar "hoge")];
       main =  (LLLet ("x",
                       LLVar "sym#fun1",
                       LLUndef))});
  ()

let test_vm_compile_clsfun _ =
  assert_eq_vminsts
    [VM_GOTO 5;
     VM_LREF_PUSH 1;
     VM_LREF_PUSH 0;
     VM_ADD;
     VM_RET;
     VM_PUSH HV_undef;
     VM_PUSH (HV_int 1);
     VM_LSET 0;
     VM_PUSH_FRAME;
     VM_PUSH (HV_function (HF_user ("sym#fun1", 1, 2)));
     VM_LREF_PUSH 0;
     VM_PUSH (h_native_get "make_closure");
     VM_CALL;
     VM_HALT]
    ({ funcs = [LLClsFun ("sym#fun1", ["sym#arg1"], ["hoge"],
                          LLInfix (InfixPlus, "sym#arg1", "hoge"))];
       main =  (LLLet ("x",
                       LLInt 1,
                       LLMakeCls ("sym#fun1", ["x"])))});

  assert_eq_vminsts
    [VM_GOTO 5;
     VM_LREF_PUSH 1;
     VM_LREF_PUSH 0;
     VM_ADD;
     VM_RET;
     VM_PUSH HV_undef;
     VM_PUSH HV_undef;
     VM_PUSH HV_undef;
     VM_PUSH (HV_int 1);
     VM_LSET 0;
     VM_PUSH_FRAME;
     VM_PUSH (HV_function (HF_user ("sym#add1", 1, 2)));
     VM_LREF_PUSH 0;
     VM_PUSH (h_native_get "make_closure");
     VM_CALL;
     VM_LSET 1;
     VM_PUSH (HV_int 2);
     VM_LSET 2;
     VM_PUSH_FRAME;
     VM_LREF_PUSH 2;
     VM_LREF_PUSH 1;
     VM_CALL;
     VM_HALT]
    ({ funcs = [LLClsFun ("sym#add1", ["sym#arg1"], ["hoge"],
                          LLInfix (InfixPlus, "sym#arg1", "hoge"))];
       main =  (LLLet ("x",
                       LLInt 1,
                       LLLet ("f", LLMakeCls ("sym#add1", ["x"]),
                              LLLet ("y", LLInt 2,
                                     LLFunApply ("f", ["y"])))))});

  assert_eq_vminsts
    [VM_GOTO 5;
     VM_LREF_PUSH 0;
     VM_LREF_PUSH 1;
     VM_SUB;
     VM_RET;
     VM_PUSH HV_undef;
     VM_PUSH HV_undef;
     VM_PUSH HV_undef;
     VM_PUSH (HV_int 1);
     VM_LSET 0;
     VM_PUSH_FRAME;
     VM_PUSH (HV_function (HF_user ("sym#sub1", 1, 2)));
     VM_LREF_PUSH 0;
     VM_PUSH (h_native_get "make_closure");
     VM_CALL;
     VM_LSET 1;
     VM_PUSH (HV_int 2);
     VM_LSET 2;
     VM_PUSH_FRAME;
     VM_LREF_PUSH 2;
     VM_LREF_PUSH 1;
     VM_CALL;
     VM_HALT]
    ({ funcs = [LLClsFun ("sym#sub1", ["sym#fv1"], ["arg"],
                          LLInfix (InfixMinus, "arg", "sym#fv1"))];
       main =  (LLLet ("x",
                       LLInt 1,
                       LLLet ("f", LLMakeCls ("sym#sub1", ["x"]),
                              LLLet ("y", LLInt 2,
                                     LLFunApply ("f", ["y"])))))});
  ()

let assert_eq_run_vminsts expected str =
  let lexbuf = Lexing.from_string str in
  let msg = "Running:\n" ^ str ^ "\n=====\n" in
  try
    assert_equal
      ~msg:msg
      ~printer:string_of_h_value
      expected
      (run_vminsts (Translator.translate lexbuf))
  with
    _ as ex ->
      begin
        print_string msg;
        raise ex
      end

let test_vm_run_simple _ =
  assert_eq_run_vminsts (HV_int 3) "1 - -2";
  assert_eq_run_vminsts (HV_int (-1)) "1 + -2";
  assert_eq_run_vminsts (HV_int 3) "1 + 2";
  assert_eq_run_vminsts (HV_string "foo") "\"foo\"";
  assert_eq_run_vminsts (HV_undef) "print(\"hello world\")";
  assert_eq_run_vminsts (HV_int 3) "identity(1+2)";
  ()

let test_vm_run_bind _ =
  assert_eq_run_vminsts (HV_int 123) "bind x -> 123; x";
  ()

let test_vm_run_bindfun _ =
  assert_eq_run_vminsts (HV_int 2) "bind fib -> lambda (n) { n }; fib(2);";
  assert_eq_run_vminsts (HV_int 2) "bind fib -> lambda (n) { n }; identity(fib(2));";
  ()

let test_vm_run_if _ =
  assert_eq_run_vminsts (HV_int 3) "if (1 < 2) 3 else 4";
  assert_eq_run_vminsts (HV_int 4) "if (1 > 2) 3 else 4";
  ()

let test_vm_run_userfun _ =
  assert_eq_run_vminsts (HV_int 3) "(lambda (x) { x+1 })(2)";
  assert_eq_run_vminsts (HV_int 3) "let x -> 2 in (lambda (x) { x+1 })(x)";
  ()

let test_vm_run_closure _ =
  assert_eq_run_vminsts (HV_int 3) "let x -> 1 in let add1 -> (lambda (y) { x + y }) in add1(2)";
  ()

let test_vm_run_fib _ =
  assert_eq_run_vminsts (HV_int 55) "bind fib->lambda(n){if(n<=0){0}else if(n=1){1}else{fib(n-1)+fib(n-2)}};fib(10);";
  ()

let add_suites _ =
  add_suites begin "Vm" >::: [
    "vm_compile_simple" >:: test_vm_compile_simple;
    "vm_ifexpr" >:: test_vm_ifexpr;
    "vm_compile_bind" >:: test_vm_compile_bind;
    "vm_compile_flatfun" >:: test_vm_compile_flatfun;
    "vm_compile_clsfun" >:: test_vm_compile_clsfun;
    "vm_run_simple" >:: test_vm_run_simple;
    "vm_run_bind" >:: test_vm_run_bind;
    "vm_run_bindfun" >:: test_vm_run_bindfun;
    "vm_run_if" >:: test_vm_run_if;
    "vm_run_userfun" >:: test_vm_run_userfun;
    "vm_run_closure" >:: test_vm_run_closure;
    "vm_run_fib" >:: test_vm_run_fib;
  ] end
;;

