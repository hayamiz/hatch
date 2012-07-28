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
		(Array.to_list expected) (Array.to_list (compile (Highlevel.compile llprog)))
	with
		_ as ex ->
		  begin
			print_string msg;
			raise ex
		  end
;;

let test_vm_compile_simple _ =
  assert_eq_vminsts
	[|VM_GOTO 1;
	  VM_GREF_PUSH "x";
	  VM_HALT|]
	({ funcs = []; main = LLVar "x" });

  assert_eq_vminsts
	[|VM_GOTO 1;
	  VM_PUSH (HV_int 1);
	  VM_HALT|]
	({ funcs = [];
	   main = (LLInt 1) });

  assert_eq_vminsts
	[|VM_GOTO 1;
	  VM_PUSH (HV_string "piyo");
	  VM_HALT|]
	({ funcs = [];
	   main = (LLString "piyo") });

  assert_eq_vminsts
	[|VM_GOTO 1;
	  VM_PUSH_FRAME;
	  VM_GREF_PUSH "arg1";
	  VM_GREF_PUSH "arg2";
	  VM_GREF_PUSH "somefunc";
	  VM_CALL;
	  VM_HALT|]
	({ funcs = [];
	   main = (LLFunApply ("somefunc", ["arg1"; "arg2"])) });

  assert_eq_vminsts
	[|VM_GOTO 1;
	  VM_PUSH HV_undef;
	  VM_PUSH HV_undef;
	  VM_PUSH (HV_int 1);
	  VM_LSET 0;
	  VM_PUSH (HV_int 4);
	  VM_LSET 1;
	  VM_LREF_PUSH 0;
	  VM_LREF_PUSH 1;
	  VM_ADD;
	  VM_HALT|]
	({ funcs = [];
	   main = (LLLet ("sym#x", LLInt 1,
					  (LLLet ("sym#y", LLInt 4,
							  LLInfix (InfixPlus, "sym#x", "sym#y"))))) });
  ()

let test_vm_ifexpr _ =
  assert_eq_vminsts
	[|VM_GOTO 1;
	  VM_PUSH HV_undef;
	  VM_PUSH (HV_int 1);
	  VM_LSET 0;
	  VM_LREF_PUSH 0;
	  VM_BIFN 8;
	  VM_PUSH (HV_string "if-clause");
	  VM_GOTO 9;
	  VM_PUSH (HV_string "else-clause");
	  VM_HALT|]
	({ funcs = [];
	   main = (LLLet ("sym#x", LLInt 1,
					  (LLIf ("sym#x", LLString "if-clause", LLString "else-clause"))))});
  ()

let test_vm_compile_flatfun _ =
  assert_eq_vminsts
	[|VM_GOTO 3;
	  VM_LREF_PUSH 0;
	  VM_RET;
	  VM_PUSH HV_undef;
	  VM_PUSH (HV_function (HF_user ("sym#fun1", 1, 1)));
	  VM_LSET 0;
	  VM_PUSH HV_undef;
	  VM_HALT|]
	({ funcs = [LLFlatFun ("sym#fun1", ["hoge"],
						   LLVar "hoge")];
	   main =  (LLLet ("x",
					   LLVar "sym#fun1",
					   LLUndef))});
  ()

let test_vm_compile_clsfun _ =
  assert_eq_vminsts
	[|VM_GOTO 5;
	  VM_LREF_PUSH 0;
	  VM_LREF_PUSH 1;
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
	  VM_HALT|]
	({ funcs = [LLClsFun ("sym#fun1", ["sym#arg1"], ["hoge"],
						  LLInfix (InfixPlus, "sym#arg1", "hoge"))];
	   main =  (LLLet ("x",
					   LLInt 1,
					   LLMakeCls ("sym#fun1", ["x"])))});

  assert_eq_vminsts
	[|VM_GOTO 5;
	  VM_LREF_PUSH 0;
	  VM_LREF_PUSH 1;
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
	  VM_CALL_CLS;
	  VM_HALT|]
	({ funcs = [LLClsFun ("sym#add1", ["sym#arg1"], ["hoge"],
						  LLInfix (InfixPlus, "sym#arg1", "hoge"))];
	   main =  (LLLet ("x",
					   LLInt 1,
					   LLLet ("f", LLMakeCls ("sym#add1", ["x"]),
							  LLLet ("y", LLInt 2,
									 LLClsApply ("f", ["y"])))))});
  ()

let add_suites _ =
  add_suites begin "Vm" >::: [
	"vm_compile_simple" >:: test_vm_compile_simple;
	"vm_ifexpr" >:: test_vm_ifexpr;
	"vm_compile_flatfun" >:: test_vm_compile_flatfun;
	"vm_compile_clsfun" >:: test_vm_compile_clsfun;
  ] end
;;

