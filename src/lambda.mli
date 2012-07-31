
(* Lambda Lifting *)

open Syntax
open Symbol


(* ll_expr: lambda-lifted normalized expression *)
type ll_expr =
	LLVar of sym
  | LLInt of int
  | LLFloat of float
  | LLString of string
  | LLBool of bool
  | LLUndef
  | LLMakeCls of sym (* func *) * sym list (* fvar values *)
  | LLFunApply of sym (* func *) * sym list (* arguments *)
  | LLBind of sym (* variable *) * ll_expr (* value *)
  | LLLet of sym (* variable *) * ll_expr (* value *) * ll_expr (* body *)
  | LLPrefix of egg_prefix_oper * sym
  | LLInfix of egg_infix_oper * sym * sym
  | LLIf of sym (* condition *) * ll_expr * ll_expr

and ll_fundef =
  LLFlatFun of sym (* fun name *) * sym list (* params *) * ll_expr
| LLClsFun of sym  (* fun name *) * sym list (* free variables *) * sym list (* params *) * ll_expr

and ll_program = {
  funcs: ll_fundef list;
  main: ll_expr;
}

val string_of_ll_program: ll_program -> string
val lift_lambda: Normal.normal_expr -> ll_program
