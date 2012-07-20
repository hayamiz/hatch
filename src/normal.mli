
(* exprssion in normalized form *)

open Syntax
open Symbol

type normal_expr =
	NexpVar of sym
  | NexpInt of int
  | NexpFloat of float
  | NexpString of string
  | NexpBool of bool
  | NexpUndef
  | NexpLambda of sym list (* parameters *) * normal_expr (* body *)
  | NexpApply of sym (* func *) * sym list (* arguments *)
  | NexpBind of sym (* variable *) * normal_expr (* value *)
  | NexpLet of sym (* variable *) * normal_expr (* value *) * normal_expr (* body *)
  | NexpPrefix of egg_prefix_oper * sym
  | NexpInfix of egg_infix_oper * sym * sym
  | NexpIf of sym (* condition *) * normal_expr * normal_expr

val string_of_normal_expr: ?indent:int -> normal_expr -> string
val normalize: Syntax.egg_expr -> normal_expr
val reduce_let: normal_expr -> normal_expr
