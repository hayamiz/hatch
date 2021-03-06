(* Data structure and functions for syntax tree *)

type egg_expr =
    ExpNop                                (* no op *)
  | ExpLiteral of egg_literal
  | ExpLambda of Symbol.sym list * egg_expr (* TODO: fix param list *)
  | ExpApply   of egg_expr * egg_expr list
  | ExpBind    of Symbol.sym * egg_expr
  | ExpLet     of Symbol.sym * egg_expr * egg_expr
  | ExpPrefix  of egg_prefix_oper * egg_expr
  | ExpInfix   of egg_infix_oper * egg_expr * egg_expr
  | ExpSeq     of egg_expr list (* compound and block expression *)
  | ExpIf      of egg_expr * egg_expr * egg_expr

and egg_prefix_oper =
    PrefixPlus
  | PrefixMinus
  | PrefixLnot

and egg_infix_oper =
    InfixPlus
  | InfixMinus
  | InfixMul
  | InfixDiv
  | InfixEq
  | InfixLe
  | InfixGe
  | InfixLt
  | InfixGt
  | InfixLand
  | InfixLor

and egg_literal = 
    LitIdent of Symbol.sym
  | LitInt of (int)
  | LitFloat of (float)
  | LitString of (string)
  | LitBool of (bool)
  | LitUndef
