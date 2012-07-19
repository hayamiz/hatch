
type location = {
  file: string;						(* filename ("(none)" if not available) *)
  line: int;							(* line number *)
  offset: int;						(* offset from the beginning of the line *)
  byte: int;							(* byte position from the beginning of the file *)
}

type egg_expr =
	ExpNop								(* no op *)
  | ExpLiteral of (egg_literal * location)
  | ExpClosure of (string list * egg_expr * location) (* TODO: fix param list *)
  | ExpApply   of (egg_expr * egg_expr list * location)
  | ExpBind    of (string * egg_expr * location)
  | ExpPrefix  of (egg_prefix_oper * egg_expr * location)
  | ExpInfix   of (egg_infix_oper * egg_expr * egg_expr * location)
  | ExpSeq     of (egg_expr list * location) (* compound and block expression *)
  | ExpIf      of (egg_expr * egg_expr * egg_expr * location)

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
	LitIdent of (string)
  | LitInt of (int)
  | LitFloat of (float)
  | LitString of (string)
  | LitBool of (bool)
  | LitUndef


let noloc = {
  file = "(none)";
  line = 0;
  offset = 0;
  byte = 0;
}

let current_file = ref "(none)"