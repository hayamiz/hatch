
(* Data structure and functions for syntax tree *)

type location = {
  file: string;						(* filename ("(none)" if not available) *)
  line: int;							(* line number *)
  offset: int;						(* offset from the beginning of the line *)
  byte: int;							(* byte position from the beginning of the file *)
}

val noloc: location

val current_file: string ref

type egg_expr =
	ExpLiteral of (egg_literal * location)
  | ExpClosure of (string list * egg_expr * location) (* TODO: fix param list *)
  | ExpApply   of (egg_expr * egg_expr list * location)
  | ExpBind    of (string * egg_expr * location)
  (* | ExpPrefix  of () *)
  (* | ExpInfix   of () *)
  | ExpSeq     of (egg_expr list * location) (* compound and block expression *)
  | ExpIf      of (egg_if_expr * location)

and egg_if_expr =
	IfExpIf   of (egg_expr * egg_expr * egg_if_expr)	(* if and elseif *)
  | IfExpElse   of (egg_expr)

and egg_literal = 
	LitIdent of (string)
  | LitInt of (int)
  | LitFloat of (float)
  | LitString of (string)
  | LitBool of (bool)
  | LitUndef
