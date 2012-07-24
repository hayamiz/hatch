
(* make K-normal form *)

open ParserUtil
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

let rec string_of_normal_expr ?(indent = 0) e =
  let is = String.make indent ' '
  in
  match e with
	  NexpVar v -> is ^ "Var: " ^ v
	| NexpInt i -> is ^ "Int: " ^ (string_of_int i)
	| NexpFloat f -> is ^ "Float: " ^ (string_of_float f)
	| NexpString s -> is ^ "String: " ^ s
	| NexpBool b -> is ^ "Bool: " ^ (string_of_bool b)
	| NexpUndef -> is ^ "Undef"
	| NexpLambda (params, body_expr) ->
		is ^ "Lambda (" ^ (String.concat "," params) ^ ") {\n" ^
		  (string_of_normal_expr body_expr ~indent:(indent+2)) ^ "\n" ^
		  is ^ "}"
	| NexpApply (f, args) ->
		is ^ "Apply: " ^ f ^ "(" ^ (String.concat "," args) ^ ")"
	| NexpBind (id, v) ->
		is ^ "Bind: " ^ id ^ " ->\n" ^
		  (string_of_normal_expr v ~indent:(indent+4)) ^ "\n"
	| NexpLet (id, v, body) ->
		is ^ "Let" ^ "\n" ^
		  is ^ "  " ^ id ^ " ->\n" ^
		  (string_of_normal_expr v ~indent:(indent+4)) ^ "\n" ^
		  (string_of_normal_expr body ~indent:(indent+2))
	| NexpPrefix (op, e) ->
		is ^ "Prefix: " ^ (string_of_prefix_oper op) ^ " " ^ e ^ "\n"
	| NexpInfix (op, e1, e2) ->
		is ^ "Infix: " ^ e1 ^ " " ^ (string_of_infix_oper op) ^ " " ^ e2
	| NexpIf  (cond, if_clause, else_clause) ->
		is ^ "If: " ^ cond ^ "\n" ^
		  (string_of_normal_expr if_clause ~indent:(indent+2)) ^ "\n" ^
		  (string_of_normal_expr else_clause ~indent:(indent+2))
;;

(* vに束縛される一時変数 tmp が作られて
   let tmp -> (norm env v)
   in
     body_f(tmp)
という式に正規化される。body_f の中の v の出現が tmp で置き換えられるよう、body_f を設計する
 *)
let make_let v body_f =
  match v with
	  NexpVar sym -> body_f sym
	| _ ->
		let tmp_sym = gensym () in
		  NexpLet (tmp_sym, v, (body_f tmp_sym))

let rec norm env exp =
  match exp with
	  ExpLiteral (LitIdent x) -> NexpVar x
	| ExpLiteral (LitInt x) -> NexpInt x
	| ExpLiteral (LitFloat x) -> NexpFloat x
	| ExpLiteral (LitBool x) -> NexpBool x
	| ExpLiteral (LitString x) -> NexpString x
	| ExpLiteral LitUndef -> NexpUndef
	| ExpLambda (params, body) ->
		NexpLambda (params, norm env body)
	| ExpApply (f, args) ->
		make_let (norm env f)
		  (fun f_sym ->
			 let rec norm_args args normed_args =
			   match args with
				   [] -> NexpApply(f_sym, (List.rev normed_args))
				 | arg :: args ->
					 make_let (norm env arg)
					   (fun arg_sym ->
						  norm_args args (arg_sym :: normed_args))
			 in
			   norm_args args [])
	| ExpBind (var, v) ->
		NexpBind (var, norm env v)
	| ExpLet (var, v, body) ->
		let env' = Smap.add var true env in
		  NexpLet (var,
				   norm env v,
				   norm env' body)
	| ExpPrefix (op, e) ->
		make_let (norm env e)
		  (fun tmpsym_e ->
			 NexpPrefix (op, tmpsym_e))
	| ExpInfix (op, e1, e2) ->
		make_let (norm env e1)
		  (fun e1_sym ->
			 make_let (norm env e2)
			   (fun e2_sym ->
				  NexpInfix (op, e1_sym, e2_sym)))
	| ExpSeq [] ->
		NexpUndef
	| ExpSeq es ->
		let rec norm_exprs exprs =
		  match exprs with
			  [] -> raise (Failure "norm ExpSeq: cannot reach here")
			| e :: [] ->
				norm env e
			| e :: rest ->
				NexpLet (gensym (),
						 (norm env e),
						 norm_exprs rest)
		in
		  norm_exprs es
	| ExpIf (cond, if_body, else_body) ->
		make_let (norm env cond)
		  (fun cond_sym ->
			 NexpIf (cond_sym, norm env if_body, norm env else_body))
	| ExpNop -> NexpUndef

let normalize exp =
  norm Smap.empty exp

let rec reduce_let nexp =
  match nexp with
	  NexpVar _
	| NexpInt _
	| NexpFloat _
	| NexpString _
	| NexpBool _
	| NexpUndef ->
		nexp
	| NexpLambda (params, body) ->
		NexpLambda (params, reduce_let body)
	| NexpApply _ ->
		nexp
	| NexpBind (id, v) ->
		NexpBind (id, reduce_let v)
	| NexpLet (id1, NexpLet (id2, v1, v2), v3) ->
		let reduced_v1 = reduce_let v1 in
		let reduced_v2 = reduce_let v2 in
		let reduced_v3 = reduce_let v3 in
		let new_nexp =
		  NexpLet (id2, reduced_v1,
				   NexpLet (id1, reduced_v2, reduced_v3))
		in
		  reduce_let new_nexp
	| NexpLet (id, v, body) ->
		NexpLet (id, reduce_let v, reduce_let body)
	| NexpPrefix _
	| NexpInfix _ ->
		nexp
	| NexpIf (cond, if_body, else_body) ->
		NexpIf (cond, reduce_let if_body, reduce_let else_body)

let rec normal_expr_equal ?(exact=false) expected actual =
  let comp_sym x y =
	if exact then
	  if (String.length x >= 4 && (String.sub x 0 4) = "sym#") &&
		(String.length y >= 4 && (String.sub y 0 4) = "sym#") then
		  true
	  else
		x = y
	else
	   true
  in
  let rec comp_syms ss1 ss2 =
	match (ss1, ss2) with
		([], []) -> true
	  | (_ :: _, [])
	  | ([], _ :: _) -> false
	  | (s1 :: ss1, s2 :: ss2) ->
		  if comp_sym s1 s2 then
			comp_syms ss1 ss2
		  else false
  in
	match (expected, actual) with
	  | (NexpVar x, NexpVar y) -> comp_sym x y
	  | (NexpInt x, NexpInt y) when x = y -> true
	  | (NexpFloat x, NexpFloat y) when x = y -> true
	  | (NexpString x, NexpString y) when x = y -> true
	  | (NexpBool x, NexpBool y) when x = y -> true
	  | (NexpUndef , NexpUndef ) -> true
	  | (NexpLambda (ps1, body1), NexpLambda (ps2, body2)) ->
		  (comp_syms ps1 ps2) && (normal_expr_equal ~exact:exact body1 body2)
	  | (NexpApply (f1, args1), NexpApply (f2, args2)) ->
		  (comp_sym f1 f2) &&
			(comp_syms args1 args2)
	  | (NexpBind (id1, v1), NexpBind (id2, v2)) ->
		  (comp_sym id1 id2) && (normal_expr_equal ~exact:exact v1 v2)
	  | (NexpLet (id1, v1, body1), NexpLet (id2, v2, body2)) ->
		  (comp_sym id1 id2) && 
			(normal_expr_equal ~exact:exact v1 v2) &&
			(normal_expr_equal ~exact:exact body1 body2)
	  | (NexpPrefix (op1, v1), NexpPrefix (op2, v2)) ->
		  (op1 = op2) && (comp_sym v1 v2)
	  | (NexpInfix (op1, lv1, rv1), NexpInfix (op2, lv2, rv2)) ->
		  (op1 = op2) && (comp_sym lv1 lv2) && (comp_sym rv1 rv2)
	  | (NexpIf (c1, if1, else1), NexpIf (c2, if2, else2)) -> 
		  (comp_sym c1 c2) && (normal_expr_equal ~exact:exact if1 if2) && (normal_expr_equal ~exact:exact else1 else2)
	  | _ ->
		  false

let rec freevars env nexp =
  let make_fvars env syms =
	List.fold_left (fun fv sym -> if Sset.mem sym env then fv else Sset.add sym fv)
	  Sset.empty syms
  in
	match nexp with
		NexpVar sym ->
		  make_fvars env [sym]
	  | NexpInt _
	  | NexpFloat _
	  | NexpString _
	  | NexpBool _
	  | NexpUndef ->
		  Sset.empty
	  | NexpLambda (params, body) ->
		  let env' = Sset.union (Sset.sset_of_list params) env in
			freevars env' body
	  | NexpApply (f, args) ->
		  let syms = f :: args in
			make_fvars env syms
	  | NexpBind (id, v) ->
		  freevars env v
	  | NexpLet (id, v, body) ->
		  let v_fvars = freevars env v in
		  let env' = Sset.add id env in
		  let body_fvars = freevars env' body in
			Sset.union v_fvars body_fvars
	  | NexpPrefix (op, sym) ->
		  make_fvars env [sym]
	  | NexpInfix (op, s1, s2) ->
		  make_fvars env [s1; s2;]
	  | NexpIf (cond, if_body, else_body) ->
		  Sset.union (make_fvars env [cond])
			(Sset.union (freevars env if_body)
			   (freevars env else_body))
