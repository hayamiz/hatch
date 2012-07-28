
open Syntax
open Symbol
open Normal
open ParserUtil

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
  | LLClsApply of sym (* cls *)  * sym list (* arguments *)
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

let rec string_of_ll_expr ?(indent = 0) e =
  let is = String.make indent ' '
  in
  match e with
	| LLVar v -> is ^ "Var: " ^ v
	| LLInt i -> is ^ "Int: " ^ (string_of_int i)
	| LLFloat f -> is ^ "Float: " ^ (string_of_float f)
	| LLString s -> is ^ "String: " ^ s
	| LLBool b -> is ^ "Bool: " ^ (string_of_bool b)
	| LLUndef -> is ^ "Undef"
	| LLMakeCls (f, fvar_vals) ->
		is ^ "MakeCls: " ^ f ^ "(" ^ (String.concat ", " fvar_vals) ^ ")"
	| LLFunApply (f, args) ->
		is ^ "FunApply: " ^ f ^ "(" ^ (String.concat "," args) ^ ")"
	| LLClsApply (f, args) ->
		is ^ "ClsApply: " ^ f ^ "(" ^ (String.concat "," args) ^ ")"
	| LLBind (id, v) ->
		is ^ "Bind: " ^ id ^ " ->\n" ^
		  (string_of_ll_expr v ~indent:(indent+6))
	| LLLet (id, v, body) ->
		is ^ "Let " ^ id ^ " ->\n" ^
		  (string_of_ll_expr v ~indent:(indent+4)) ^ "\n" ^
		  (string_of_ll_expr body ~indent:(indent+2))
	| LLPrefix (op, e) ->
		is ^ "Prefix: " ^ (string_of_prefix_oper op) ^ " " ^ e ^ "\n"
	| LLInfix (op, e1, e2) ->
		is ^ "Infix: " ^ e1 ^ " " ^ (string_of_infix_oper op) ^ " " ^ e2
	| LLIf  (cond, if_clause, else_clause) ->
		is ^ "If: " ^ cond ^ "\n" ^
		  (string_of_ll_expr if_clause ~indent:(indent+2)) ^ "\n" ^
		  (string_of_ll_expr else_clause ~indent:(indent+2))
;;

let string_of_ll_fundef f =
  match f with
	  LLFlatFun (fname, params, body) ->
		"  FlatFun " ^ fname ^ " (" ^ (String.concat ", " params) ^ "):\n" ^
		  (string_of_ll_expr ~indent:4 body)
	| LLClsFun (fname, params, fvars, body) ->
		"  ClsFun " ^ fname ^ " fvars:(" ^ (String.concat ", " fvars) ^ ") (" ^
		  (String.concat ", " params) ^ "):\n" ^
		  string_of_ll_expr ~indent:4 body


let string_of_ll_program llp =
  "Lambda-Lifted program:\n" ^
	" Functions:\n" ^ (String.concat "\n" (List.map string_of_ll_fundef llp.funcs)) ^ "\n" ^
	" Main expr:\n" ^
	(string_of_ll_expr ~indent:2 llp.main)

let rec replace_symbols mapping lle =
  let findsym sym =
	if Smap.mem sym mapping then
	  Smap.find sym mapping
	else
	  sym
  in
	match lle with
		LLVar sym -> LLVar (findsym sym)
	  | LLMakeCls (fsym, argsyms) ->
		  LLMakeCls (findsym fsym, List.map findsym argsyms)
	  | LLFunApply (fsym, argsyms) ->
		  LLFunApply (findsym fsym, List.map findsym argsyms)
	  | LLBind (s, e) ->
		  LLBind (s, replace_symbols mapping e)
	  | LLLet (sym, v, body) ->
		  LLLet (sym, replace_symbols mapping v, replace_symbols mapping body)
	  | LLPrefix (op, sym) ->
		  LLPrefix (op, findsym sym)
	  | LLInfix (op, s1, s2) ->
		  LLInfix (op, findsym s1, findsym s2)
	  | LLIf (sym, e1, e2) ->
		  LLIf (findsym sym, replace_symbols mapping e1, replace_symbols mapping e2)
	  | _ -> lle

let rec do_lambda_lift env funcs nexp =
  let simple_ll_program e =
	{ funcs = funcs;
	  main = e}
  in
  let make_ll_program newfuncs e =
	{ funcs = newfuncs;
	  main = e}
  in
  match nexp with
	  NexpVar sym -> simple_ll_program (LLVar sym)
	| NexpInt x -> simple_ll_program (LLInt x)
	| NexpFloat x -> simple_ll_program (LLFloat x)
	| NexpString x -> simple_ll_program (LLString x)
	| NexpBool x -> simple_ll_program (LLBool x)
	| NexpUndef -> simple_ll_program LLUndef
	| NexpLambda (params, body) ->
		let param_env = Sset.sset_of_list params in
		let env' = Sset.union env param_env in
		let fvars = Normal.freevars param_env body in
		let { funcs = funcs'; main = body' } = do_lambda_lift env' funcs body in
		let new_funname = gensym () in
		  begin
			match (Sset.elements (Sset.inter fvars env)) with
				[] ->
				  let new_fundef = LLFlatFun (new_funname, params, body') in
					make_ll_program (new_fundef :: funcs') (LLVar new_funname)
			  | fvars' ->
				  let (newfvars, fvar_mapping) =
					List.fold_left (fun (newfvs, fvmap) fv ->
									  let newfv = gensym () in
										(newfv :: newfvs, Smap.add fv newfv fvmap))
					  ([], Smap.empty) fvars'
				  in
				  let newfvars = List.rev newfvars in
				  let body'' = replace_symbols fvar_mapping body' in
				  let new_fundef = LLClsFun (new_funname, newfvars, params, body'') in
					make_ll_program (new_fundef :: funcs') (LLMakeCls (new_funname, fvars'))
		  end
	| NexpApply (f, args) ->
		simple_ll_program (LLFunApply (f, args))
	| NexpBind (var, value) ->
		let {funcs = funcs'; main = value' } =
		  do_lambda_lift env funcs value
		in
		  make_ll_program funcs' (LLBind (var, value'))
	| NexpLet (var, value, body) ->
		let {funcs = funcs'; main = value' } =
		  do_lambda_lift env funcs value
		in
		let env' =
		  Sset.add var env
		in
		let {funcs = funcs''; main = body' } =
		  do_lambda_lift env' funcs' body
		in
		  make_ll_program funcs'' (LLLet (var, value', body'))
	| NexpPrefix (op, v) -> simple_ll_program (LLPrefix (op, v))
	| NexpInfix (op, v1, v2) -> simple_ll_program (LLInfix (op, v1, v2))
	| NexpIf (cond, if_body, else_body) ->
		let {funcs = funcs'; main = if_body' } =
		  do_lambda_lift env funcs if_body
		in
		let {funcs = funcs''; main = else_body' } =
		  do_lambda_lift env funcs' else_body
		in
		  make_ll_program funcs'' (LLIf (cond, if_body', else_body'))

let lambda_lift nexp =
  do_lambda_lift Sset.empty [] nexp
