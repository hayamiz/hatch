
open Normal

let rec do_beta_reduce env nexp =
  let findsym env sym =
	if Smap.mem sym env then
	  Smap.find sym env
	else
	  sym
  in
  let addsym env from_sym to_sym =
	let new_env = Smap.add from_sym to_sym env in
	  new_env
  in
  match nexp with
	  NexpVar id ->
		NexpVar (findsym env id)
	| NexpInt _
	| NexpFloat _
	| NexpString _
	| NexpBool _
	| NexpUndef ->
		nexp
	| NexpLambda (params, body) ->
		NexpLambda (List.map (fun s -> findsym env s) params,
					do_beta_reduce env body)
	| NexpApply (f, args) ->
		NexpApply (findsym env f,
				   List.map (fun s -> findsym env s) args)
	| NexpBind (id, v) ->
		NexpBind (id, do_beta_reduce env v)
	| NexpLet (id, NexpVar id', body) ->
		let env' = (addsym env id id') in
		  do_beta_reduce env' body
	| NexpLet (id, v, body) ->
		NexpLet (id, do_beta_reduce env v, do_beta_reduce env body)
	| NexpPrefix (op, v) ->
		NexpPrefix (op, findsym env v)
	| NexpInfix (op, v1, v2) ->
		NexpInfix (op, findsym env v1, findsym env v2)
	| NexpIf (cond, if_body, else_body) ->
		NexpIf (findsym env cond, do_beta_reduce env if_body, do_beta_reduce env else_body)


let beta_reduce exp =
  do_beta_reduce Smap.empty exp
