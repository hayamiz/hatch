
open Normal

let do_const_fold imap nexp =
  match nexp with
	  NexpVar id ->
		nexp
	| NexpInt _
	| NexpFloat _
	| NexpString _
	| NexpBool _
	| NexpUndef ->
		nexp
	| NexpLambda (params, body) ->
		nexp
	| NexpApply (f, args) ->
		nexp
	| NexpBind (id, v)  ->
		nexp
	| NexpLet (id, v, body)  ->
		nexp
	| NexpPrefix (op, v)  ->
		nexp
	| NexpInfix (op, v1, v2) ->
		nexp
	| NexpIf (cond, if_body, else_body) ->
		nexp

let const_fold nexp =
  do_const_fold Smap.empty nexp
