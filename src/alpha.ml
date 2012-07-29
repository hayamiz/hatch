
open Normal

let rec do_alpha_convert env nexp =
  let findsym env sym =
    if Smap.mem sym env then
      Smap.find sym env
    else
      sym
  in
  let rec addsyms env syms =
    match syms with
      [] -> env
    | s :: syms ->
      let new_sym = Symbol.gensym ~basesym:s () in
      let new_env = Smap.add s new_sym env in
      addsyms new_env syms
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
    let env' = addsyms env params in
    NexpLambda (List.map (fun s -> findsym env' s) params,
                do_alpha_convert env' body)
  | NexpApply (f, args) ->
    NexpApply (findsym env f,
               List.map (fun s -> findsym env s) args)
  | NexpBind (id, v) ->
    NexpBind (id, do_alpha_convert env v)
  | NexpLet (id, v, body) ->
    let env' = (addsyms env [id]) in
    NexpLet (findsym env' id,
             do_alpha_convert env v,
             do_alpha_convert env' body)
  | NexpPrefix (op, v) ->
    NexpPrefix (op, findsym env v)
  | NexpInfix (op, v1, v2) ->
    NexpInfix (op, findsym env v1, findsym env v2)
  | NexpIf (cond, if_body, else_body) ->
    NexpIf (findsym env cond, do_alpha_convert env if_body, do_alpha_convert env else_body)

let alpha_convert exp =
  do_alpha_convert Smap.empty exp
