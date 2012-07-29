
open Symbol
open Syntax
open Lambda
include Hvalue
include Hnative

exception Compile_error

type label = string

(* stk(n) ... n-th value from the top of the stack *)
(* opr(n) ... n-th operand of vminst *)
type hlvminst =
  HL_NOP                (* no operation *)
| HL_PUSH of h_value    (* opr(0) -> stack; *)
| HL_POP                (* _ <- stack; *)
| HL_DUP                (* t <- stack; t -> stack; t -> stack; *)
| HL_LREF_PUSH of int   (* opr(0)-th local variable   -> stack; *)
| HL_GREF_PUSH of h_symbol  (* global variable opr(0) -> stack; *)
| HL_LSET of int       (* t <- stack; t -> opr(0)-th local variable *)
| HL_GSET of h_symbol  (* t <- stack; t -> global variable opr(0) *)
| HL_ADD            (* t0 <- stack; t1 <- stack; t1 + t0 -> stack; *)
| HL_SUB            (* t0 <- stack; t1 <- stack; t1 - t0 -> stack; *)
| HL_MUL            (* t0 <- stack; t1 <- stack; t1 * t0 -> stack; *)
| HL_DIV            (* t0 <- stack; t1 <- stack; t1 / t0 -> stack; *)
| HL_EQ             (* t0 <- stack; t1 <- stack; if t1 == t0 then true -> stack; else false -> stack; *)
| HL_LE             (* t0 <- stack; t1 <- stack; if t1 <= t0 then t0 -> stack; else false -> stack; *)
| HL_GE             (* t0 <- stack; t1 <- stack; if t1 >= t0 then t0 -> stack; else false -> stack; *)
| HL_LT             (* t0 <- stack; t1 <- stack; if t1 < t0 then t0 -> stack; else false -> stack; *)
| HL_GT             (* t0 <- stack; t1 <- stack; if t1 > t0 then t0 -> stack; else false -> stack; *)
| HL_LAND           (* t0 <- stack; t1 <- stack; if t1 && t0 then true -> stack; else false -> stack; *)
| HL_LOR            (* t0 <- stack; t1 <- stack; if t1 || t0 then true -> stack; else false -> stack; *)
| HL_LNOT           (* t <- stack; !t -> stack *)
| HL_GOTO of label    (* goto opr(0) *)
| HL_BIF  of label    (* t <- stack; if t then goto opr(0); else next inst; *)
| HL_BIFN of label   (* t <- stack; if !t then goto opr(0); else next inst; *)
| HL_PUSH_FRAME
| HL_CALL
| HL_RET
| HL_LABEL of label
| HL_HALT

let rec hlvminst_equal ?(exact=false) expected actual =
  match (expected, actual) with
  | (HL_GOTO x, HL_GOTO y)
  | (HL_BIF x, HL_BIF y)
  | (HL_BIFN x, HL_BIFN y)
  | (HL_LABEL x, HL_LABEL y) -> comp_sym x y
  | (HL_PUSH x, HL_PUSH y) ->
    begin
      match (x, y) with
        (HV_function (HF_native (name1, _)), HV_function (HF_native (name2, _))) ->
          name1 = name2
      | (HV_closure (HF_native (name1, _), _), HV_closure (HF_native (name2, _), _)) ->
        name1 = name2
      | _ -> x = y
    end
  | (x, y) -> x = y

let rec string_of_hlvminst ?(addr=(-1)) inst =
  let prefix = if addr < 0 then "> " else ((string_of_int addr) ^ ": ") in
  match inst with
    HL_NOP -> prefix ^  "NOP"
  | HL_PUSH hv -> prefix ^  "PUSH(" ^ (string_of_h_value hv) ^ ")"
  | HL_POP -> prefix ^  "POP"
  | HL_DUP -> prefix ^  "DUP"
  | HL_LREF_PUSH x -> prefix ^  "LREF_PUSH(" ^ (string_of_int x) ^ ")"
  | HL_GREF_PUSH s -> prefix ^  "GREF_PUSH(" ^ s ^ ")"
  | HL_LSET x -> prefix ^  "LSET(" ^ (string_of_int x) ^ ")"
  | HL_GSET x -> prefix ^  "GSET(" ^ x ^ ")"
  | HL_ADD -> prefix ^  "ADD"
  | HL_SUB -> prefix ^  "SUB"
  | HL_MUL -> prefix ^  "MUL"
  | HL_DIV -> prefix ^  "DIV"
  | HL_EQ -> prefix ^  "EQ"
  | HL_LE -> prefix ^  "LE"
  | HL_GE -> prefix ^  "GE"
  | HL_LT -> prefix ^  "LT"
  | HL_GT -> prefix ^  "GT"
  | HL_LAND -> prefix ^  "LAND"
  | HL_LOR -> prefix ^  "LOR"
  | HL_LNOT -> prefix ^  "LNOT"
  | HL_LABEL x -> x ^ ":"
  | HL_GOTO x -> prefix ^  "GOTO(" ^ x ^ ")"
  | HL_BIF x -> prefix ^  "BIF(" ^ x ^ ")"
  | HL_BIFN x -> prefix ^  "BIFN(" ^ x ^ ")"
  | HL_PUSH_FRAME -> prefix ^  "PUSH_FRAME"
  | HL_CALL -> prefix ^  "CALL"
  | HL_RET -> prefix ^  "RET"
  | HL_HALT -> prefix ^  "HALT"

and string_of_hlvminsts insts =
  let (_, inst_strs) =
    List.fold_left
      (fun (addr, strs) inst ->
        let str = string_of_hlvminst ~addr:addr inst in
        match inst with
        | HL_LABEL _ -> (addr, str :: strs)
        | _ ->          (addr+1, str :: strs))
      (0, []) insts
  in
  (String.concat "\n" (List.rev inst_strs))

and hlvminsts_equal ?(exact=false) expected actual =
  match (expected, actual) with
  | ([], []) -> true
  | (e :: erest, a :: arest) when (hlvminst_equal e a) ->
    hlvminsts_equal erest arest
  | (e :: _, a :: _) ->
    begin
      print_string ("hlvminsts_equal unmatched: e = " ^
                       (string_of_hlvminst e) ^ ", a = " ^ (string_of_hlvminst a));
      false
    end
  | _ -> false


let list_index e l =
  let rec list_index_iter n l =
    match l with
      [] -> raise Not_found
    | e' :: l when e = e' -> n
    | _ :: l -> list_index_iter (n+1) l
  in
  list_index_iter 0 l

let list_dup e n =
  let rec list_dup_iter l n =
    if n = 0 then
      l
    else
      list_dup_iter (e :: l) (n - 1)
  in
  list_dup_iter [] n

let rec get_local_vars lle =
  match lle with
    LLBind (_, e) ->
      get_local_vars e
  | LLIf (_, e1, e2) ->
    (get_local_vars e1) @ (get_local_vars e2)
  | LLLet (var, e1, e2) ->
    var :: ((get_local_vars e1) @ (get_local_vars e2))
  | _ -> []

(* args: function args (+ closure args) *)
let rec compile_ll_expr funenv (localenv: sym list) (lle: ll_expr): hlvminst list =
  let push_value_by_sym sym =
    if Smap.mem sym funenv then
      HL_PUSH (Smap.find sym funenv)
    else if List.mem sym localenv then
      HL_LREF_PUSH (list_index sym localenv)
    else
      HL_GREF_PUSH sym
  in
  let expr_body =
    match lle with
      LLVar sym -> [push_value_by_sym sym]
    | LLInt x -> [HL_PUSH (HV_int x)]
    | LLFloat x -> [HL_PUSH (HV_float x)]
    | LLString x -> [HL_PUSH (HV_string x)]
    | LLBool x -> [HL_PUSH (HV_bool x)]
    | LLUndef -> [HL_PUSH HV_undef]
    | LLMakeCls (fsym, argsyms) ->
      (HL_PUSH_FRAME ::
         (push_value_by_sym fsym) ::
         (List.map push_value_by_sym argsyms)) @ 
        [HL_PUSH (h_native_get "make_closure"); HL_CALL]
    | LLFunApply (fsym, argsyms) ->
      (HL_PUSH_FRAME ::
         (List.map push_value_by_sym argsyms)) @
        [(push_value_by_sym fsym); HL_CALL]
    | LLBind (sym, e) ->
      (compile_ll_expr funenv localenv e) @
        [HL_DUP; HL_GSET sym]
    | LLLet (sym, v, body) ->
      if List.mem sym localenv then
        (compile_ll_expr funenv localenv v) @
          [HL_LSET (list_index sym localenv)] @
          (compile_ll_expr funenv localenv body)
      else
        (print_string ("localenv = " ^ (String.concat ", " localenv) ^ "\n"); raise Compile_error)
    | LLPrefix (op, v) ->
      raise (Failure "prefix operator not implemented")
    | LLInfix (op, v1, v2) ->
      (push_value_by_sym v1) ::
        (push_value_by_sym v2) ::
        [match op with
          InfixPlus  -> HL_ADD
        | InfixMinus -> HL_SUB
        | InfixMul   -> HL_MUL
        | InfixDiv	-> HL_DIV
        | InfixEq	-> HL_EQ
        | InfixLe	-> HL_LE
        | InfixGe	-> HL_GE
        | InfixLt	-> HL_LT
        | InfixGt	-> HL_GT
        | InfixLand	-> HL_LAND
        | InfixLor   -> HL_LOR]
    | LLIf (cond_sym, ifclause, elseclause) ->
      let else_label = gensym () in
      let finish_label = gensym () in
      [push_value_by_sym cond_sym;
       HL_BIFN else_label] @
        (compile_ll_expr funenv localenv ifclause) @
        [HL_GOTO finish_label;
         HL_LABEL else_label] @
        (compile_ll_expr funenv localenv elseclause) @
        [HL_LABEL finish_label]
  in
  expr_body

let compile_ll_fundef funenv (fundef: ll_fundef) =
  match fundef with
  | (LLFlatFun (sym, args, body)) ->
    let local_vars = (get_local_vars body) in
    let localenv_vars = args @ local_vars in
    let h_funval =
      HV_function (HF_user_highlevel (sym, sym, List.length args)) in
    let funenv' = (Smap.add sym h_funval funenv) in
    let body_insts =
      [HL_LABEL sym] @

(* reserve stack for local vars *)
        (list_dup (HL_PUSH HV_undef) (List.length local_vars)) @
        (compile_ll_expr funenv' localenv_vars body)
    in
    (funenv', body_insts @ [HL_RET])
  | (LLClsFun (sym, fvars, args, body)) ->
    let local_vars = (get_local_vars body) in
    let localenv_vars = args @ fvars @ local_vars in
    let h_funval =
      HV_function (HF_user_highlevel (sym, sym, (List.length fvars) + (List.length args))) in
    let funenv' = (Smap.add sym h_funval funenv) in
    let body_insts =
      [HL_LABEL sym] @

(* reserve stack for local vars *)
        (list_dup (HL_PUSH HV_undef) (List.length local_vars)) @
        (compile_ll_expr funenv' localenv_vars body)
    in
    (funenv', body_insts @ [HL_RET])


let compile (llprog: ll_program): hlvminst list =
  let rec compile_fundefs funenv insts fundefs =
    match fundefs with
      [] -> (funenv, insts)
    | fundef :: fundefs' ->
      let (funenv', fun_insts) = compile_ll_fundef funenv fundef in
      compile_fundefs funenv' (insts @ fun_insts) fundefs'
  in
  let (funenv, fundef_insts) =
    compile_fundefs Smap.empty [HL_GOTO "*MAIN*"] (List.rev llprog.funcs)
  in
  let local_vars = get_local_vars llprog.main in
  let main_expr =
    (list_dup (HL_PUSH HV_undef) (List.length local_vars)) @
      (compile_ll_expr funenv local_vars llprog.main) @ [HL_HALT]
  in
  fundef_insts @ [HL_LABEL "*MAIN*"] @ main_expr
