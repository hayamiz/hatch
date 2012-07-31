
open Symbol
open Syntax
open Lambda
open Highlevel
include Hvalue
include Hnative

exception Compile_error

(* stk(n) ... n-th value from the top of the stack *)
(* opr(n) ... n-th operand of vminst *)
type vminst =
  VM_NOP                (* no operation *)
| VM_PUSH of h_value    (* opr(0) -> stack; *)
| VM_POP                (* _ <- stack; *)
| VM_DUP
| VM_LREF_PUSH of int   (* opr(0)-th local variable   -> stack; *)
| VM_GREF_PUSH of h_symbol  (* global variable opr(0) -> stack; *)
| VM_LSET of int       (* t <- stack; t -> opr(0)-th local variable *)
| VM_GSET of h_symbol  (* t <- stack; t -> global variable opr(0) *)
| VM_ADD            (* t0 <- stack; t1 <- stack; t1 + t0 -> stack; *)
| VM_SUB            (* t0 <- stack; t1 <- stack; t1 - t0 -> stack; *)
| VM_MUL            (* t0 <- stack; t1 <- stack; t1 * t0 -> stack; *)
| VM_DIV            (* t0 <- stack; t1 <- stack; t1 / t0 -> stack; *)
| VM_EQ             (* t0 <- stack; t1 <- stack; if t1 == t0 then true -> stack; else false -> stack; *)
| VM_LE             (* t0 <- stack; t1 <- stack; if t1 <= t0 then t0 -> stack; else false -> stack; *)
| VM_GE             (* t0 <- stack; t1 <- stack; if t1 >= t0 then t0 -> stack; else false -> stack; *)
| VM_LT             (* t0 <- stack; t1 <- stack; if t1 < t0 then t0 -> stack; else false -> stack; *)
| VM_GT             (* t0 <- stack; t1 <- stack; if t1 > t0 then t0 -> stack; else false -> stack; *)
| VM_LAND           (* t0 <- stack; t1 <- stack; if t1 && t0 then true -> stack; else false -> stack; *)
| VM_LOR            (* t0 <- stack; t1 <- stack; if t1 || t0 then true -> stack; else false -> stack; *)
| VM_LNOT           (* t <- stack; !t -> stack *)
| VM_GOTO of int    (* goto opr(0) *)
| VM_BIF  of int    (* t <- stack; if t then goto opr(0); else next inst; *)
| VM_BIFN of int   (* t <- stack; if !t then goto opr(0); else next inst; *)
| VM_PUSH_FRAME
| VM_CALL
| VM_RET
| VM_HALT

let rec vminst_equal ?(exact=false) expected actual =
  match (expected, actual) with
  | (VM_GOTO x, VM_GOTO y)
  | (VM_BIF x, VM_BIF y)
  | (VM_BIFN x, VM_BIFN y) -> x = y
  | (VM_PUSH x, VM_PUSH y) ->
    begin
      match (x, y) with
        (HV_function (HF_native (name1, _)), HV_function (HF_native (name2, _))) ->
          name1 = name2
      | (HV_closure (HF_native (name1, _), _), HV_closure (HF_native (name2, _), _)) ->
        name1 = name2
      | _ -> x = y
    end
  | (x, y) -> x = y

let list_mapi (f: int -> 'a -> 'b) (ls: 'a list) =
  let (_, ret) = (List.fold_left (fun (i, ret) l -> (i + 1, f i l :: ret)) (0, []) ls)
  in List.rev ret

let rec string_of_vminst inst =
  match inst with
    VM_NOP -> "NOP"
  | VM_PUSH hv -> "PUSH(" ^ (string_of_h_value hv) ^ ")"
  | VM_POP -> "POP"
  | VM_DUP -> "DUP"
  | VM_LREF_PUSH x -> "LREF_PUSH(" ^ (string_of_int x) ^ ")"
  | VM_GREF_PUSH s -> "GREF_PUSH(" ^ s ^ ")"
  | VM_LSET x -> "LSET(" ^ (string_of_int x) ^ ")"
  | VM_GSET x -> "LSET(" ^ x ^ ")"
  | VM_ADD -> "ADD"
  | VM_SUB -> "SUB"
  | VM_MUL -> "MUL"
  | VM_DIV -> "DIV"
  | VM_EQ -> "EQ"
  | VM_LE -> "LE"
  | VM_GE -> "GE"
  | VM_LT -> "LT"
  | VM_GT -> "GT"
  | VM_LAND -> "LAND"
  | VM_LOR -> "LOR"
  | VM_LNOT -> "LNOT"
  | VM_GOTO x -> "GOTO(" ^ (string_of_int x) ^ ")"
  | VM_BIF x -> "BIF(" ^ (string_of_int x) ^ ")"
  | VM_BIFN x -> "BIFN(" ^ (string_of_int x) ^ ")"
  | VM_PUSH_FRAME -> "PUSH_FRAME"
  | VM_CALL -> "CALL"
  | VM_RET -> "RET"
  | VM_HALT -> "HALT"

and string_of_vminsts insts =
  (String.concat "\n"
     (list_mapi (fun addr inst ->
       (string_of_int addr) ^ ": " ^ (string_of_vminst inst))
        insts))

and vminsts_equal ?(exact=false) expected actual =
  match (expected, actual) with
  | ([], []) -> true
  | (e :: erest, a :: arest) when (vminst_equal e a) ->
    vminsts_equal erest arest
  | (e :: _, a :: _) ->
    begin
      print_string ("vminsts_equal unmatched: e = " ^
                       (string_of_vminst e) ^ ", a = " ^ (string_of_vminst a));
      false
    end
  | _ -> false

let compile_h_value addrs (hv: h_value): h_value =
  match hv with
  | HV_function (HF_user_highlevel (name, label, arity)) ->
    HV_function (HF_user (name, Smap.find label addrs, arity))
  | _ -> hv

let compile (hlvminsts: hlvminst list): vminst list =
  let (addrs, _) =
    List.fold_left (fun (addrs, cur_addr) hlvminst ->
      match hlvminst with
        HL_LABEL l -> ((Smap.add l cur_addr addrs), cur_addr)
      | _ -> (addrs, cur_addr + 1))
      (Smap.empty, 0) hlvminsts
  in
  let get_addr label =
    Smap.find label addrs
  in
  let inst_list = List.fold_right
    (fun hlinst insts ->
      match hlinst with
        HL_LABEL _ -> insts
      | _ ->
        let inst = match hlinst with
            HL_NOP -> VM_NOP
          | HL_PUSH hv -> VM_PUSH (compile_h_value addrs hv)
          | HL_POP -> VM_POP
          | HL_DUP -> VM_DUP
          | HL_LREF_PUSH x -> VM_LREF_PUSH x
          | HL_GREF_PUSH s -> VM_GREF_PUSH s
          | HL_LSET x -> VM_LSET x
          | HL_GSET x -> VM_GSET x
          | HL_ADD -> VM_ADD
          | HL_SUB -> VM_SUB
          | HL_MUL -> VM_MUL
          | HL_DIV -> VM_DIV
          | HL_EQ -> VM_EQ
          | HL_LE -> VM_LE
          | HL_GE -> VM_GE
          | HL_LT -> VM_LT
          | HL_GT -> VM_GT
          | HL_LAND -> VM_LAND
          | HL_LOR -> VM_LOR
          | HL_LNOT -> VM_LNOT
          | HL_LABEL _ -> raise Compile_error
          | HL_GOTO x -> VM_GOTO (get_addr x)
          | HL_BIF x -> VM_BIF (get_addr x)
          | HL_BIFN x -> VM_BIFN (get_addr x)
          | HL_PUSH_FRAME -> VM_PUSH_FRAME
          | HL_CALL -> VM_CALL
          | HL_RET -> VM_RET
          | HL_HALT -> VM_HALT
        in
        inst :: insts)
    hlvminsts []
  in
  inst_list


let iota s e =
  let rec iota_iter ret n =
    if n > e then
      ret
    else
      iota_iter (n :: ret) (n+1)
  in
  List.rev (iota_iter [] s)

type vm_status = {
  pc: int;
  sp: int;
  fp: int;
  ep: int;
  inst: vminst;
  callstack: int list;
  stack: h_value array;
}

let run_vminsts (insts: vminst list): h_value =
  let code = Array.of_list insts in
  let callstack = ref [] in
  let stack = Array.make 8192 HV_undef in
  let rec step genv pc sp fp ep =
    let inst = code.(pc) in
    let stackdbg = if sp >= 0 then Array.sub stack 0 (sp+1) else [||] in
    let status = {
      pc = pc; sp = sp; fp = fp; ep = ep; inst = inst; callstack = !callstack; stack = stackdbg;
    } in
    let _ = status in
    match inst with
    | VM_NOP ->
      step genv (pc+1) sp fp ep
    | VM_PUSH hv ->
      stack.(sp+1) <- hv;
      step genv (pc+1) (sp+1) fp ep
    | VM_POP ->
      step genv (pc+1) (sp-1) fp ep
    | VM_DUP ->
      stack.(sp+1) <- stack.(sp);
      step genv (pc+1) (sp+1) fp ep
    | VM_LREF_PUSH ofst ->
      stack.(sp+1) <- stack.(ep+ofst);
      step genv (pc+1) (sp+1) fp ep
    | VM_GREF_PUSH s ->
      stack.(sp+1) <- (Smap.find s genv);
      step genv (pc+1) (sp+1) fp ep
    | VM_LSET ofst ->
      stack.(ep+ofst) <- stack.(sp);
      step genv (pc+1) (sp-1) fp ep
    | VM_GSET s ->
      let genv' = Smap.add s stack.(sp) genv
      in step genv' (pc+1) (sp-1) fp ep
    | VM_ADD ->
      let t1 = stack.(sp) in
      let t0 = stack.(sp-1) in
      stack.(sp-1) <- h_add t0 t1;
      step genv (pc+1) (sp-1) fp ep
    | VM_SUB ->
      let t1 = stack.(sp) in
      let t0 = stack.(sp-1) in
      stack.(sp-1) <- h_sub t0 t1;
      step genv (pc+1) (sp-1) fp ep
    | VM_MUL -> 
      let t1 = stack.(sp) in
      let t0 = stack.(sp-1) in
      stack.(sp-1) <- h_mul t0 t1;
      step genv (pc+1) (sp-1) fp ep
    | VM_DIV -> 
      let t1 = stack.(sp) in
      let t0 = stack.(sp-1) in
      stack.(sp-1) <- h_div t0 t1;
      step genv (pc+1) (sp-1) fp ep
    | VM_EQ ->
      let t1 = stack.(sp) in
      let t0 = stack.(sp-1) in
      stack.(sp-1) <- h_eq t0 t1;
      step genv (pc+1) (sp-1) fp ep
    | VM_LE -> 
      let t1 = stack.(sp) in
      let t0 = stack.(sp-1) in
      stack.(sp-1) <- h_le t0 t1;
      step genv (pc+1) (sp-1) fp ep
    | VM_GE -> 
      let t1 = stack.(sp) in
      let t0 = stack.(sp-1) in
      stack.(sp-1) <- h_ge t0 t1;
      step genv (pc+1) (sp-1) fp ep
    | VM_LT -> 
      let t1 = stack.(sp) in
      let t0 = stack.(sp-1) in
      stack.(sp-1) <- h_lt t0 t1;
      step genv (pc+1) (sp-1) fp ep
    | VM_GT -> 
      let t1 = stack.(sp) in
      let t0 = stack.(sp-1) in
      stack.(sp-1) <- h_gt t0 t1;
      step genv (pc+1) (sp-1) fp ep
    | VM_LAND -> 
      let t1 = stack.(sp) in
      let t0 = stack.(sp-1) in
      stack.(sp-1) <- h_land t0 t1;
      step genv (pc+1) (sp-1) fp ep
    | VM_LOR -> 
      let t1 = stack.(sp) in
      let t0 = stack.(sp-1) in
      stack.(sp-1) <- h_lor t0 t1;
      step genv (pc+1) (sp-1) fp ep
    | VM_LNOT ->
      stack.(sp) <- h_lnot stack.(sp);
      step genv (pc+1) sp fp ep
    | VM_GOTO addr ->
      step genv addr sp fp ep
    | VM_BIF addr ->
      if (h_is_true stack.(sp)) then
        step genv addr (sp-1) fp ep
      else
        step genv (pc+1) (sp-1) fp ep
    | VM_BIFN addr -> 
      if (h_is_true stack.(sp)) then
        step genv (pc+1) (sp-1) fp ep
      else
        step genv addr (sp-1) fp ep
    | VM_PUSH_FRAME ->
      stack.(sp + 1) <- HV_fp fp;
      step genv (pc+1) (sp+1) (sp+1) ep
    | VM_CALL ->
      let nargs = sp - fp - 1 in
      begin
        match stack.(sp) with
        | HV_function (HF_user (name, addr, arity)) ->
          if arity = nargs then
            begin
              callstack := (pc+1)::!callstack;
              step genv addr (sp-1) fp (fp+1)
            end
          else
            arity_error name arity nargs
        | HV_function (HF_native (name, f)) ->
          let fp' = match stack.(fp) with 
              HV_fp x -> x
            | _ -> runtime_error ("fp error")
          in
          let args = List.map (fun idx -> stack.(idx)) (iota (fp+1) (sp-1))
          in
          stack.(fp) <- f args;
          step genv (pc+1) fp fp' ep
        | HV_closure (HF_user (name, addr, arity), fvar_args) ->
          if arity = nargs + (List.length fvar_args) then
            begin
              let _ = List.fold_left (fun sp' arg -> stack.(sp') <- arg; sp' + 1) sp fvar_args in
              callstack := (pc+1)::!callstack;
              step genv addr (sp) fp (fp+1)
            end
          else
            arity_error name (arity - (List.length fvar_args)) nargs
        | _ ->
          runtime_error ("Egg object '" ^ (string_of_h_value stack.(sp)) ^
                            "' is not a function.")
      end
    | VM_RET ->
      let retval = stack.(sp) in
      let fp' = match stack.(fp) with
          HV_fp x -> x
        | _ -> runtime_error ("fp error")
      in
      let return_addr = List.hd !callstack in
      stack.(fp) <- retval;
      callstack := List.tl !callstack;
      step genv return_addr fp fp' (fp'+1)
    | VM_HALT ->
      stack.(sp)
  in
  step h_native_env 0 (-1) (-1) 0
