
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
  | VM_CALL_CLS
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

let rec string_of_vminst inst =
  "> " ^
	match inst with
		VM_NOP -> "NOP"
	  | VM_PUSH hv -> "PUSH(" ^ (string_of_h_value hv) ^ ")"
	  | VM_POP -> "POP"
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
	  | VM_CALL_CLS -> "CALL_CLS"
	  | VM_RET -> "RET"
	  | VM_HALT -> "HALT"

and string_of_vminsts insts =
  (String.concat "\n" (List.map (fun inst -> string_of_vminst inst) insts))

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

let compile (hlvminsts: hlvminst list): vminst array =
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
			   | HL_CALL_CLS -> VM_CALL_CLS
			   | HL_RET -> VM_RET
			   | HL_HALT -> VM_HALT
			 in
			   inst :: insts)
	hlvminsts []
  in
	Array.of_list inst_list


type vmstate = {
  stack_pointer: int;
  frame_pointer: int;
}
