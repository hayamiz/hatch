
(* Hatch value system *)

exception Runtime_error

let quote_string str =
  (Str.global_replace
	 (Str.regexp "\t") "\\t"
	 (Str.global_replace
		(Str.regexp "\n") "\\n"
		(Str.global_replace (Str.regexp "\"") "\\\"" str)))

type h_symbol = string

type h_function =
	(* user defined function in hatch VM *)
  | HF_user of string (* name *) * int (* program address*) * int (* arity *)

  (* user defined function in highlevel VM *)
  | HF_user_highlevel of string (* name *) * Symbol.sym (* program address label *) * int (* arity *)

  (* native function *)
  | HF_native of string (* name *) * ((h_value list) (* arguments *) -> h_value)



and h_value =
  | HV_undef
  | HV_bool      of bool
  | HV_int       of int
  | HV_float     of float
  | HV_string    of string
  | HV_symbol    of h_symbol
  | HV_function  of h_function
  | HV_closure   of (h_function * h_value list)


let rec string_of_h_value (hv: h_value): string =
  match hv with
	| HV_undef -> "undef"
	| HV_bool x -> string_of_bool x
	| HV_int x -> string_of_int x
	| HV_float x -> string_of_float x
	| HV_string x -> "\"" ^ (quote_string x) ^ "\""
	| HV_symbol x -> x
	| HV_function hf -> string_of_h_function hf
	| HV_closure (hf, args) ->
		"(" ^ (string_of_h_function hf) ^ "(" ^
		  (String.concat ", " (List.map string_of_h_value args)) ^ "))"

and string_of_h_function (hf: h_function): string =
  match hf with
	| HF_user (name, pc, arity) -> (* this value is only allowed in HatchVM *)
		"<fun " ^ name ^ ":Addr=" ^ (string_of_int pc) ^ ",arity=" ^ (string_of_int arity) ^ ">" 
	| HF_user_highlevel (name, label, arity) -> (* this value is only allowed in HighlevelVM *)
		"<fun " ^ name ^ ":Label=" ^ label ^ ",arity=" ^ (string_of_int arity) ^ ">" 
	| HF_native (name, _) ->
		"<native fun:" ^ name ^ ">"

let h_make_sym (str: string): h_symbol =
  str

let h_is_true (hv: h_value): bool =
  match hv with
	  HV_undef
	| HV_bool false ->
		false
	| _ -> true

let h_add (hv1: h_value) (hv2: h_value): h_value =
  match (hv1, hv2) with
	  (HV_int x, HV_int y) -> HV_int (x + y)
	| (HV_int x, HV_float y) -> HV_float ((float_of_int x) +. y)
	| (HV_float x, HV_int y) -> HV_float (x +. (float_of_int y))
	| (HV_string s1, HV_string s2) -> HV_string (s1 ^ s2)
	| _ -> HV_undef

let h_mul (hv1: h_value) (hv2: h_value): h_value =
  match (hv1, hv2) with
	  (HV_int x, HV_int y) -> HV_int (x * y)
	| (HV_int x, HV_float y) -> HV_float ((float_of_int x) *. y)
	| (HV_float x, HV_int y) -> HV_float (x *. (float_of_int y))
	| (HV_string s1, HV_int x) ->
		let rec rep strs n =
		  match n with
			  0 -> String.concat "" strs
			| n -> rep (s1 :: strs) (n - 1)
		in
		  HV_string (rep [] x)
	| _ -> HV_undef

let h_num_infix
	(iop: int -> int -> int)
	(fop: float -> float -> float): h_value -> h_value -> h_value =
  (fun hv1 hv2 ->
	match (hv1, hv2) with
		(HV_int x, HV_int y) -> HV_int (iop x y)
	  | (HV_int x, HV_float y) -> HV_float (fop (float_of_int x) y)
	  | (HV_float x, HV_int y) -> HV_float (fop x (float_of_int y))
	  | _ -> HV_undef)

let h_sub = h_num_infix (-) (-.)

let h_div = h_num_infix (/) (/.)

