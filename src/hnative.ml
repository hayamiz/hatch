
open Hvalue

let runtime_error msg =
  raise (Runtime_error msg)

let arity_error fname expected got =
  let msg = "function '" ^ fname ^ "' requires " ^ (string_of_int expected) ^
    " args, but got " ^ (string_of_int got)
  in runtime_error msg

let h_native_make_closure (vals: h_value list): h_value =
  match vals with
	  ((HV_function (_ as fval))) :: ((_ :: _) as args) ->
		HV_closure (fval, args)
	| _ ->
		raise (Runtime_error "make_closure: error")

let h_native_print (vals: h_value list): h_value =
  let rec iter vals =
	match vals with
	  | [] -> HV_undef
	  | HV_int x :: vals ->
		  print_string (string_of_int x);
		  iter vals
	  | HV_float x :: vals ->
		  print_string (string_of_float x);
		  iter vals
	  | HV_undef :: vals ->
		  print_string "undef";
		  iter vals
	  | HV_bool x :: vals ->
		  print_string (string_of_bool x);
		  iter vals
	  | HV_string x :: vals ->
		  print_string x;
		  iter vals
	  | (HV_function _ as v) :: vals ->
		  print_string (string_of_h_value v);
		  iter vals
	  | (HV_closure _ as v) :: vals ->
		  print_string (string_of_h_value v);
		  iter vals
  in
	iter vals

let h_native_puts (vals: h_value list): h_value =
  let _ = List.iter (fun v -> let _ = h_native_print [v] in print_newline ()) vals
  in HV_undef

let h_native_identity (vals: h_value list): h_value =
  if List.length vals = 1 then
    List.hd vals
  else
    arity_error "identity" 1 (List.length vals)

let h_native_env =
  List.fold_left
	(fun env (name, f) ->
	   Smap.add name (HV_function (HF_native (name, f))) env)
	Smap.empty
	[("make_closure", h_native_make_closure);
	 ("print", h_native_print);
	 ("puts", h_native_puts);
	 ("identity", h_native_identity);
	 ]

let h_native_get name =
  Smap.find name h_native_env
