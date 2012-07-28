
open Hvalue

let h_native_make_closure (vals: h_value list): h_value =
  match vals with
	  ((HV_function (_ as fval))) :: ((_ :: _) as args) ->
		HV_closure (fval, args)
	| _ ->
		raise Runtime_error

let h_native_print (vals: h_value list): h_value =
  HV_undef

let h_native_env =
  List.fold_left
	(fun env (name, f) ->
	   Smap.add name f env)
	Smap.empty
	[("make_closure", h_native_make_closure);
	 ("print", h_native_print);
	 ]

let h_native_get name =
  let f = Smap.find name h_native_env
  in
	HV_function (HF_native (name, f))
