
type sym = string


let counter = ref 0

let gensym ?(basesym="") _ =
  incr counter;
  let suffix = if (String.length basesym) > 0 then
      "." ^ basesym
    else
      ""
  in
  (Printf.sprintf "sym#%d" !counter)  ^ suffix

let comp_sym x y =
  if (String.length x >= 4 && (String.sub x 0 4) = "sym#") &&
	(String.length y >= 4 && (String.sub y 0 4) = "sym#") then
	  true
  else
	x = y

let rec comp_syms ss1 ss2 =
  match (ss1, ss2) with
	  ([], []) -> true
	| (_ :: _, [])
	| ([], _ :: _) -> false
	| (s1 :: ss1, s2 :: ss2) ->
		if comp_sym s1 s2 then
		  comp_syms ss1 ss2
		else false
