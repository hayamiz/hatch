
open Tree

exception Type_mismatch

type exp_type =
	TypUnsolved
  | TypInt
  | TypFloat
  | TypString
  | TypBool
  | TypClosure

let rec solve_type e =
  match e with
	  ExpLiteral (LitInt _, _) -> TypInt
	| ExpLiteral (LitFloat _, _) -> TypFloat
	| ExpLiteral (LitString _, _) -> TypString
	| ExpLiteral (LitBool _, _) -> TypBool
	| ExpClosure _ -> TypClosure
	| ExpPrefix (PrefixLnot, e, _) ->
		begin match (solve_type e) with
		  TypUnsolved -> TypUnsolved
		  | _ -> TypBool
		end
	| ExpPrefix (preop, e, _) when preop = PrefixPlus || preop = PrefixMinus ->
		begin match (solve_type e) with
		  TypInt -> TypInt
		  | TypFloat -> TypFloat
		  | _ -> TypUnsolved
		end
	| ExpInfix (InfixPlus, e1, e2, _) ->
		begin match (solve_type e1, solve_type e2) with
			(TypInt, TypInt) -> TypInt
		  | (TypInt, TypFloat) -> TypFloat
		  | (TypFloat, TypInt) -> TypFloat
		  | (TypFloat, TypFloat) -> TypFloat
		  | (TypString, TypString) -> TypString
		  | _ -> TypUnsolved
		end
	| ExpInfix (InfixMinus, e1, e2, _)
	| ExpInfix (InfixDiv, e1, e2, _) ->
		begin match (solve_type e1, solve_type e2) with
			(TypInt, TypInt) -> TypInt
		  | (TypInt, TypFloat) -> TypFloat
		  | (TypFloat, TypInt) -> TypFloat
		  | (TypFloat, TypFloat) -> TypFloat
		  | _ -> TypUnsolved
		end
	| ExpInfix (InfixMul, e1, e2, _) ->
		begin match (solve_type e1, solve_type e2) with
			(TypInt, TypInt) -> TypInt
		  | (TypInt, TypFloat) -> TypFloat
		  | (TypFloat, TypInt) -> TypFloat
		  | (TypFloat, TypFloat) -> TypFloat
		  | (TypString, TypInt) -> TypString
		  | _ -> TypUnsolved
		end
	| ExpInfix (InfixLe, e1, e2, _)
	| ExpInfix (InfixGe, e1, e2, _)
	| ExpInfix (InfixLt, e1, e2, _)
	| ExpInfix (InfixGt, e1, e2, _) ->
		begin match (solve_type e1, solve_type e2) with
			(TypInt, TypInt) -> TypInt
		  | (TypInt, TypFloat) -> TypFloat
		  | (TypFloat, TypInt) -> TypInt
		  | (TypFloat, TypFloat) -> TypFloat
		  | _ -> TypUnsolved
		end
	| ExpInfix (InfixEq, e1, e2, _) ->
		begin match (solve_type e1, solve_type e2) with
			(TypInt, TypInt) -> TypInt
		  | (TypInt, TypFloat) -> TypFloat
		  | (TypFloat, TypInt) -> TypInt
		  | (TypFloat, TypFloat) -> TypFloat
		  | (a, b) when a = b -> TypBool
		  | _ -> TypUnsolved
		end
	| ExpInfix (inop, e1, e2, _) -> TypUnsolved
	| _ -> TypUnsolved

