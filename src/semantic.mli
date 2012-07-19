
exception Type_mismatch

type exp_type =
	TypUnsolved
  | TypInt
  | TypFloat
  | TypString
  | TypBool
  | TypClosure

val solve_type: Tree.egg_expr -> exp_type
