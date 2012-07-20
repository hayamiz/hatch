
open Syntax

val string_of_prefix_oper: Syntax.egg_prefix_oper -> string
val string_of_infix_oper: Syntax.egg_infix_oper -> string
val string_of_token: Parser.token -> string
val string_of_tokens: Parser.token list -> string
val string_of_literal: egg_literal -> string
val string_of_expr: ?indent:int -> egg_expr -> string

val egg_literal_equal: egg_literal -> egg_literal -> bool
val egg_expr_equal: egg_expr -> egg_expr -> bool
