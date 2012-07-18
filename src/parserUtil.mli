
open Tree

val string_of_token: Parser.token -> string
val string_of_tokens: Parser.token list -> string
val string_of_literal: egg_literal -> string
val string_of_expr: egg_expr -> string

val egg_literal_equal: egg_literal -> egg_literal -> bool
val egg_expr_equal: egg_expr -> egg_expr -> bool
