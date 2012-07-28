
type sym = string

val gensym: unit -> sym
val comp_sym: sym -> sym -> bool
val comp_syms: sym list -> sym list -> bool

