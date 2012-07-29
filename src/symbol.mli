
type sym = string

val gensym: ?basesym:string -> unit -> sym
val comp_sym: sym -> sym -> bool
val comp_syms: sym list -> sym list -> bool

