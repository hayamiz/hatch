
module M =
  Map.Make
    (struct
      type t = Symbol.sym
      let compare = compare
    end)

include M
