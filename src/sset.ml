
module Sset =
  Set.Make
    (struct
      type t = Symbol.sym
      let compare = compare
    end)
include Sset

let sset_of_list ls =
  List.fold_left (fun ss sym -> Sset.add sym ss) Sset.empty ls

