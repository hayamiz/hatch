
type sym = string


let counter = ref 0

let gensym _ =
  incr counter;
  Printf.sprintf "sym#%d" !counter

