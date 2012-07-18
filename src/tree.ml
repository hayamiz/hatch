
type location = {
  file: string;						(* filename ("(none)" if not available) *)
  line: int;							(* line number *)
  offset: int;						(* offset from the beginning of the line *)
  byte: int;							(* byte position from the beginning of the file *)
}


let noloc = {
  file = "(none)";
  line = 0;
  offset = 0;
  byte = 0;
}

let current_file = ref "(none)"
