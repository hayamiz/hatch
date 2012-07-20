
open Vmis

type code_object = {
  label: string;
  body: vm_insn array;
}

type code_segment = {
  objects: code_object array;
}


let string_of_vminsns insns =
  let strs = List.map
	(fun insn ->
	   match insn with 
		   VM_NOP -> "NOP")
	(Array.to_list insns)
  in
	String.concat "\n" strs
