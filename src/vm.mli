
open Vmis

val string_of_vminsns: vm_insn array -> string

type code_object = {
  label: string;
  body: vm_insn array;
}

type code_segment = {
  objects: code_object array;
}

