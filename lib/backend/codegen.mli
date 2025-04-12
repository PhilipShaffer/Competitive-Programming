open Llvm
open Common.Ast

(* Typed value structure *)
type typed_value = {
  value: llvalue;
  typ: value_type;
}

(* Type checking utilities *)
val is_string_expr : expr -> bool
val is_float_expr : expr -> bool

(* LLVM value access *)
val get_llvm_value : typed_value -> llvalue

(* Code generation *)
val compile : stmt -> llmodule 