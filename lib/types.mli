open Ast

(* Type comparison result *)
type comp = EQ | INCOMP

(* Core type operations *)
val eq : value_type -> value_type -> bool
val comp : value_type -> value_type -> comp

(* Type checking for operators *)
val is_valid_binop : bop -> value_type -> value_type -> bool
val is_valid_unop : uop -> value_type -> bool

(* Type inference *)
val binop_result_type : bop -> value_type -> value_type -> value_type option
val unop_result_type : uop -> value_type -> value_type option

(* Utilities *)
val type_to_string : value_type -> string 