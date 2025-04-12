open Llvm
open Ast

(* Environment types *)
type env_entry = {
  name: string;
  typ: value_type;
  mutable value: llvalue option;
}

type environment

(* Environment operations *)
val create_env : ?parent:environment -> unit -> environment
val lookup : environment -> string -> env_entry option
val add_var : environment -> string -> value_type -> env_entry
val update_var_value : environment -> string -> llvalue -> unit
val get_var_type : environment -> string -> value_type
val get_var_value : environment -> string -> llvalue option 