open Llvm
open Base
open Ast

(* Environment entry types *)
type env_entry = 
  | VarEntry of {
      name: string;
      typ: value_type;
      mutable value: llvalue option;  (* LLVM value, None until assigned *)
    }
  | FunEntry of {
      name: string;
      param_types: value_type list;
      return_type: value_type;
      llvm_function: llvalue;
    }

(* Environment structure *)
type environment = {
  parent: environment option;
  entries: (string, env_entry) Hashtbl.t;
}

(* Create a new environment *)
let create_env ?parent () = {
  parent = parent;
  entries = Hashtbl.create (module String);
}

(* Look up a symbol in the environment *)
let rec lookup env name =
  match Hashtbl.find env.entries name with
  | Some entry -> Some entry
  | None -> 
    match env.parent with
    | Some parent_env -> lookup parent_env name
    | None -> None

(* Add a variable to the environment *)
let add_var env name typ =
  let entry = VarEntry { name; typ; value = None } in
  Hashtbl.set env.entries ~key:name ~data:entry;
  entry

(* Add a function to the environment *)
let add_fun env name param_types return_type llvm_function =
  let entry = FunEntry { name; param_types; return_type; llvm_function } in
  Hashtbl.set env.entries ~key:name ~data:entry;
  entry

(* Update a variable's LLVM value *)
let update_var_value env name value =
  match lookup env name with
  | Some (VarEntry entry) -> entry.value <- Some value
  | _ -> failwith ("Variable " ^ name ^ " not found or not a variable")

(* Get a variable's type *)
let get_var_type env name =
  match lookup env name with
  | Some (VarEntry entry) -> entry.typ
  | _ -> failwith ("Variable " ^ name ^ " not found or not a variable")

(* Get a function's LLVM value *)
let get_fun_value env name =
  match lookup env name with
  | Some (FunEntry entry) -> entry.llvm_function
  | _ -> failwith ("Function " ^ name ^ " not found or not a function")

(* Initialize built-in functions *)
let init_builtins env context builder =
  (* Add print function *)
  let print_type = function_type void_type context [| pointer_type i8_type context |] in
  let print_fun = declare_function "printf" print_type the_module in
  add_fun env "print" [StringType] VoidType print_fun;

  (* Add other built-in functions here as needed *)
  () 