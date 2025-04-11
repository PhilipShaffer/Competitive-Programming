open Llvm
open Base
open Ast

(* Environment entry types *)
type env_entry = {
  name: string;
  typ: value_type;
  mutable value: llvalue option;  (* LLVM value, None until assigned *)
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
  let entry = { name; typ; value = None } in
  Hashtbl.set env.entries ~key:name ~data:entry;
  entry

(* Update a variable's LLVM value *)
let update_var_value env name value =
  match lookup env name with
  | Some entry -> entry.value <- Some value
  | None -> failwith ("Variable " ^ name ^ " not found")

(* Get a variable's type *)
let get_var_type env name =
  match lookup env name with
  | Some entry -> entry.typ
  | None -> failwith ("Variable " ^ name ^ " not found")

(* Get a variable's LLVM value *)
let get_var_value env name =
  match lookup env name with
  | Some entry -> entry.value
  | None -> failwith ("Variable " ^ name ^ " not found") 