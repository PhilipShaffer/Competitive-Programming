(* lib/common/tac.ml *)

open Ast (* Need access to Ast.bop for printing *)

(* --- Type Definitions --- *)
(* These should mirror the definitions in tac.mli *)

type operand =
  | Var of string       (** Variable identifier *)
  | Const of int      (** Integer constant *)

type label = string     (** Label identifier *)

type instruction =
  | Assign of string * operand  (** x := y *)
  | BinOp of string * operand * Ast.bop * operand (** x := y op z *)
  | Label of label        (** L: *)
  | Jump of label         (** goto L *)
  | CondJump of operand * label * label (** if x goto L1 else goto L2 *)
  | Call of string option * string * operand list (** [x :=] f(arg1, arg2, ...) *)
  | Return of operand option (** return [x] *)

type basic_block = {
  label: label;          (** Entry label of the block *)
  instrs: instruction list; (** Instructions in the block *)
}

type tac_function = {
  name: string;           (** Function name *)
  params: string list;    (** Parameter names *)
  blocks: basic_block list; (** Basic blocks *)
}

type tac_program = {
  functions: tac_function list; (** List of functions in the program *)
}

(* --- Pretty Printing Functions --- *)

let string_of_operand = function
  | Var s -> s
  | Const i -> string_of_int i

(* Helper to print binary operators *)
let string_of_bop = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Lt -> "<"
  | Leq -> "<="
  | Gt -> ">"
  | Geq -> ">="
  | Eq -> "=="
  | Neq -> "!="
  | And -> "&&"
  | Or -> "||"
  | Mod -> "%"

let string_of_instruction = function
  | Assign (dest, src) ->
      Printf.sprintf "  %s := %s" dest (string_of_operand src)
  | BinOp (dest, op1, bop, op2) ->
      Printf.sprintf "  %s := %s %s %s" dest (string_of_operand op1) (string_of_bop bop) (string_of_operand op2)
  | Label l ->
      Printf.sprintf "%s:" l
  | Jump l ->
      Printf.sprintf "  goto %s" l
  | CondJump (cond, l1, l2) ->
      Printf.sprintf "  if %s goto %s else goto %s" (string_of_operand cond) l1 l2
  | Call (dest_opt, fname, args) ->
      let args_str = String.concat ", " (List.map string_of_operand args) in
      (match dest_opt with
       | Some dest -> Printf.sprintf "  %s := call %s(%s)" dest fname args_str
       | None -> Printf.sprintf "  call %s(%s)" fname args_str)
  | Return op_opt ->
      (match op_opt with
       | Some op -> Printf.sprintf "  return %s" (string_of_operand op)
       | None -> "  return")

let string_of_basic_block block =
  let label_str = Printf.sprintf "%s:\n" block.label in
  let instrs_str = List.map string_of_instruction block.instrs |> String.concat "\n" in
  label_str ^ instrs_str

let string_of_tac_function func =
  let params_str = String.concat ", " func.params in
  let header = Printf.sprintf "define %s(%s):\n" func.name params_str in
  let blocks_str = List.map string_of_basic_block func.blocks |> String.concat "\n\n" in
  header ^ blocks_str

let string_of_tac_program prog =
  List.map string_of_tac_function prog.functions |> String.concat "\n\n"