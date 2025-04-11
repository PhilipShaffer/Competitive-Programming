open Base
open Ast

(* Type comparison result *)
type comp =
  | EQ    (* Types are equal *)
  | INCOMP (* Types are incompatible *)

(* Check if two types are equal *)
let eq (t1: value_type) (t2: value_type) : bool =
  match (t1, t2) with
  | (IntType, IntType) -> true
  | (FloatType, FloatType) -> true
  | (StringType, StringType) -> true
  | (BoolType, BoolType) -> true
  | _ -> false

(* Compare two types *)
let comp (t1: value_type) (t2: value_type) : comp =
  if eq t1 t2 then EQ else INCOMP

(* Check if a binary operation is valid for the given types *)
let is_valid_binop (op: bop) (t1: value_type) (t2: value_type) : bool =
  match op with
  | Add | Sub | Mult | Div | Mod ->
      (* Arithmetic operations only allowed between same numeric types *)
      (match (t1, t2) with
       | (IntType, IntType) -> true
       | (FloatType, FloatType) -> true
       | _ -> false)
  | Lt | Leq | Gt | Geq ->
      (* Comparison operations only allowed between same numeric types *)
      (match (t1, t2) with
       | (IntType, IntType) -> true
       | (FloatType, FloatType) -> true
       | _ -> false)
  | Eq | Neq ->
      (* Equality operations allowed between any types, but must be same type *)
      eq t1 t2
  | And | Or ->
      (* Logical operations only allowed between booleans *)
      (match (t1, t2) with
       | (BoolType, BoolType) -> true
       | _ -> false)

(* Check if a unary operation is valid for the given type *)
let is_valid_unop (op: uop) (t: value_type) : bool =
  match op with
  | Neg ->
      (* Negation only allowed on numeric types *)
      (match t with
       | IntType | FloatType -> true
       | _ -> false)
  | Not ->
      (* Logical not only allowed on boolean *)
      (match t with
       | BoolType -> true
       | _ -> false)

(* Get the result type of a binary operation *)
let binop_result_type (op: bop) (t1: value_type) (t2: value_type) : value_type option =
  if not (is_valid_binop op t1 t2) then None
  else
    match op with
    | Add | Sub | Mult | Div | Mod ->
        (* Arithmetic operations return the same type as operands *)
        Some t1
    | Lt | Leq | Gt | Geq | Eq | Neq | And | Or ->
        (* Comparison and logical operations return boolean *)
        Some BoolType

(* Get the result type of a unary operation *)
let unop_result_type (op: uop) (t: value_type) : value_type option =
  if not (is_valid_unop op t) then None
  else
    match op with
    | Neg ->
        (* Negation returns the same type as operand *)
        Some t
    | Not ->
        (* Logical not returns boolean *)
        Some BoolType

(* Convert type to string for error messages *)
let type_to_string (t: value_type) : string =
  match t with
  | IntType -> "int"
  | FloatType -> "float"
  | StringType -> "string"
  | BoolType -> "bool" 