(* Abstract Syntax Tree (AST) for the While language *)

(* Source location information *)
type location = {
  start_line: int;
  start_col: int;
  end_line: int;
  end_col: int;
}

(* Value type enum to represent types *)
type value_type =
  | IntType
  | FloatType
  | StringType
  | BoolType

(* Type information for expressions *)
type type_info = {
  expr_type: value_type;
  pos: location option;  (* Source location information *)
}

(* Unary operators - operations that take a single operand *)
type uop =
  | Neg   (* Arithmetic negation: -e *)
  | Not   (* Logical negation: not e *)

(* Binary operators - operations that take two operands *)
type bop =
  | Add   (* Addition: e1 + e2 *)
  | Sub   (* Subtraction: e1 - e2 *)
  | Mult  (* Multiplication: e1 * e2 *)
  | Div   (* Division: e1 / e2 *)
  | Lt    (* Less than: e1 < e2 *)
  | Leq   (* Less than or equal: e1 <= e2 *)
  | Gt    (* Greater than: e1 > e2 *)
  | Geq   (* Greater than or equal: e1 >= e2 *)
  | Eq    (* Equality: e1 == e2 *)
  | Neq   (* Inequality: e1 != e2 *)
  | And   (* Logical AND: e1 and e2 *)
  | Or    (* Logical OR: e1 or e2 *)
  | Mod   (* Modulo: e1 % e2 *)

(* Raw expressions without type information *)
type raw_expr =
 | Var of string             (* Variable reference: x *)
 | Int of int                (* Integer literal: 42 *)
 | String of string
 | Float of float
 | Bool of bool              (* Boolean literal: true, false *)
 | Binop of bop * expr * expr (* Binary operation: e1 op e2 *)
 | Unop of uop * expr        (* Unary operation: op e *)

(* Typed expressions - expressions with type information *)
and expr = {
  expr: raw_expr;
  type_info: type_info option;  (* None for unannotated expressions *)
  loc: location;                (* Source location *)
}

(* Statements - represent actions or commands *)
type stmt =
 | Assign of string * expr   (* Assignment: x = expr *)
 | Declare of string * value_type * expr (* Type declaration *)
 | Let of string * expr * stmt (* Let binding: let x = expr in stmt *)
 | If of expr * stmt * stmt  (* Conditional: if expr then stmt1 else stmt2 *)
 | While of expr * stmt      (* Loop: while expr do stmt *)
 | Print of expr             (* Print statement: print expr *)
 | Block of stmt list        (* Block of statements: { stmt1; stmt2; ...; stmtn; } *)

(* Show functions *)
val show_location : location -> string
val show_value_type : value_type -> string
val show_type_info : type_info -> string
val show_uop : uop -> string
val show_bop : bop -> string
val show_raw_expr : raw_expr -> string
val show_expr : expr -> string
val show_stmt : stmt -> string

(* Equality functions *)
val equal_location : location -> location -> bool
val equal_value_type : value_type -> value_type -> bool
val equal_type_info : type_info -> type_info -> bool
val equal_uop : uop -> uop -> bool
val equal_bop : bop -> bop -> bool
val equal_raw_expr : raw_expr -> raw_expr -> bool
val equal_expr : expr -> expr -> bool
val equal_stmt : stmt -> stmt -> bool 