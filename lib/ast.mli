(* Core language types *)
type value_type = IntType | FloatType | StringType | BoolType

(* Type information *)
type type_info = {
  expr_type: value_type;
  pos: int option;
}

(* Operators *)
type uop = Neg | Not
type bop = Add | Sub | Mult | Div | Lt | Leq | Gt | Geq | Eq | Neq | And | Or | Mod

(* Expressions *)
type raw_expr = 
  | Var of string
  | Int of int
  | String of string
  | Float of float
  | Bool of bool
  | Binop of bop * expr * expr
  | Unop of uop * expr
and expr = {
  expr: raw_expr;
  type_info: type_info option;
}

(* Statements *)
type stmt = 
  | Assign of string * expr
  | Declare of string * value_type * expr
  | Let of string * expr * stmt
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Print of expr
  | Block of stmt list 