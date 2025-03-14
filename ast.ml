type uop =
  | Neg
  | Not

type bop =
  | Add
  | Sub
  | Mul
  | Div
  | Lt
  | Leq
  | Gt
  | Geq
  | Neq
  | Eq
  | And
  | Or
  | Mod

type expr =
 | Var of string
 | Int of int
 | Bool of bool
 | Binop of bop * expr * expr
 | Let of string * expr * expr

type stmt =
 | Assign of string * expr
 | If of expr * stmt * stmt
 | While of expr * stmt
 | Print of expr
 | Block of stmt list