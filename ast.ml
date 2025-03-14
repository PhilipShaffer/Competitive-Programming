type uop =
  | Neg
  | Not

type bop =
  | Add
  | Sub
  | Mult
  | Div
  | Lt
  | Leq
  | Gt
  | Geq
  | Eq
  | Neq
  | And
  | Or
  | Mod

type expr =
 | Var of string
 | Int of int
 | Bool of bool
 | Binop of bop * expr * expr
 | Unop of uop * expr

type stmt =
 | Assign of string * expr
 | Let of string * expr * stmt
 | If of expr * stmt * stmt
 | While of expr * stmt
 | Print of expr
 | Block of stmt list