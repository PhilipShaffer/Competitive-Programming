(* Abstract Syntax Tree (AST) for the While language *)

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

(* Expressions - represent computations that produce values *)
type expr =
 | Var of string             (* Variable reference: x *)
 | Int of int                (* Integer literal: 42 *)
 | Bool of bool              (* Boolean literal: true, false *)
 | Binop of bop * expr * expr (* Binary operation: e1 op e2 *)
 | Unop of uop * expr        (* Unary operation: op e *)

(* Statements - represent actions or commands *)
type stmt =
 | Assign of string * expr   (* Assignment: x = e *)
 | Let of string * expr * stmt (* Let binding: let x = e in s *)
 | If of expr * stmt * stmt  (* Conditional: if e then s1 else s2 *)
 | While of expr * stmt      (* Loop: while e do s *)
 | Print of expr             (* Print statement: print e *)
 | Block of stmt list        (* Block of statements: { s1; s2; ...; sn; } *)