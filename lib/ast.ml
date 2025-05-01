(* Abstract Syntax Tree (AST) for the While language *)

(* Value type enum to represent types *)
type value_type =
  | IntType
  | FloatType
  | StringType
  | BoolType
  | VoidType
  | ArrayType of value_type  (* Array type with element type *)

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
 | String of string
 | Float of float
 | Bool of bool              (* Boolean literal: true, false *)
 | Binop of bop * expr * expr (* Binary operation: e1 op e2 *)
 | Unop of uop * expr        (* Unary operation: op e *)
 | FunCall of string * expr list  (* Function call: f(a, b, ...) *)
 | ArrayLit of expr list     (* Array literal: [1, 2, 3] *)
 | ArrayGet of expr * expr   (* Array access: arr[idx] *)
 | ArraySet of expr * expr * expr (* Array assignment: arr[idx] = value *)
 | ArrayLen of expr          (* Array length: len(arr) *)

(* Statements - represent actions or commands *)
type stmt =
 | Assign of string * expr   (* Assignment: x = expr *)
 | Declare of string * value_type * expr (* Type declaration *)
 | Let of string * expr * stmt (* Let binding: let x = expr in stmt *)
 | If of expr * stmt * stmt  (* Conditional: if expr then stmt1 else stmt2 *)
 | While of expr * stmt      (* Loop: while expr do stmt *)
 | Print of expr             (* Print statement: print expr *)
 | Block of stmt list        (* Block of statements: { stmt1; stmt2; ...; stmtn; } *)
 | FunDecl of string * (string * value_type) list * value_type * stmt  (* Function declaration: name, params, return type, body *)
 | Return of expr           (* Return statement *)