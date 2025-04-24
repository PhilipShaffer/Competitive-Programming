open Base
open Ast

(** High-Level Intermediate Representation - Interface *)

(** Unique symbol identifier *)
type hir_symbol = int

(** HIR expressions, always type-annotated *)
type hir_expr =
  | HVar of hir_symbol * value_type
  | HInt of int
  | HString of string
  | HFloat of float
  | HBool of bool
  | HBinop of bop * hir_expr * hir_expr * value_type
  | HUnop of uop * hir_expr * value_type
  | HFunCall of hir_symbol * hir_expr list * value_type

(** HIR statements *)
and hir_stmt =
  | HAssign of hir_symbol * hir_expr
  | HDeclare of hir_symbol * value_type * hir_expr
  | HLet of hir_symbol * hir_expr * hir_stmt
  | HIf of hir_expr * hir_stmt * hir_stmt
  | HWhile of hir_expr * hir_stmt
  | HPrint of hir_expr
  | HBlock of hir_stmt list
  | HFunDecl of hir_symbol * (hir_symbol * value_type) list * value_type * hir_stmt
  | HReturn of hir_expr

(** Pretty printer for unary operators *)
val pp_uop : Ast.uop -> string

(** Pretty printer for binary operators *)
val pp_bop : Ast.bop -> string

(** Pretty printer for types *)
val pp_ty : Ast.value_type -> string

(** Pretty printer for HIR expressions *)
val pp_hir_expr : hir_expr -> string

(** Pretty printer for HIR statements *)
val pp_hir_stmt : hir_stmt -> string

(** [type_of_expr expr]
    Returns the Ast.value_type associated with the outermost operation
    of the given HIR expression.
*)
val type_of_expr : hir_expr -> Ast.value_type 