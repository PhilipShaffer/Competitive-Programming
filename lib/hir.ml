(* HIR types for the strict While language *)
open Ast
open Base

(* Unique symbol identifier *)
type hir_symbol = int

(* HIR expressions, always type-annotated *)
type hir_expr =
  | HVar of hir_symbol * value_type
  | HInt of int
  | HString of string
  | HFloat of float
  | HBool of bool
  | HBinop of bop * hir_expr * hir_expr * value_type
  | HUnop of uop * hir_expr * value_type
  | HFunCall of hir_symbol * hir_expr list * value_type
  | HArrayLit of hir_expr list * value_type  (* Array literal with element type *)
  | HArrayGet of hir_expr * hir_expr * value_type  (* Array access with element type *)
  | HArrayLen of hir_expr  (* Array length *)

(* HIR statements *)
type hir_stmt =
  | HAssign of hir_symbol * hir_expr
  | HArrayAssign of hir_expr * hir_expr * hir_expr  (* Array assignment: arr[idx] = value *)
  | HDeclare of hir_symbol * value_type * hir_expr
  | HLet of hir_symbol * hir_expr * hir_stmt
  | HIf of hir_expr * hir_stmt * hir_stmt
  | HWhile of hir_expr * hir_stmt
  | HPrint of hir_expr
  | HBlock of hir_stmt list
  | HFunDecl of hir_symbol * (hir_symbol * value_type) list * value_type * hir_stmt
  | HReturn of hir_expr

(* Helper function to get the type of an expression *) 
let type_of_expr (expr : hir_expr) : Ast.value_type =
  match expr with
  | HVar (_, ty) -> ty
  | HInt _ -> Ast.IntType
  | HFloat _ -> Ast.FloatType
  | HString _ -> Ast.StringType
  | HBool _ -> Ast.BoolType
  | HBinop (_, _, _, ty) -> ty
  | HUnop (_, _, ty) -> ty
  | HFunCall (_, _, ty) -> ty
  | HArrayLit (_, ty) -> ty
  | HArrayGet (_, _, ty) -> ty
  | HArrayLen _ -> Ast.IntType

(* Minimal pretty-printer for HIR - adapted from bin/main.ml *) 
let rec pp_hir_stmt (stmt : hir_stmt) : string =
  match stmt with
  | HAssign (sym, expr) -> Printf.sprintf "HAssign(%d, %s)" sym (pp_hir_expr expr)
  | HArrayAssign (arr, idx, value) -> Printf.sprintf "HArrayAssign(%s, %s, %s)" (pp_hir_expr arr) (pp_hir_expr idx) (pp_hir_expr value)
  | HDeclare (sym, ty, expr) -> Printf.sprintf "HDeclare(%d, %s, %s)" sym (pp_ty ty) (pp_hir_expr expr)
  | HLet (sym, expr, s) -> Printf.sprintf "HLet(%d, %s, %s)" sym (pp_hir_expr expr) (pp_hir_stmt s)
  | HIf (cond, t, f) -> Printf.sprintf "HIf(%s, %s, %s)" (pp_hir_expr cond) (pp_hir_stmt t) (pp_hir_stmt f)
  | HWhile (cond, body) -> Printf.sprintf "HWhile(%s, %s)" (pp_hir_expr cond) (pp_hir_stmt body)
  | HPrint expr -> Printf.sprintf "HPrint(%s)" (pp_hir_expr expr)
  | HBlock sl -> Printf.sprintf "HBlock([%s])" (String.concat ~sep:"; " (List.map ~f:pp_hir_stmt sl))
  | HFunDecl (sym, params, ret, body) ->
      let params_str = String.concat ~sep:", " (List.map ~f:(fun (s, t) -> Printf.sprintf "%d:%s" s (pp_ty t)) params) in
      Printf.sprintf "HFunDecl(%d, [%s], %s, %s)" sym params_str (pp_ty ret) (pp_hir_stmt body)
  | HReturn expr -> Printf.sprintf "HReturn(%s)" (pp_hir_expr expr)

and pp_hir_expr (expr : hir_expr) : string =
  match expr with
  | HVar (sym, ty) -> Printf.sprintf "HVar(%d:%s)" sym (pp_ty ty)
  | HInt i -> Printf.sprintf "HInt(%d)" i
  | HString s -> Printf.sprintf "HString(\"%s\")" s
  | HFloat f -> Printf.sprintf "HFloat(%f)" f
  | HBool b -> Printf.sprintf "HBool(%b)" b
  | HBinop (op, e1, e2, ty) -> Printf.sprintf "HBinop(%s, %s, %s, %s)" (pp_bop op) (pp_hir_expr e1) (pp_hir_expr e2) (pp_ty ty)
  | HUnop (op, e, ty) -> Printf.sprintf "HUnop(%s, %s, %s)" (pp_uop op) (pp_hir_expr e) (pp_ty ty)
  | HFunCall (sym, args, ty) ->
      let args_str = String.concat ~sep:", " (List.map ~f:pp_hir_expr args) in
      Printf.sprintf "HFunCall(%d, [%s], %s)" sym args_str (pp_ty ty)
  | HArrayLit (elems, ty) ->
      let elems_str = String.concat ~sep:", " (List.map ~f:pp_hir_expr elems) in
      Printf.sprintf "HArrayLit([%s], %s)" elems_str (pp_ty ty)
  | HArrayGet (arr, idx, ty) -> Printf.sprintf "HArrayGet(%s, %s, %s)" (pp_hir_expr arr) (pp_hir_expr idx) (pp_ty ty)
  | HArrayLen arr -> Printf.sprintf "HArrayLen(%s)" (pp_hir_expr arr)

and pp_ty (ty : Ast.value_type) : string =
  match ty with
  | Ast.IntType -> "int"
  | Ast.FloatType -> "float"
  | Ast.StringType -> "string"
  | Ast.BoolType -> "bool"
  | Ast.VoidType -> "void"
  | Ast.ArrayType elem_ty -> Printf.sprintf "%s[]" (pp_ty elem_ty)

and pp_bop (op : Ast.bop) : string =
  match op with
  | Ast.Add -> "+"
  | Ast.Sub -> "-"
  | Ast.Mult -> "*"
  | Ast.Div -> "/"
  | Ast.Mod -> "%"
  | Ast.Lt -> "<"
  | Ast.Leq -> "<="
  | Ast.Gt -> ">"
  | Ast.Geq -> ">="
  | Ast.Eq -> "=="
  | Ast.Neq -> "!="
  | Ast.And -> "and"
  | Ast.Or -> "or"

(* Add pp_uop here with the other recursive printers *) 
and pp_uop (op : Ast.uop) : string =
  match op with
  | Ast.Neg -> "-"
  | Ast.Not -> "not"
