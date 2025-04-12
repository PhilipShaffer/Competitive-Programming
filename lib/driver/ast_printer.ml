open Common

let string_of_value_type = function
  | Ast.IntType -> "int"
  | Ast.FloatType -> "float"
  | Ast.StringType -> "string"
  | Ast.BoolType -> "bool"

let string_of_uop = function
  | Ast.Neg -> "-"
  | Ast.Not -> "!"

let string_of_bop = function
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
  | Ast.And -> "&&"
  | Ast.Or -> "||"

let indent n = String.make n ' '

let rec string_of_expr_with_type e n = 
  let type_str = match e.Ast.type_info with
    | Some ti -> " : " ^ string_of_value_type ti.expr_type
    | None -> ""
  in
  string_of_raw_expr_with_type e.Ast.expr n ^ type_str

and string_of_raw_expr_with_type expr n = match expr with
  | Ast.Var x -> indent n ^ "Var(" ^ x ^ ")"
  | Ast.Int n -> indent n ^ "Int(" ^ string_of_int n ^ ")"
  | Ast.String s -> indent n ^ "String(\"" ^ s ^ "\")"
  | Ast.Float f -> indent n ^ "Float(" ^ string_of_float f ^ ")"
  | Ast.Bool b -> indent n ^ "Bool(" ^ string_of_bool b ^ ")"
  | Ast.Binop (op, e1, e2) ->
      indent n ^ "Binop(" ^ string_of_bop op ^ ",\n" ^
      string_of_expr_with_type e1 (n + 2) ^ ",\n" ^
      string_of_expr_with_type e2 (n + 2) ^ ")"
  | Ast.Unop (op, e) ->
      indent n ^ "Unop(" ^ string_of_uop op ^ ",\n" ^
      string_of_expr_with_type e (n + 2) ^ ")"

let rec string_of_stmt_with_type stmt n = match stmt with
  | Ast.Assign (x, e) ->
      indent n ^ "Assign(\n" ^
      indent (n + 2) ^ "Var(" ^ x ^ "),\n" ^
      string_of_expr_with_type e (n + 2) ^ ")"
  | Ast.Declare (x, t, e) ->
      indent n ^ "Declare(\n" ^
      indent (n + 2) ^ "Var(" ^ x ^ ") : " ^ string_of_value_type t ^ ",\n" ^
      string_of_expr_with_type e (n + 2) ^ ")"
  | Ast.Let (x, e, s) ->
      indent n ^ "Let(\n" ^
      indent (n + 2) ^ "Var(" ^ x ^ "),\n" ^
      string_of_expr_with_type e (n + 2) ^ ",\n" ^
      string_of_stmt_with_type s (n + 2) ^ ")"
  | Ast.If (e, s1, s2) ->
      indent n ^ "If(\n" ^
      string_of_expr_with_type e (n + 2) ^ ",\n" ^
      string_of_stmt_with_type s1 (n + 2) ^ ",\n" ^
      string_of_stmt_with_type s2 (n + 2) ^ ")"
  | Ast.While (e, s) ->
      indent n ^ "While(\n" ^
      string_of_expr_with_type e (n + 2) ^ ",\n" ^
      string_of_stmt_with_type s (n + 2) ^ ")"
  | Ast.Print e ->
      indent n ^ "Print(\n" ^
      string_of_expr_with_type e (n + 2) ^ ")"
  | Ast.Block stmts ->
      indent n ^ "Block[\n" ^
      String.concat ",\n" (List.map (fun s -> string_of_stmt_with_type s (n + 2)) stmts) ^
      "\n" ^ indent n ^ "]"

let string_of_stmt stmt = string_of_stmt_with_type stmt 0

let print_stmt stmt = print_endline (string_of_stmt stmt) 