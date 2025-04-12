open Llvm
open Base
open Common.Ast
open Common.Types

(* 
 * FUTURE IMPROVEMENT NOTES:
 * This code could be improved by implementing a more structured type system:
 * 1. Created a value_type enum to represent types (Int, Float, String, Bool) ✓
 * 2. Associate types with values in a single data structure ✓
 * 3. Use pattern matching for type checking instead of equality comparisons ✓
 * 4. Implement a unified get_expr_type function to centralize type determination ✓
 *)

(* Typed value structure to associate types with values *)
type typed_value = {
  value: llvalue;  (* The LLVM value *)
  typ: Common.Ast.value_type; (* The type of the value *)
}

let context = global_context ()
let the_module = create_module context "main"
let builder = builder context
let int_type = i64_type context
let float_type = double_type context (* double type chosen from LLVM - > Ocaml documenetation. float_type does NOT work*)
let string_type = pointer_type context

(* Create the global environment *)
let global_env = Common.Env.create_env ()

(* Helper function to get LLVM type from value_type *)
let llvm_type_of_value_type = function
  | Common.Ast.IntType -> int_type
  | Common.Ast.FloatType -> float_type
  | Common.Ast.StringType -> string_type
  | Common.Ast.BoolType -> int_type  (* Bool is represented as i32 where 0 is false, non-zero is true *)
;;

(* Create an alloca instruction in the entry block of the function. This
 * is used for mutable variables etc. *)
let create_entry_block_alloca the_function var_name typ =
  let builder =
    builder_at context (instr_begin (entry_block the_function))
  in
  build_alloca typ var_name builder
;;

(* Helper function to get type from type_info *)
let get_type expr = 
  match expr.type_info with
  | Some info -> info.expr_type
  | None -> raise (Failure "Expression missing type information")

(* Helper function to check if an expression is a string *)
let is_string_expr expr = Poly.(=) (get_type expr) Common.Ast.StringType

(* Helper function to check if an expression is a float *)
let is_float_expr expr = Poly.(=) (get_type expr) Common.Ast.FloatType

(* Helper function to extract the LLVM value from a typed_value *)
let get_llvm_value v = v.value

(* Helper function to get position string from type_info *)
let get_pos_str expr =
  match expr.type_info with
  | Some { pos = Some pos; _ } -> Printf.sprintf " at position %d" pos
  | _ -> ""

(* Enhanced error reporting with position *)
let error_with_pos msg expr =
  raise (Failure (Printf.sprintf "%s%s" msg (get_pos_str expr)))

let error_with_context msg expr op =
  let op_str = match op with
    | Add -> "addition"
    | Sub -> "subtraction"
    | Mult -> "multiplication"
    | Div -> "division"
    | Mod -> "modulo"
    | Lt -> "less than"
    | Leq -> "less than or equal"
    | Gt -> "greater than"
    | Geq -> "greater than or equal"
    | Eq -> "equality"
    | Neq -> "inequality"
    | And -> "logical and"
    | Or -> "logical or"
  in
  let type_str = type_to_string (get_type expr) in
  error_with_pos (Printf.sprintf "%s in %s operation on %s" msg op_str type_str) expr

(* Helper for generating comparison operations with better error handling *)
let codegen_comparison op lhs_val rhs_val expr ~is_float =
  try
    let cmp_builder = if is_float then
      match op with
      | Lt -> build_fcmp Fcmp.Olt
      | Leq -> build_fcmp Fcmp.Ole
      | Gt -> build_fcmp Fcmp.Ogt
      | Geq -> build_fcmp Fcmp.Oge
      | Eq -> build_fcmp Fcmp.Oeq
      | Neq -> build_fcmp Fcmp.One
      | _ -> error_with_pos "Invalid float comparison operator" expr
    else
      match op with
      | Lt -> build_icmp Icmp.Slt
      | Leq -> build_icmp Icmp.Sle
      | Gt -> build_icmp Icmp.Sgt
      | Geq -> build_icmp Icmp.Sge
      | Eq -> build_icmp Icmp.Eq
      | Neq -> build_icmp Icmp.Ne
      | _ -> error_with_pos "Invalid integer comparison operator" expr
    in
    build_zext (cmp_builder lhs_val rhs_val "cmptmp" builder) int_type "booltmp" builder
  with e ->
    error_with_context (Printf.sprintf "Failed to generate comparison: %s" (Exn.to_string e)) expr op

(* Helper for generating arithmetic operations with better error handling *)
let codegen_arithmetic op lhs_val rhs_val expr ~is_float =
  try
    if is_float then
      match op with
      | Add -> build_fadd lhs_val rhs_val "addtmp" builder
      | Sub -> build_fsub lhs_val rhs_val "subtmp" builder
      | Mult -> build_fmul lhs_val rhs_val "multmp" builder
      | Div -> build_fdiv lhs_val rhs_val "divtmp" builder
      | Mod -> build_frem lhs_val rhs_val "modtmp" builder
      | _ -> raise (Failure "Invalid float arithmetic operator")
    else
      match op with
      | Add -> build_add lhs_val rhs_val "addtmp" builder
      | Sub -> build_sub lhs_val rhs_val "subtmp" builder
      | Mult -> build_mul lhs_val rhs_val "multmp" builder
      | Div -> build_sdiv lhs_val rhs_val "divtmp" builder
      | Mod -> build_srem lhs_val rhs_val "modtmp" builder
      | _ -> raise (Failure "Invalid integer arithmetic operator")
  with e ->
    error_with_context (Printf.sprintf "Failed to generate arithmetic: %s" 
      (Exn.to_string e)) expr op

(* Helper for generating logical operations with better error handling *)
let codegen_logical op lhs_val rhs_val expr =
  try
    let to_bool val_ = build_icmp Icmp.Ne val_ (const_int int_type 0) "tobool" builder in
    let result = match op with
      | And -> build_and (to_bool lhs_val) (to_bool rhs_val) "andtmp" builder
      | Or -> build_or (to_bool lhs_val) (to_bool rhs_val) "ortmp" builder
      | _ -> raise (Failure "Invalid logical operator")
    in
    build_zext result int_type "booltmp" builder
  with e ->
    error_with_context (Printf.sprintf "Failed to generate logical operation: %s" 
      (Exn.to_string e)) expr op

let rec codegen_expr expr =
  match expr.expr with
  | Var name ->
      (match Common.Env.lookup global_env name with
       | Some entry ->
           (* Use type info from the annotated AST *)
           let typ = get_type expr in
           let value = Option.value_exn entry.value in
           { value = (match typ with
               | Common.Ast.StringType -> value
               | _ -> build_load (llvm_type_of_value_type typ) value name builder);
             typ = typ }
       | None -> error_with_pos (Printf.sprintf "Unknown variable: %s" name) expr)
  
  | Int n -> { value = const_int int_type n; typ = Common.Ast.IntType }
  | Float f -> { value = const_float float_type f; typ = Common.Ast.FloatType }
  | Bool b -> { value = const_int int_type (if b then 1 else 0); typ = Common.Ast.BoolType }
  | String s -> { value = build_global_stringptr s "str" builder; typ = Common.Ast.StringType }
  
  | Binop (op, lhs, rhs) ->
      let lhs_typed = codegen_expr lhs in
      let rhs_typed = codegen_expr rhs in
      let lhs_val = lhs_typed.value in
      let rhs_val = rhs_typed.value in
      
      (* Get the result type from the annotated AST *)
      let result_type = get_type expr in
      let lhs_type = get_type lhs in
      
      let result_value = match op with
        | Add | Sub | Mult | Div | Mod -> 
            if Poly.(=) result_type Common.Ast.FloatType then
              codegen_arithmetic op lhs_val rhs_val expr ~is_float:true
            else
              codegen_arithmetic op lhs_val rhs_val expr ~is_float:false
        | Lt | Leq | Gt | Geq | Eq | Neq ->
            if Poly.(=) lhs_type Common.Ast.FloatType then
              codegen_comparison op lhs_val rhs_val expr ~is_float:true
            else if Poly.(=) lhs_type Common.Ast.StringType then
              (match op with
               | Eq | Neq ->
                   let strcmp_type = function_type int_type [| string_type; string_type |] in
                   let strcmp = declare_function "strcmp" strcmp_type the_module in
                   let result = build_call strcmp_type strcmp [| lhs_val; rhs_val |] "strcmp" builder in
                   let cmp = if Poly.(=) op Eq
                            then build_icmp Icmp.Eq result (const_int int_type 0) "cmptmp" builder
                            else build_icmp Icmp.Ne result (const_int int_type 0) "cmptmp" builder in
                   build_zext cmp int_type "booltmp" builder
               | _ -> error_with_pos "Unsupported string comparison operation" expr)
            else
              codegen_comparison op lhs_val rhs_val expr ~is_float:false
        | And | Or -> codegen_logical op lhs_val rhs_val expr
      in
      { value = result_value; typ = result_type }
  
  | Unop (op, e) ->
      let expr_typed = codegen_expr e in
      let expr_val = expr_typed.value in
      let result_type = get_type expr in
      
      let result_value = match op, result_type with
        | Common.Ast.Neg, Common.Ast.FloatType -> 
            (try build_fneg expr_val "negtmp" builder
             with e -> error_with_pos (Printf.sprintf "Failed to negate float: %s" 
               (Exn.to_string e)) expr)
        | Common.Ast.Neg, Common.Ast.IntType -> 
            (try build_neg expr_val "negtmp" builder
             with e -> error_with_pos (Printf.sprintf "Failed to negate integer: %s" 
               (Exn.to_string e)) expr)
        | Common.Ast.Not, Common.Ast.BoolType -> 
            (try
               let cmp = build_icmp Icmp.Eq expr_val (const_int int_type 0) "nottmp" builder in
               build_zext cmp int_type "booltmp" builder
             with e -> error_with_pos (Printf.sprintf "Failed to generate logical not: %s" 
               (Exn.to_string e)) expr)
        | _ -> error_with_pos "Invalid unary operation" expr
      in
      { value = result_value; typ = result_type }

(* Codegen for statements *)
let rec codegen_stmt stmt =
  match stmt with
  | Assign (name, expr) ->
      let var = match Common.Env.lookup global_env name with
        | Some entry -> Option.value_exn entry.value
        | None -> raise (Failure (Printf.sprintf "Unknown variable: %s" name))
      in
      let value_typed = codegen_expr expr in
      let _ = build_store value_typed.value var builder in
      { value = const_int int_type 0; typ = Common.Ast.IntType }

  | Declare (name, _typ, init_expr) ->
      let init_typed = codegen_expr init_expr in
      (* Use type info from the annotated AST *)
      let declared_type = get_type init_expr in
      let var = create_entry_block_alloca (block_parent (insertion_block builder)) 
                                        name (llvm_type_of_value_type declared_type) in
      let _ = build_store init_typed.value var builder in
      let _ = Common.Env.add_var global_env name declared_type in
      let _ = Common.Env.update_var_value global_env name var in
      { value = const_int int_type 0; typ = Common.Ast.IntType }

  | Let (name, init_expr, body) ->
      let init_typed = codegen_expr init_expr in
      let old_binding = Common.Env.lookup global_env name in
      let var = create_entry_block_alloca (block_parent (insertion_block builder)) name (llvm_type_of_value_type init_typed.typ) in
      let _ = build_store init_typed.value var builder in
      let _ = Common.Env.add_var global_env name init_typed.typ in
      let _ = Common.Env.update_var_value global_env name var in
      let body_val = codegen_stmt body in
      (match old_binding with
       | Some old_var -> 
           let _ = Common.Env.add_var global_env name old_var.typ in
           let _ = Common.Env.update_var_value global_env name (Option.value_exn old_var.value) in
           ()
       | None -> 
           (* Just remove the current binding by setting it to None *)
           let _ = Common.Env.update_var_value global_env name (const_int int_type 0) in
           ());
      body_val

  | If (cond, then_stmt, else_stmt) ->
      (* Get the current function *)
      let the_function = block_parent (insertion_block builder) in
      
      (* Create basic blocks for then, else, and merge *)
      let then_bb = append_block context "then" the_function in
      let else_bb = append_block context "else" the_function in
      let merge_bb = append_block context "ifcont" the_function in
      
      (* Generate code for the condition *)
      let cond_typed = codegen_expr cond in
      let cond_val = cond_typed.value in
      
      let cond_val = build_icmp Icmp.Ne cond_val (const_int int_type 0) "ifcond" builder in
      let _ = build_cond_br cond_val then_bb else_bb builder in
      
      (* Emit then block *)
      position_at_end then_bb builder;
      let _ = codegen_stmt then_stmt in
      let _ = build_br merge_bb builder in
      
      (* Emit else block *)
      position_at_end else_bb builder;
      let _ = codegen_stmt else_stmt in
      let _ = build_br merge_bb builder in
      
      (* Emit merge block *)
      position_at_end merge_bb builder;
      { value = const_int int_type 0; typ = Common.Ast.IntType }

  | While (cond, body) ->
      (* Get the current function *)
      let the_function = block_parent (insertion_block builder) in
      
      (* Create basic blocks for condition, loop body, and after loop *)
      let cond_bb = append_block context "while.cond" the_function in
      let body_bb = append_block context "while.body" the_function in
      let after_bb = append_block context "while.end" the_function in
      
      (* Branch to condition block *)
      let _ = build_br cond_bb builder in
      
      (* Emit condition block *)
      position_at_end cond_bb builder;
      let cond_typed = codegen_expr cond in
      let cond_val = cond_typed.value in
      
      let cond_val = build_icmp Icmp.Ne cond_val (const_int int_type 0) "whilecond" builder in
      let _ = build_cond_br cond_val body_bb after_bb builder in
      
      (* Emit loop body *)
      position_at_end body_bb builder;
      let _ = codegen_stmt body in
      let _ = build_br cond_bb builder in
      
      (* Emit after loop *)
      position_at_end after_bb builder;
      { value = const_int int_type 0; typ = Common.Ast.IntType }

  | Print expr ->
      (* Evaluate the expression to get the value *)
      let value_typed = codegen_expr expr in
      let expr_type = get_type expr in
      
      (* Get the printf function - note it's a variadic function *)
      let printf_type = var_arg_function_type int_type [| string_type |] in
      let printf = declare_function "printf" printf_type the_module in
      
      (* Create the appropriate format string and arguments based on type *)
      let args = match expr_type with
        | Common.Ast.IntType -> 
           [| build_global_stringptr "%ld\n" "fmt" builder; 
              value_typed.value |]
        | Common.Ast.FloatType -> 
           [| build_global_stringptr "%f\n" "fmt" builder;
              value_typed.value |]
        | Common.Ast.BoolType ->
           [| build_global_stringptr "%s\n" "fmt" builder;
              build_select 
                (build_icmp Icmp.Ne value_typed.value (const_int int_type 0) "ifcond" builder)
                (build_global_stringptr "true" "true_str" builder)
                (build_global_stringptr "false" "false_str" builder)
                "bool_str" builder |]
        | Common.Ast.StringType ->
           [| build_global_stringptr "%s\n" "fmt" builder;
              value_typed.value |]
      in
      
      (* Call printf with the format string and value *)
      let _ = build_call printf_type printf args "printf" builder in
      { value = const_int int_type 0; typ = Common.Ast.IntType }

  | Block stmts ->
      (* Execute each statement in sequence *)
      List.fold stmts ~init:{ value = const_int int_type 0; typ = Common.Ast.IntType }
        ~f:(fun _ s -> codegen_stmt s)

(* Main code generation function *)
let compile stmt =
  (* Create main function *)
  let main_type = function_type int_type [| |] in
  let main_func = declare_function "main" main_type the_module in
  
  (* Create entry block *)
  let bb = append_block context "entry" main_func in
  position_at_end bb builder;
  
  (* Generate code for the statement *)
  let ret_val = codegen_stmt stmt in
  
  (* Return from main *)
  let _ = build_ret ret_val.value builder in
  
  (* Verify the module *)
  match Llvm_analysis.verify_module the_module with
  | None -> the_module
  | Some msg -> raise (Failure msg)
