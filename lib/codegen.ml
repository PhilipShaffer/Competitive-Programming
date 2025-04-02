open Llvm
open Base
open Ast

(* 
 * FUTURE IMPROVEMENT NOTES:
 * This code could be improved by implementing a more structured type system:
 * 1. Created a value_type enum to represent types (Int, Float, String, Bool) ✓
 * 2. Associate types with values in a single data structure ✓
 * 3. Use pattern matching for type checking instead of equality comparisons ✓
 * 4. Implement a unified get_expr_type function to centralize type determination ✓
 *)

(* Value type enum to represent types *)
type value_type =
  | IntType
  | FloatType
  | StringType
  | BoolType

(* Typed value structure to associate types with values *)
type typed_value = {
  value: llvalue;  (* The LLVM value *)
  typ: value_type; (* The type of the value *)
}

let context = global_context ()
let the_module = create_module context "main"
let builder = builder context
let named_values : (string, typed_value) Hashtbl.t = Hashtbl.create (module String)
let int_type = i32_type context
let float_type = float_type context
let string_type = pointer_type context

(* Helper function to get LLVM type from value_type *)
let llvm_type_of_value_type = function
  | IntType -> int_type
  | FloatType -> float_type
  | StringType -> string_type
  | BoolType -> int_type  (* Bool is represented as i32 where 0 is false, non-zero is true *)
;;

(* Create an alloca instruction in the entry block of the function. This
 * is used for mutable variables etc. *)
let create_entry_block_alloca the_function var_name typ =
  let builder =
    builder_at context (instr_begin (entry_block the_function))
  in
  build_alloca typ var_name builder
;;

(* Helper function to get the type of an expression *)
let rec get_expr_type = function
  | Int _ -> IntType
  | Float _ -> FloatType
  | String _ -> StringType
  | Bool _ -> BoolType
  | Var name -> 
      (match Hashtbl.find named_values name with
       | Some typed_val -> typed_val.typ
       | None -> IntType)  (* Default to IntType if not found *)
  | Binop (op, lhs, rhs) ->
      (match op with
       | Add | Sub | Mult | Div | Mod -> 
           (* Check if either operand is float, result is float, otherwise int *)
           if Poly.(=) (get_expr_type lhs) FloatType || Poly.(=) (get_expr_type rhs) FloatType then
             FloatType
           else
             IntType
       | Lt | Leq | Gt | Geq | Eq | Neq | And | Or ->
           (* Comparison operators always return boolean *)
           BoolType)
  | Unop (op, expr) ->
      (match op with
       | Neg -> 
           (* Negation preserves the type (float or int) *)
           let typ = get_expr_type expr in
           if Poly.(=) typ FloatType then FloatType else IntType
       | Not -> 
           (* Logical not always returns boolean *)
           BoolType)
;;

(* Helper function to check if an expression is a string *)
let is_string_expr expr = Poly.(=) (get_expr_type expr) StringType
;;

(* Helper function to check if an expression is a float *)
let is_float_expr expr = Poly.(=) (get_expr_type expr) FloatType
;;

(* Helper function to extract the LLVM value from a typed_value or directly use a raw llvalue *)
let get_llvm_value v = v.value
;;

let rec codegen_expr = function
  | Var name ->
    (* Look up the name in the symbol table *)
    (match Hashtbl.find named_values name with
     | Some value ->
        (* Determine how to handle the value based on its type *)
        (match value.typ with
         | StringType -> 
            (* For string variables, just return the pointer *)
            value
         | FloatType ->
            (* For float variables, load from the alloca *)
            { value = build_load float_type value.value name builder; typ = FloatType }
         | BoolType ->
            (* For boolean variables, load from the alloca *)
            { value = build_load int_type value.value name builder; typ = BoolType }
         | IntType ->
            (* For int variables, load from the alloca *)
            { value = build_load int_type value.value name builder; typ = IntType })
     | None -> raise (Failure ("unknown variable name: " ^ name)))
  
  | Int n -> 
      { value = const_int int_type n; typ = IntType }
  
  | Float f -> 
      { value = const_float float_type f; typ = FloatType }
  
  | Bool b -> 
      { value = const_int int_type (if b then 1 else 0); typ = BoolType }
  
  | String s ->
    (* For string literals, create a global string pointer *)
    { value = build_global_stringptr s "str" builder; typ = StringType }
  
  | Binop (op, lhs, rhs) ->
    let lhs_typed = codegen_expr lhs in
    let rhs_typed = codegen_expr rhs in
    let lhs_val = lhs_typed.value in
    let rhs_val = rhs_typed.value in
    
    (* Get the types of the operands *)
    let lhs_type = get_expr_type lhs in
    let rhs_type = get_expr_type rhs in
    
    (* Check if either operand is a string and handle accordingly *)
    if (match lhs_type, rhs_type with
        | StringType, _ | _, StringType -> true
        | _ -> false) then
      (* For strings, we currently only support equality comparison *)
      (match op with
       | Eq ->
          (* Use strcmp for string comparison *)
          let strcmp_type = function_type int_type [| string_type; string_type |] in
          let strcmp = declare_function "strcmp" strcmp_type the_module in
          let result = build_call strcmp_type strcmp [| lhs_val; rhs_val |] "strcmp" builder in
          (* strcmp returns 0 for equal strings, so we need to compare with 0 *)
          let cmp = build_icmp Icmp.Eq result (const_int int_type 0) "cmptmp" builder in
          { value = build_zext cmp int_type "booltmp" builder; typ = BoolType }
       | Neq ->
          (* Use strcmp for string comparison *)
          let strcmp_type = function_type int_type [| string_type; string_type |] in
          let strcmp = declare_function "strcmp" strcmp_type the_module in
          let result = build_call strcmp_type strcmp [| lhs_val; rhs_val |] "strcmp" builder in
          (* strcmp returns 0 for equal strings, so we need to compare with 0 *)
          let cmp = build_icmp Icmp.Ne result (const_int int_type 0) "cmptmp" builder in
          { value = build_zext cmp int_type "booltmp" builder; typ = BoolType }
       | _ -> raise (Failure "unsupported operation on strings"))
    (* Check if either operand is a float and handle accordingly *)
    else if (match lhs_type, rhs_type with
             | FloatType, _ | _, FloatType -> true
             | _ -> false) then
      (* For float operations, we need to convert both operands to float if they're not already *)
      let lhs_val = 
        if Poly.(=) lhs_type FloatType then lhs_val 
        else build_sitofp lhs_val float_type "float_cast" builder in
      let rhs_val = 
        if Poly.(=) rhs_type FloatType then rhs_val 
        else build_sitofp rhs_val float_type "float_cast" builder in
      
      (* Perform the float operation *)
      match op with
      | Add -> { value = build_fadd lhs_val rhs_val "addtmp" builder; typ = FloatType }
      | Sub -> { value = build_fsub lhs_val rhs_val "subtmp" builder; typ = FloatType }
      | Mult -> { value = build_fmul lhs_val rhs_val "multmp" builder; typ = FloatType }
      | Div -> { value = build_fdiv lhs_val rhs_val "divtmp" builder; typ = FloatType }
      | Mod -> { value = build_frem lhs_val rhs_val "modtmp" builder; typ = FloatType }
      | Lt -> 
         let cmp = build_fcmp Fcmp.Olt lhs_val rhs_val "cmptmp" builder in
         { value = build_zext cmp int_type "booltmp" builder; typ = BoolType }
      | Leq -> 
         let cmp = build_fcmp Fcmp.Ole lhs_val rhs_val "cmptmp" builder in
         { value = build_zext cmp int_type "booltmp" builder; typ = BoolType }
      | Gt -> 
         let cmp = build_fcmp Fcmp.Ogt lhs_val rhs_val "cmptmp" builder in
         { value = build_zext cmp int_type "booltmp" builder; typ = BoolType }
      | Geq -> 
         let cmp = build_fcmp Fcmp.Oge lhs_val rhs_val "cmptmp" builder in
         { value = build_zext cmp int_type "booltmp" builder; typ = BoolType }
      | Eq -> 
         let cmp = build_fcmp Fcmp.Oeq lhs_val rhs_val "cmptmp" builder in
         { value = build_zext cmp int_type "booltmp" builder; typ = BoolType }
      | Neq -> 
         let cmp = build_fcmp Fcmp.One lhs_val rhs_val "cmptmp" builder in
         { value = build_zext cmp int_type "booltmp" builder; typ = BoolType }
      | And ->
         (* Convert condition values to booleans, then perform logical AND *)
         let lhs_bool = build_fcmp Fcmp.One lhs_val (const_float float_type 0.0) "lhsbool" builder in
         let rhs_bool = build_fcmp Fcmp.One rhs_val (const_float float_type 0.0) "rhsbool" builder in
         let and_result = build_and lhs_bool rhs_bool "andtmp" builder in
         { value = build_zext and_result int_type "booltmp" builder; typ = BoolType }
      | Or ->
         (* Convert condition values to booleans, then perform logical OR *)
         let lhs_bool = build_fcmp Fcmp.One lhs_val (const_float float_type 0.0) "lhsbool" builder in
         let rhs_bool = build_fcmp Fcmp.One rhs_val (const_float float_type 0.0) "rhsbool" builder in
         let or_result = build_or lhs_bool rhs_bool "ortmp" builder in
         { value = build_zext or_result int_type "booltmp" builder; typ = BoolType }
    else
      (* Standard integer operations *)
      (match op with
       | Add -> { value = build_add lhs_val rhs_val "addtmp" builder; typ = IntType }
       | Sub -> { value = build_sub lhs_val rhs_val "subtmp" builder; typ = IntType }
       | Mult -> { value = build_mul lhs_val rhs_val "multmp" builder; typ = IntType }
       | Div -> { value = build_sdiv lhs_val rhs_val "divtmp" builder; typ = IntType }
       | Mod -> { value = build_srem lhs_val rhs_val "modtmp" builder; typ = IntType }
       | Lt -> 
          let cmp = build_icmp Icmp.Slt lhs_val rhs_val "cmptmp" builder in
          { value = build_zext cmp int_type "booltmp" builder; typ = BoolType }
       | Leq -> 
          let cmp = build_icmp Icmp.Sle lhs_val rhs_val "cmptmp" builder in
          { value = build_zext cmp int_type "booltmp" builder; typ = BoolType }
       | Gt -> 
          let cmp = build_icmp Icmp.Sgt lhs_val rhs_val "cmptmp" builder in
          { value = build_zext cmp int_type "booltmp" builder; typ = BoolType }
       | Geq -> 
          let cmp = build_icmp Icmp.Sge lhs_val rhs_val "cmptmp" builder in
          { value = build_zext cmp int_type "booltmp" builder; typ = BoolType }
       | Eq -> 
          let cmp = build_icmp Icmp.Eq lhs_val rhs_val "cmptmp" builder in
          { value = build_zext cmp int_type "booltmp" builder; typ = BoolType }
       | Neq -> 
          let cmp = build_icmp Icmp.Ne lhs_val rhs_val "cmptmp" builder in
          { value = build_zext cmp int_type "booltmp" builder; typ = BoolType }
       | And ->
          (* Convert condition values to booleans, then perform logical AND *)
          let lhs_bool = build_icmp Icmp.Ne lhs_val (const_int int_type 0) "lhsbool" builder in
          let rhs_bool = build_icmp Icmp.Ne rhs_val (const_int int_type 0) "rhsbool" builder in
          let and_result = build_and lhs_bool rhs_bool "andtmp" builder in
          { value = build_zext and_result int_type "booltmp" builder; typ = BoolType }
       | Or ->
          (* Convert condition values to booleans, then perform logical OR *)
          let lhs_bool = build_icmp Icmp.Ne lhs_val (const_int int_type 0) "lhsbool" builder in
          let rhs_bool = build_icmp Icmp.Ne rhs_val (const_int int_type 0) "rhsbool" builder in
          let or_result = build_or lhs_bool rhs_bool "ortmp" builder in
          { value = build_zext or_result int_type "booltmp" builder; typ = BoolType })
  
  | Unop (op, expr) ->
    let expr_typed = codegen_expr expr in
    let expr_val = expr_typed.value in
    let expr_type = get_expr_type expr in
    
    (match op with
     | Neg -> 
        if Poly.(=) expr_type FloatType then
          { value = build_fneg expr_val "negtmp" builder; typ = FloatType }
        else
          { value = build_neg expr_val "negtmp" builder; typ = IntType }
     | Not ->
        (* For logical not, we check if the expression is equal to 0 (false) *)
        if Poly.(=) expr_type FloatType then
          let cmp = build_fcmp Fcmp.Oeq expr_val (const_float float_type 0.0) "cmptmp" builder in
          { value = build_zext cmp int_type "booltmp" builder; typ = BoolType }
        else
          let cmp = build_icmp Icmp.Eq expr_val (const_int int_type 0) "cmptmp" builder in
          { value = build_zext cmp int_type "booltmp" builder; typ = BoolType })

and codegen_stmt = function
  | Assign (var, expr) ->
    (* Evaluate the expression to get the value *)
    let value_typed = codegen_expr expr in
    
    (* Look up the name or create it if this is the first use *)
    (match Hashtbl.find named_values var with
     | Some existing_var ->
        (* Existing variable, update its value *)
        (match existing_var.typ with
         | StringType ->
            (* For strings, just update the reference *)
            Hashtbl.set named_values ~key:var ~data:value_typed;
            value_typed
         | FloatType ->
            (* For floats, store in the alloca *)
            ignore (build_store value_typed.value existing_var.value builder);
            value_typed
         | BoolType ->
            (* For booleans, store in the alloca *)
            ignore (build_store value_typed.value existing_var.value builder);
            value_typed
         | IntType ->
            (* For ints, store in the alloca *)
            ignore (build_store value_typed.value existing_var.value builder);
            value_typed)
     | None ->
        (* New variable, handle based on type *)
        (match expr with
         | String _ ->
            (* For strings, just store the pointer directly *)
            Hashtbl.set named_values ~key:var ~data:value_typed;
            value_typed
         | Float _ ->
            (* For floats, allocate space and store *)
            let the_function = block_parent (insertion_block builder) in
            let alloca = create_entry_block_alloca the_function var float_type in
            ignore (build_store value_typed.value alloca builder);
            let typed_alloca = { value = alloca; typ = FloatType } in
            Hashtbl.set named_values ~key:var ~data:typed_alloca;
            value_typed
         | Bool _ ->
            (* For booleans, allocate space and store *)
            let the_function = block_parent (insertion_block builder) in
            let alloca = create_entry_block_alloca the_function var int_type in
            ignore (build_store value_typed.value alloca builder);
            let typed_alloca = { value = alloca; typ = BoolType } in
            Hashtbl.set named_values ~key:var ~data:typed_alloca;
            value_typed
         | Int _ ->
            (* For ints, allocate space and store *)
            let the_function = block_parent (insertion_block builder) in
            let alloca = create_entry_block_alloca the_function var int_type in
            ignore (build_store value_typed.value alloca builder);
            let typed_alloca = { value = alloca; typ = IntType } in
            Hashtbl.set named_values ~key:var ~data:typed_alloca;
            value_typed
         | _ ->
            (* For other types, allocate space and store *)
            let the_function = block_parent (insertion_block builder) in
            let alloca = create_entry_block_alloca the_function var int_type in
            ignore (build_store value_typed.value alloca builder);
            let typed_alloca = { value = alloca; typ = IntType } in
            Hashtbl.set named_values ~key:var ~data:typed_alloca;
            value_typed))
  
  | Let (var, expr, body) ->
    (* Evaluate the initializer *)
    let init_typed = codegen_expr expr in
    
    (* Save the old variable binding if it exists *)
    let old_binding = Hashtbl.find named_values var in
    
    (* Handle based on the expression type *)
    (match expr with
     | String _ ->
        (* For strings, just store the pointer directly *)
        Hashtbl.set named_values ~key:var ~data:init_typed;
     | Float _ ->
        (* For floats, create an alloca and store *)
        let the_function = block_parent (insertion_block builder) in
        let alloca = create_entry_block_alloca the_function var float_type in
        ignore (build_store init_typed.value alloca builder);
        let typed_alloca = { value = alloca; typ = FloatType } in
        Hashtbl.set named_values ~key:var ~data:typed_alloca;
     | Bool _ ->
        (* For booleans, create an alloca and store *)
        let the_function = block_parent (insertion_block builder) in
        let alloca = create_entry_block_alloca the_function var int_type in
        ignore (build_store init_typed.value alloca builder);
        let typed_alloca = { value = alloca; typ = BoolType } in
        Hashtbl.set named_values ~key:var ~data:typed_alloca;
     | Int _ ->
        (* For ints, create an alloca and store *)
        let the_function = block_parent (insertion_block builder) in
        let alloca = create_entry_block_alloca the_function var int_type in
        ignore (build_store init_typed.value alloca builder);
        let typed_alloca = { value = alloca; typ = IntType } in
        Hashtbl.set named_values ~key:var ~data:typed_alloca;
     | _ ->
        (* For other types, create an alloca and store *)
        let the_function = block_parent (insertion_block builder) in
        let alloca = create_entry_block_alloca the_function var int_type in
        ignore (build_store init_typed.value alloca builder);
        let typed_alloca = { value = alloca; typ = IntType } in
        Hashtbl.set named_values ~key:var ~data:typed_alloca);
    
    (* Generate code for the body *)
    let body_typed = codegen_stmt body in
    
    (* Restore the old binding if it existed *)
    (match old_binding with
     | Some old_value -> Hashtbl.set named_values ~key:var ~data:old_value
     | None -> Hashtbl.remove named_values var);
    
    (* Return the value of the body *)
    body_typed
  
  | If (cond, then_stmt, else_stmt) ->
    (* Generate code for the condition *)
    let cond_typed = codegen_expr cond in
    let cond_type = get_expr_type cond in
    
    (* Convert condition to a boolean value *)
    let cond_val = 
      if Poly.(=) cond_type FloatType then
        (* For float conditions, compare with 0.0 *)
        let zero = const_float float_type 0.0 in
        build_fcmp Fcmp.One cond_typed.value zero "ifcond" builder
      else
        (* For int conditions, compare with 0 *)
        let zero = const_int int_type 0 in
        build_icmp Icmp.Ne cond_typed.value zero "ifcond" builder
    in
    
    (* Get the current function *)
    let start_bb = insertion_block builder in
    let the_function = block_parent start_bb in
    
    (* Create blocks for the then, else, and merge cases *)
    let then_bb = append_block context "then" the_function in
    let else_bb = append_block context "else" the_function in
    let merge_bb = append_block context "ifcont" the_function in
    
    (* Create the conditional branch instruction *)
    ignore (build_cond_br cond_val then_bb else_bb builder);
    
    (* Generate code for the then branch *)
    position_at_end then_bb builder;
    let then_typed = codegen_stmt then_stmt in
    
    (* Branch to the merge block *)
    ignore (build_br merge_bb builder);
    
    (* Get the updated then block for the phi node *)
    let then_bb = insertion_block builder in
    
    (* Generate code for the else branch *)
    position_at_end else_bb builder;
    let else_typed = codegen_stmt else_stmt in
    
    (* Branch to the merge block *)
    ignore (build_br merge_bb builder);
    
    (* Get the updated else block for the phi node *)
    let else_bb = insertion_block builder in
    
    (* Generate code for the merge block - position first, then create PHI *)
    position_at_end merge_bb builder;
    
    (* Create a PHI node *)
    let phi = build_phi [(then_typed.value, then_bb); (else_typed.value, else_bb)] "iftmp" builder in
    
    { value = phi; typ = BoolType }
  
  | While (cond, body) ->
    (* Create the loop condition and body blocks *)
    let the_function = block_parent (insertion_block builder) in
    let cond_bb = append_block context "while.cond" the_function in
    let body_bb = append_block context "while.body" the_function in
    let after_bb = append_block context "while.end" the_function in
    
    (* Branch to the condition block *)
    ignore (build_br cond_bb builder);
    
    (* Generate code for the condition block *)
    position_at_end cond_bb builder;
    let cond_typed = codegen_expr cond in
    let cond_type = get_expr_type cond in
    
    let cond_val = 
      if Poly.(=) cond_type FloatType then
        (* For float conditions, compare with 0.0 *)
        let zero = const_float float_type 0.0 in
        build_fcmp Fcmp.One cond_typed.value zero "whilecond" builder
      else
        (* For int conditions, compare with 0 *)
        let zero = const_int int_type 0 in
        build_icmp Icmp.Ne cond_typed.value zero "whilecond" builder
    in
    ignore (build_cond_br cond_val body_bb after_bb builder);
    
    (* Generate code for the body block *)
    position_at_end body_bb builder;
    ignore (codegen_stmt body);
    
    (* Loop back to the condition block *)
    ignore (build_br cond_bb builder);
    
    (* Continue with the code after the loop *)
    position_at_end after_bb builder;
    
    (* Return a consistent value (0 for now) *)
    { value = const_int int_type 0; typ = IntType }
  
  | Print expr ->
    (* Evaluate the expression to get the value *)
    let value_typed = codegen_expr expr in
    let expr_type = get_expr_type expr in
    
    (* Get the printf function - note it's a variadic function *)
    let printf_type = var_arg_function_type int_type [| pointer_type context |] in
    let printf = declare_function "printf" printf_type the_module in
    
    (* Create appropriate format string based on expression type *)
    let format_str = 
      match expr_type with
      | StringType -> build_global_stringptr "%s\n" "fmt" builder
      | FloatType -> build_global_stringptr "%f\n" "fmt" builder
      | _ -> build_global_stringptr "%d\n" "fmt" builder
    in
    
    (* Call printf with the format string and value *)
    ignore (build_call printf_type printf [| format_str; value_typed.value |] "printf" builder);
    
    (* Return a constant 0 instead of the value, to avoid affecting program return *)
    { value = const_int int_type 0; typ = IntType }
  
  | Block stmts ->
    (* Execute each statement in the block and return the value of the last one *)
    let rec process_stmts = function
      | [] -> { value = const_int int_type 0; typ = IntType }  (* Empty block returns 0 *)
      | [last] -> codegen_stmt last  (* Last statement's value is returned *)
      | first :: rest ->
          ignore (codegen_stmt first);  (* Execute but ignore intermediate results *)
          process_stmts rest
    in
    process_stmts stmts

(* Top-level function to compile a program *)
let compile program =
  (* Create main function *)
  let main_ty = function_type int_type [| |] in
  let main_fn = declare_function "main" main_ty the_module in
  
  (* Create entry block in main function *)
  let entry = append_block context "entry" main_fn in
  position_at_end entry builder;
  
  (* Generate code for the program *)
  let _ = codegen_stmt program in
  
  (* Return 0 from main function (standard success exit code) *)
  ignore (build_ret (const_int int_type 0) builder);
  
  (* Verify the module *)
  Llvm_analysis.assert_valid_module the_module;
  
  the_module
