open Llvm
open Base
open Ast

let context = global_context ()
let the_module = create_module context "main"
let builder = builder context
let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create (module String)
let var_types : (string, string) Hashtbl.t = Hashtbl.create (module String)  (* Store variable types *)
let int_type = i32_type context
let float_type = float_type context
let string_type = pointer_type context

(* Create an alloca instruction in the entry block of the function. This
 * is used for mutable variables etc. *)
 let create_entry_block_alloca the_function var_name typ =
  let builder =
    builder_at context (instr_begin (entry_block the_function))
  in
  build_alloca typ var_name builder
;;

(* Helper function to check if an expression is a string literal *)
let is_string_expr = function
  | String _ -> true
  | _ -> false
;;

(* Helper function to check if an expression is a float *)
let is_float_expr = function
  | Float _ -> true
  | Var name -> 
      (match Hashtbl.find var_types name with
       | Some "float" -> true
       | _ -> false)
  | _ -> false
;;

let rec codegen_expr = function
  | Var name ->
    (* Look up the name in the symbol table *)
    (match Hashtbl.find named_values name with
     | Some value ->
        (* Determine how to handle the value based on its type *)
        (match Hashtbl.find var_types name with
         | Some "string" -> 
            (* For string variables, just return the pointer *)
            value
         | Some "float" ->
            (* For float variables, load from the alloca *)
            build_load float_type value name builder
         | _ -> 
            (* For other variables, load from the alloca *)
            build_load int_type value name builder)
     | None -> raise (Failure ("unknown variable name: " ^ name)))
  
  | Int n -> const_int int_type n
  
  | Float f -> const_float float_type f
  
  | Bool b -> const_int int_type (if b then 1 else 0)
  
  | String s ->
    (* For string literals, create a global string pointer *)
    let str_val = build_global_stringptr s "str" builder in
    (* Remember that this is a string type *)
    str_val
  
  | Binop (op, lhs, rhs) ->
    let lhs_val = codegen_expr lhs in
    let rhs_val = codegen_expr rhs in
    
    (* Check if either operand is a string and handle accordingly *)
    if is_string_expr lhs || is_string_expr rhs then
      (* For strings, we currently only support equality comparison *)
      (match op with
       | Eq ->
          (* Use strcmp for string comparison *)
          let strcmp_type = function_type int_type [| string_type; string_type |] in
          let strcmp = declare_function "strcmp" strcmp_type the_module in
          let result = build_call strcmp_type strcmp [| lhs_val; rhs_val |] "strcmp" builder in
          (* strcmp returns 0 for equal strings, so we need to compare with 0 *)
          let cmp = build_icmp Icmp.Eq result (const_int int_type 0) "cmptmp" builder in
          build_zext cmp int_type "booltmp" builder
       | Neq ->
          (* Use strcmp for string comparison *)
          let strcmp_type = function_type int_type [| string_type; string_type |] in
          let strcmp = declare_function "strcmp" strcmp_type the_module in
          let result = build_call strcmp_type strcmp [| lhs_val; rhs_val |] "strcmp" builder in
          (* strcmp returns 0 for equal strings, so we need to compare with 0 *)
          let cmp = build_icmp Icmp.Ne result (const_int int_type 0) "cmptmp" builder in
          build_zext cmp int_type "booltmp" builder
       | _ -> raise (Failure "unsupported operation on strings"))
    (* Check if either operand is a float and handle accordingly *)
    else if is_float_expr lhs || is_float_expr rhs then
      (* For float operations, we need to convert both operands to float if they're not already *)
      let lhs_val = 
        if is_float_expr lhs then lhs_val 
        else build_sitofp lhs_val float_type "float_cast" builder in
      let rhs_val = 
        if is_float_expr rhs then rhs_val 
        else build_sitofp rhs_val float_type "float_cast" builder in
      
      (* Perform the float operation *)
      match op with
      | Add -> build_fadd lhs_val rhs_val "addtmp" builder
      | Sub -> build_fsub lhs_val rhs_val "subtmp" builder
      | Mult -> build_fmul lhs_val rhs_val "multmp" builder
      | Div -> build_fdiv lhs_val rhs_val "divtmp" builder
      | Mod -> build_frem lhs_val rhs_val "modtmp" builder
      | Lt -> 
         let cmp = build_fcmp Fcmp.Olt lhs_val rhs_val "cmptmp" builder in
         build_zext cmp int_type "booltmp" builder
      | Leq -> 
         let cmp = build_fcmp Fcmp.Ole lhs_val rhs_val "cmptmp" builder in
         build_zext cmp int_type "booltmp" builder
      | Gt -> 
         let cmp = build_fcmp Fcmp.Ogt lhs_val rhs_val "cmptmp" builder in
         build_zext cmp int_type "booltmp" builder
      | Geq -> 
         let cmp = build_fcmp Fcmp.Oge lhs_val rhs_val "cmptmp" builder in
         build_zext cmp int_type "booltmp" builder
      | Eq -> 
         let cmp = build_fcmp Fcmp.Oeq lhs_val rhs_val "cmptmp" builder in
         build_zext cmp int_type "booltmp" builder
      | Neq -> 
         let cmp = build_fcmp Fcmp.One lhs_val rhs_val "cmptmp" builder in
         build_zext cmp int_type "booltmp" builder
      | And ->
         (* Convert condition values to booleans, then perform logical AND *)
         let lhs_bool = build_fcmp Fcmp.One lhs_val (const_float float_type 0.0) "lhsbool" builder in
         let rhs_bool = build_fcmp Fcmp.One rhs_val (const_float float_type 0.0) "rhsbool" builder in
         let and_result = build_and lhs_bool rhs_bool "andtmp" builder in
         build_zext and_result int_type "booltmp" builder
      | Or ->
         (* Convert condition values to booleans, then perform logical OR *)
         let lhs_bool = build_fcmp Fcmp.One lhs_val (const_float float_type 0.0) "lhsbool" builder in
         let rhs_bool = build_fcmp Fcmp.One rhs_val (const_float float_type 0.0) "rhsbool" builder in
         let or_result = build_or lhs_bool rhs_bool "ortmp" builder in
         build_zext or_result int_type "booltmp" builder
    else
      (* Standard integer operations *)
      (match op with
       | Add -> build_add lhs_val rhs_val "addtmp" builder
       | Sub -> build_sub lhs_val rhs_val "subtmp" builder
       | Mult -> build_mul lhs_val rhs_val "multmp" builder
       | Div -> build_sdiv lhs_val rhs_val "divtmp" builder
       | Mod -> build_srem lhs_val rhs_val "modtmp" builder
       | Lt -> 
          let cmp = build_icmp Icmp.Slt lhs_val rhs_val "cmptmp" builder in
          build_zext cmp int_type "booltmp" builder
       | Leq -> 
          let cmp = build_icmp Icmp.Sle lhs_val rhs_val "cmptmp" builder in
          build_zext cmp int_type "booltmp" builder
       | Gt -> 
          let cmp = build_icmp Icmp.Sgt lhs_val rhs_val "cmptmp" builder in
          build_zext cmp int_type "booltmp" builder
       | Geq -> 
          let cmp = build_icmp Icmp.Sge lhs_val rhs_val "cmptmp" builder in
          build_zext cmp int_type "booltmp" builder
       | Eq -> 
          let cmp = build_icmp Icmp.Eq lhs_val rhs_val "cmptmp" builder in
          build_zext cmp int_type "booltmp" builder
       | Neq -> 
          let cmp = build_icmp Icmp.Ne lhs_val rhs_val "cmptmp" builder in
          build_zext cmp int_type "booltmp" builder
       | And ->
          (* Convert condition values to booleans, then perform logical AND *)
          let lhs_bool = build_icmp Icmp.Ne lhs_val (const_int int_type 0) "lhsbool" builder in
          let rhs_bool = build_icmp Icmp.Ne rhs_val (const_int int_type 0) "rhsbool" builder in
          let and_result = build_and lhs_bool rhs_bool "andtmp" builder in
          build_zext and_result int_type "booltmp" builder
       | Or ->
          (* Convert condition values to booleans, then perform logical OR *)
          let lhs_bool = build_icmp Icmp.Ne lhs_val (const_int int_type 0) "lhsbool" builder in
          let rhs_bool = build_icmp Icmp.Ne rhs_val (const_int int_type 0) "rhsbool" builder in
          let or_result = build_or lhs_bool rhs_bool "ortmp" builder in
          build_zext or_result int_type "booltmp" builder)
  
  | Unop (op, expr) ->
    let expr_val = codegen_expr expr in
    (match op with
     | Neg -> 
        if is_float_expr expr then
          build_fneg expr_val "negtmp" builder
        else
          build_neg expr_val "negtmp" builder
     | Not ->
        (* For logical not, we check if the expression is equal to 0 (false) *)
        if is_float_expr expr then
          let cmp = build_fcmp Fcmp.Oeq expr_val (const_float float_type 0.0) "cmptmp" builder in
          build_zext cmp int_type "booltmp" builder
        else
          let cmp = build_icmp Icmp.Eq expr_val (const_int int_type 0) "cmptmp" builder in
          build_zext cmp int_type "booltmp" builder)

and codegen_stmt = function
  | Assign (var, expr) ->
    (* Evaluate the expression to get the value *)
    let value = codegen_expr expr in
    
    (* Record the variable type *)
    (match expr with
     | String _ -> Hashtbl.set var_types ~key:var ~data:"string"
     | Float _ -> Hashtbl.set var_types ~key:var ~data:"float"
     | _ -> ());
    
    (* Look up the name or create it if this is the first use *)
    (match Hashtbl.find named_values var with
     | Some alloca ->
        (* Existing variable, update its value *)
        (match Hashtbl.find var_types var with
         | Some "string" ->
            (* For strings, just store the pointer *)
            Hashtbl.set named_values ~key:var ~data:value;
            value
         | Some "float" ->
            (* For floats, store in the alloca *)
            ignore (build_store value alloca builder);
            value
         | _ ->
            (* For other types, store in the alloca *)
            ignore (build_store value alloca builder);
            value)
     | None ->
        (* New variable, handle based on type *)
        (match expr with
         | String _ ->
            (* For strings, just store the pointer directly *)
            Hashtbl.set named_values ~key:var ~data:value;
            value
         | Float _ ->
            (* For floats, allocate space and store *)
            let the_function = block_parent (insertion_block builder) in
            let alloca = create_entry_block_alloca the_function var float_type in
            Hashtbl.set named_values ~key:var ~data:alloca;
            ignore (build_store value alloca builder);
            value
         | _ ->
            (* For other types, allocate space and store *)
            let the_function = block_parent (insertion_block builder) in
            let alloca = create_entry_block_alloca the_function var int_type in
            Hashtbl.set named_values ~key:var ~data:alloca;
            ignore (build_store value alloca builder);
            value))
  
  | Let (var, expr, body) ->
    (* Evaluate the initializer *)
    let init_val = codegen_expr expr in
    
    (* Record the variable type *)
    (match expr with
     | String _ -> Hashtbl.set var_types ~key:var ~data:"string"
     | Float _ -> Hashtbl.set var_types ~key:var ~data:"float"
     | _ -> ());
    
    (* Save the old variable binding and type if they exist *)
    let old_binding = Hashtbl.find named_values var in
    let old_type = Hashtbl.find var_types var in
    
    (* Handle based on the expression type *)
    (match expr with
     | String _ ->
        (* For strings, just store the pointer directly *)
        Hashtbl.set named_values ~key:var ~data:init_val;
     | Float _ ->
        (* For floats, create an alloca and store *)
        let the_function = block_parent (insertion_block builder) in
        let alloca = create_entry_block_alloca the_function var float_type in
        Hashtbl.set named_values ~key:var ~data:alloca;
        ignore (build_store init_val alloca builder);
     | _ ->
        (* For other types, create an alloca and store *)
        let the_function = block_parent (insertion_block builder) in
        let alloca = create_entry_block_alloca the_function var int_type in
        Hashtbl.set named_values ~key:var ~data:alloca;
        ignore (build_store init_val alloca builder));
    
    (* Generate code for the body *)
    let body_val = codegen_stmt body in
    
    (* Restore the old binding and type if they existed *)
    (match old_binding with
     | Some old_value -> Hashtbl.set named_values ~key:var ~data:old_value
     | None -> Hashtbl.remove named_values var);
    
    (match old_type with
     | Some old_type_val -> Hashtbl.set var_types ~key:var ~data:old_type_val
     | None -> Hashtbl.remove var_types var);
    
    (* Return the value of the body *)
    body_val
  
  | If (cond, then_stmt, else_stmt) ->
    (* Generate code for the condition *)
    let cond_val = codegen_expr cond in
    
    (* Convert condition to a boolean value *)
    let cond_val = 
      if is_float_expr cond then
        (* For float conditions, compare with 0.0 *)
        let zero = const_float float_type 0.0 in
        build_fcmp Fcmp.One cond_val zero "ifcond" builder
      else
        (* For int conditions, compare with 0 *)
        let zero = const_int int_type 0 in
        build_icmp Icmp.Ne cond_val zero "ifcond" builder
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
    let then_val = codegen_stmt then_stmt in
    
    (* Branch to the merge block *)
    ignore (build_br merge_bb builder);
    
    (* Get the updated then block for the phi node *)
    let then_bb = insertion_block builder in
    
    (* Generate code for the else branch *)
    position_at_end else_bb builder;
    let else_val = codegen_stmt else_stmt in
    
    (* Branch to the merge block *)
    ignore (build_br merge_bb builder);
    
    (* Get the updated else block for the phi node *)
    let else_bb = insertion_block builder in
    
    (* Generate code for the merge block - position first, then create PHI *)
    position_at_end merge_bb builder;
    
    (* Create a PHI node *)
    let phi = build_phi [(then_val, then_bb); (else_val, else_bb)] "iftmp" builder in
    
    phi
  
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
    let cond_val = codegen_expr cond in
    let cond_val = 
      if is_float_expr cond then
        (* For float conditions, compare with 0.0 *)
        let zero = const_float float_type 0.0 in
        build_fcmp Fcmp.One cond_val zero "whilecond" builder
      else
        (* For int conditions, compare with 0 *)
        let zero = const_int int_type 0 in
        build_icmp Icmp.Ne cond_val zero "whilecond" builder
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
    const_int int_type 0
  
  | Print expr ->
    (* Evaluate the expression to get the value to print *)
    let value = codegen_expr expr in
    
    (* Get the printf function - note it's a variadic function *)
    let printf_type = var_arg_function_type int_type [| pointer_type context |] in
    let printf = declare_function "printf" printf_type the_module in
    
    (* Create appropriate format string based on expression type *)
    let format_str, print_value = 
      match expr with
      | String _ -> 
          (* For string literals, use %s format and pass the string pointer directly *)
          build_global_stringptr "%s\n" "fmt" builder, value
      | Float _ ->
          (* For float literals, use %f format *)
          build_global_stringptr "%f\n" "fmt" builder, value
      | Var var_name ->
          (* For variables, check the type and format accordingly *)
          (match Hashtbl.find var_types var_name with
           | Some "string" ->
               (* For string variables, use %s format and pass the string pointer directly *)
               build_global_stringptr "%s\n" "fmt" builder, value
           | Some "float" ->
               (* For float variables, use %f format *)
               build_global_stringptr "%f\n" "fmt" builder, value
           | _ ->
               (* For other values, use %d format and pass the int value *)
               build_global_stringptr "%d\n" "fmt" builder, value)
      | _ -> 
          (* For other values, use %d format and pass the int value *)
          build_global_stringptr "%d\n" "fmt" builder, value
    in
    
    (* Call printf with the format string and value *)
    ignore (build_call printf_type printf [| format_str; print_value |] "printf" builder);
    
    (* Return a constant 0 instead of the value, to avoid affecting program return *)
    const_int int_type 0
  
  | Block stmts ->
    (* Execute each statement in the block and return the value of the last one *)
    let rec process_stmts = function
      | [] -> const_int int_type 0  (* Empty block returns 0 *)
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
