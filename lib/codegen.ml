open Llvm
open Ast


(* Typed value structure to associate types with values *)
type typed_value = {
  value: llvalue;  (* The LLVM value *)
  typ: Ast.value_type; (* The type of the value *)
}

let context = global_context ()
let the_module = create_module context "main"
let builder = builder context
let named_values : (string, typed_value) Base.Hashtbl.t = 
   Base.Hashtbl.create (module Base.String)
let int_type = i64_type context
let float_type = double_type context (* double type chosen from LLVM - > Ocaml documenetation. float_type does NOT work*)
let string_type = pointer_type context

(* Helper function to get LLVM type from value_type *)
let llvm_type_of_value_type = function
  | Ast.IntType -> int_type
  | Ast.FloatType -> float_type
  | Ast.StringType -> string_type
  | Ast.BoolType -> int_type  (* Bool is represented as i32 where 0 is false, non-zero is true *)
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
  | Int _ -> Ast.IntType
  | Float _ -> Ast.FloatType
  | String _ -> Ast.StringType
  | Bool _ -> Ast.BoolType
  | Call (name, _) ->
      (* For now, assume function calls return int type if not found *)
      (try
         match lookup_function name the_module with
         | Some f ->
             let ret_ty = return_type (type_of f) in
             if ret_ty = int_type then Ast.IntType
             else if ret_ty = float_type then Ast.FloatType
             else if ret_ty = string_type then Ast.StringType
             else Ast.IntType
         | None -> Ast.IntType
       with _ -> Ast.IntType)
  | Var name -> 
      (match Base.Hashtbl.find named_values name with
       | Some typed_val -> typed_val.typ
       | None -> Ast.IntType)  (* Default to IntType if not found *)
  | Binop (op, lhs, rhs) ->
      (match op with
       | Add | Sub | Mult | Div | Mod -> 
           (* Check if either operand is float, result is float, otherwise int *)
           if Base.Poly.(=) (get_expr_type lhs) Ast.FloatType || Base.Poly.(=) (get_expr_type rhs) Ast.FloatType then
            Ast.FloatType
           else
             Ast.IntType
       | Lt | Leq | Gt | Geq | Eq | Neq | And | Or ->
           (* Comparison operators always return boolean *)
           Ast.BoolType)
  | Unop (op, expr) ->
      (match op with
       | Neg -> 
           (* Negation preserves the type (float or int) *)
           let typ = get_expr_type expr in
           if Base.Poly.(=) typ Ast.FloatType then Ast.FloatType else Ast.IntType
       | Not -> 
           (* Logical not always returns boolean *)
           Ast.BoolType)
;;

(* Helper function to check if an expression is a string *)
let is_string_expr expr = Base.Poly.(=) (get_expr_type expr) Ast.StringType
;;

(* Helper function to check if an expression is a float *)
let is_float_expr expr = Base.Poly.(=) (get_expr_type expr) Ast.FloatType
;;

(* Helper function to extract the LLVM value from a typed_value or directly use a raw llvalue *)
let get_llvm_value v = v.value
;;

let rec codegen_expr = function
  | Var name ->
    (* Look up the name in the symbol table *)
    (match Base.Hashtbl.find named_values name with
     | Some value ->
        (* Determine how to handle the value based on its type *)
        (match value.typ with
         | Ast.StringType -> 
            (* For string variables, just return the pointer *)
            value
         | Ast.FloatType ->
            (* For float variables, load from the alloca *)
            { value = build_load float_type value.value name builder; typ = Ast.FloatType }
         | Ast.BoolType ->
            (* For boolean variables, load from the alloca *)
            { value = build_load int_type value.value name builder; typ = Ast.BoolType }
         | Ast.IntType ->
            (* For int variables, load from the alloca *)
            { value = build_load int_type value.value name builder; typ = Ast.IntType })
     | None -> raise (Failure ("unknown variable name: " ^ name)))
  
  | Int n -> 
      { value = const_int int_type n; typ = Ast.IntType }
  
  | Float f -> 
      { value = const_float float_type f; typ = Ast.FloatType }
  
  | Bool b -> 
      { value = const_int int_type (if b then 1 else 0); typ = Ast.BoolType }
  
  | String s ->
    (* For string literals, create a global string pointer *)
    { value = build_global_stringptr s "str" builder; typ = Ast.StringType }
  
  | Call (name, args) ->
      (* Look up the function name in the module table *)
      let callee =
        match lookup_function name the_module with
        | Some callee -> callee
        | None -> raise (Failure ("Unknown function: " ^ name))
      in
      
      (* Check correct argument count *)
      let param_count = Array.length (params callee) in
      if List.length args != param_count then
        raise (Failure (Printf.sprintf "Function %s called with wrong number of arguments (expected %d, got %d)" 
                         name param_count (List.length args)))
      else
        (* Generate code for each argument *)
        let arg_values = List.map codegen_expr args in
        let arg_llvm_values = List.map (fun v -> v.value) arg_values in
        
        (* Call the function *)
        let call_ty = type_of callee in
        let result = build_call call_ty callee (Array.of_list arg_llvm_values) "calltmp" builder in
        
        (* Determine return type *)
        let ret_ty = return_type call_ty in
        if ret_ty = int_type then
          { value = result; typ = Ast.IntType }
        else if ret_ty = float_type then
          { value = result; typ = Ast.FloatType }
        else if ret_ty = string_type then
          { value = result; typ = Ast.StringType }
        else
          { value = result; typ = Ast.IntType }
  
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
        | Ast.StringType, _ | _, Ast.StringType -> true
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
          { value = build_zext cmp int_type "booltmp" builder; typ = Ast.BoolType }
       | Neq ->
          (* Use strcmp for string comparison *)
          let strcmp_type = function_type int_type [| string_type; string_type |] in
          let strcmp = declare_function "strcmp" strcmp_type the_module in
          let result = build_call strcmp_type strcmp [| lhs_val; rhs_val |] "strcmp" builder in
          (* strcmp returns 0 for equal strings, so we need to compare with 0 *)
          let cmp = build_icmp Icmp.Ne result (const_int int_type 0) "cmptmp" builder in
          { value = build_zext cmp int_type "booltmp" builder; typ =Ast.BoolType }
       | _ -> raise (Failure "unsupported operation on strings"))
    (* Check if either operand is a float and handle accordingly *)
    else if (match lhs_type, rhs_type with
             | Ast.FloatType, Ast.FloatType -> true        (* Changed from ast.f, _ || _, ast.f to not allow float + int*)
             | _ -> false) then
(*     
      (* BRUG TIL TYPECASTING SENERE!!!!! *)
             (* For float operations, we need to convert both operands to float if they're not already *)
      let lhs_val = 
        if Poly.(=) lhs_type Ast.FloatType then lhs_val 
        else build_sitofp lhs_val float_type "float_cast" builder in 
      let rhs_val = 
        if Poly.(=) rhs_type Ast.FloatType then rhs_val 
        else build_sitofp rhs_val float_type "float_cast" builder in
*)     
      (* Perform the float operation *)
      match op with
      | Add -> { value = build_fadd lhs_val rhs_val "addtmp" builder; typ = Ast.FloatType }
      | Sub -> { value = build_fsub lhs_val rhs_val "subtmp" builder; typ = Ast.FloatType }
      | Mult -> { value = build_fmul lhs_val rhs_val "multmp" builder; typ = Ast.FloatType }
      | Div -> { value = build_fdiv lhs_val rhs_val "divtmp" builder; typ = Ast.FloatType }
      | Mod -> { value = build_frem lhs_val rhs_val "modtmp" builder; typ = Ast.FloatType }
      | Lt -> 
         let cmp = build_fcmp Fcmp.Olt lhs_val rhs_val "cmptmp" builder in
         { value = build_zext cmp int_type "booltmp" builder; typ = Ast.BoolType }
      | Leq -> 
         let cmp = build_fcmp Fcmp.Ole lhs_val rhs_val "cmptmp" builder in
         { value = build_zext cmp int_type "booltmp" builder; typ = Ast.BoolType }
      | Gt -> 
         let cmp = build_fcmp Fcmp.Ogt lhs_val rhs_val "cmptmp" builder in
         { value = build_zext cmp int_type "booltmp" builder; typ = Ast.BoolType }
      | Geq -> 
         let cmp = build_fcmp Fcmp.Oge lhs_val rhs_val "cmptmp" builder in
         { value = build_zext cmp int_type "booltmp" builder; typ = Ast.BoolType }
      | Eq -> 
         let cmp = build_fcmp Fcmp.Oeq lhs_val rhs_val "cmptmp" builder in
         { value = build_zext cmp int_type "booltmp" builder; typ = Ast.BoolType }
      | Neq -> 
         let cmp = build_fcmp Fcmp.One lhs_val rhs_val "cmptmp" builder in
         { value = build_zext cmp int_type "booltmp" builder; typ = Ast.BoolType }
      | And ->
         (* Convert condition values to booleans, then perform logical AND *)
         let lhs_bool = build_fcmp Fcmp.One lhs_val (const_float float_type 0.0) "lhsbool" builder in
         let rhs_bool = build_fcmp Fcmp.One rhs_val (const_float float_type 0.0) "rhsbool" builder in
         let and_result = build_and lhs_bool rhs_bool "andtmp" builder in
         { value = build_zext and_result int_type "booltmp" builder; typ = Ast.BoolType }
      | Or ->
         (* Convert condition values to booleans, then perform logical OR *)
         let lhs_bool = build_fcmp Fcmp.One lhs_val (const_float float_type 0.0) "lhsbool" builder in
         let rhs_bool = build_fcmp Fcmp.One rhs_val (const_float float_type 0.0) "rhsbool" builder in
         let or_result = build_or lhs_bool rhs_bool "ortmp" builder in
         { value = build_zext or_result int_type "booltmp" builder; typ = Ast.BoolType }
    else
      (* Standard integer operations *)
      (match op with
       | Add -> { value = build_add lhs_val rhs_val "addtmp" builder; typ = Ast.IntType }
       | Sub -> { value = build_sub lhs_val rhs_val "subtmp" builder; typ = Ast.IntType }
       | Mult -> { value = build_mul lhs_val rhs_val "multmp" builder; typ = Ast.IntType }
       | Div -> { value = build_sdiv lhs_val rhs_val "divtmp" builder; typ = Ast.IntType }
       | Mod -> { value = build_srem lhs_val rhs_val "modtmp" builder; typ = Ast.IntType }
       | Lt -> 
          let cmp = build_icmp Icmp.Slt lhs_val rhs_val "cmptmp" builder in
          { value = build_zext cmp int_type "booltmp" builder; typ = Ast.BoolType }
       | Leq -> 
          let cmp = build_icmp Icmp.Sle lhs_val rhs_val "cmptmp" builder in
          { value = build_zext cmp int_type "booltmp" builder; typ = Ast.BoolType }
       | Gt -> 
          let cmp = build_icmp Icmp.Sgt lhs_val rhs_val "cmptmp" builder in
          { value = build_zext cmp int_type "booltmp" builder; typ = Ast.BoolType }
       | Geq -> 
          let cmp = build_icmp Icmp.Sge lhs_val rhs_val "cmptmp" builder in
          { value = build_zext cmp int_type "booltmp" builder; typ = Ast.BoolType }
       | Eq -> 
          let cmp = build_icmp Icmp.Eq lhs_val rhs_val "cmptmp" builder in
          { value = build_zext cmp int_type "booltmp" builder; typ = Ast.BoolType }
       | Neq -> 
          let cmp = build_icmp Icmp.Ne lhs_val rhs_val "cmptmp" builder in
          { value = build_zext cmp int_type "booltmp" builder; typ = Ast.BoolType }
       | And ->
          (* Convert condition values to booleans, then perform logical AND *)
          let lhs_bool = build_icmp Icmp.Ne lhs_val (const_int int_type 0) "lhsbool" builder in
          let rhs_bool = build_icmp Icmp.Ne rhs_val (const_int int_type 0) "rhsbool" builder in
          let and_result = build_and lhs_bool rhs_bool "andtmp" builder in
          { value = build_zext and_result int_type "booltmp" builder; typ = Ast.BoolType }
       | Or ->
          (* Convert condition values to booleans, then perform logical OR *)
          let lhs_bool = build_icmp Icmp.Ne lhs_val (const_int int_type 0) "lhsbool" builder in
          let rhs_bool = build_icmp Icmp.Ne rhs_val (const_int int_type 0) "rhsbool" builder in
          let or_result = build_or lhs_bool rhs_bool "ortmp" builder in
          { value = build_zext or_result int_type "booltmp" builder; typ = Ast.BoolType })
  
  | Unop (op, expr) ->
    let expr_typed = codegen_expr expr in
    let expr_val = expr_typed.value in
    let expr_type = get_expr_type expr in
    
    (match op with
     | Neg -> 
        if Base.Poly.(=) expr_type Ast.FloatType then
          { value = build_fneg expr_val "negtmp" builder; typ = Ast.FloatType }
        else
          { value = build_neg expr_val "negtmp" builder; typ = Ast.IntType }
     | Not ->
        (* For logical not, we check if the expression is equal to 0 (false) *)
        if Base.Poly.(=) expr_type Ast.FloatType then
          let cmp = build_fcmp Fcmp.Oeq expr_val (const_float float_type 0.0) "cmptmp" builder in
          { value = build_zext cmp int_type "booltmp" builder; typ = Ast.BoolType }
        else
          let cmp = build_icmp Icmp.Eq expr_val (const_int int_type 0) "cmptmp" builder in
          { value = build_zext cmp int_type "booltmp" builder; typ = Ast.BoolType })

and codegen_stmt = function
  (* Updated Assign case to include type checking *)
  | Declare (var, declared_type, expr) ->
   (* Generate code for the initializer expression *)
   let init_typed = codegen_expr expr in

   (* Check if the declared type matches the initializer type *)
   if Base.Poly.(=) (declared_type : Ast.value_type) (init_typed.typ : Ast.value_type) then
     (match declared_type with
      | Ast.StringType ->
         (* For strings, just store the reference directly *)
         Base.Hashtbl.set named_values ~key:var ~data:init_typed;
         init_typed
      | Ast.FloatType ->
         (* Create an alloca for floats *)
         let the_function = block_parent (insertion_block builder) in
         let alloca = create_entry_block_alloca the_function var float_type in

         (* Add the variable to the symbol table *)
         let typed_alloca = { value = alloca; typ = Ast.FloatType } in
         Base.Hashtbl.set named_values ~key:var ~data:typed_alloca;

         (* Convert the initializer value to float if necessary *)
         let value_to_store =
           if Base.Poly.(=) init_typed.typ Ast.IntType then
             build_sitofp init_typed.value float_type "float_cast" builder
           else
             init_typed.value
         in

         (* Store the value in the allocated memory *)
         ignore (build_store value_to_store alloca builder);

         (* Return the initialized value *)
         { value = alloca; typ = Ast.FloatType }
      | _ ->
         (* Create an alloca for other types *)
         let the_function = block_parent (insertion_block builder) in
         let alloca = create_entry_block_alloca the_function var (llvm_type_of_value_type declared_type) in

         (* Add the variable to the symbol table *)
         let typed_alloca = { value = alloca; typ = declared_type } in
         Base.Hashtbl.set named_values ~key:var ~data:typed_alloca;

         (* Store the value in the allocated memory *)
         ignore (build_store init_typed.value alloca builder);

         (* Return the initialized value *)
         init_typed)
   else
     (* Raise an error if the types do not match *)
     raise (Failure (Printf.sprintf "Type mismatch in declaration: variable '%s' declared as %s but initialized with %s"
                       var
                       (match declared_type with
                        | Ast.IntType -> "int"
                        | Ast.FloatType -> "float"
                        | Ast.StringType -> "string"
                        | Ast.BoolType -> "bool")
                       (match init_typed.typ with
                        | Ast.IntType -> "int"
                        | Ast.FloatType -> "float"
                        | Ast.StringType -> "string"
                        | Ast.BoolType -> "bool")))
  | Assign (var, expr) ->
    (* Evaluate the expression to get the value *)
    let value_typed = codegen_expr expr in
    
    (* Look up the name in the symbol table *)
    (match Base.Hashtbl.find named_values var with
     | Some existing_var ->
        (* Check if the types match *)
        if Base.Poly.(=) existing_var.typ value_typed.typ then
          (* Types match, update the value *)
          (match existing_var.typ with
           | Ast.StringType ->
              (* For strings, just update the reference *)
              Base.Hashtbl.set named_values ~key:var ~data:value_typed;
              value_typed
           | _ ->
              (* For other types, store in the alloca *)
              ignore (build_store value_typed.value existing_var.value builder);
              value_typed)
        else
          (* Types do not match, raise an error *)
          raise (Failure (Printf.sprintf "Type mismatch: variable '%s' is of type %s but one or more of assigned values are of type %s"
                            var
                            (match existing_var.typ with
                             | Ast.IntType -> "Int"
                             | Ast.FloatType -> "Float"
                             | Ast.StringType -> "String"
                             | Ast.BoolType -> "Bool")
                            (match value_typed.typ with
                             | Ast.IntType -> "Int"
                             | Ast.FloatType -> "Float"
                             | Ast.StringType -> "String"
                             | Ast.BoolType -> "Bool")))
     | None ->
        (* Variable not declared, raise an error *)
        raise (Failure (Printf.sprintf "Variable '%s' not declared" var)))

  (* Updated Let case to include type checking *)
  | Let (var, expr, body) ->
    (* Evaluate the initializer *)
    let init_typed = codegen_expr expr in
    
    (* Save the old variable binding if it exists *)
    let old_binding = Base.Hashtbl.find named_values var in
    
    (* Handle based on the expression type *)
    (match expr with
     | String _ ->
        (* For strings, just store the pointer directly *)
        Base.Hashtbl.set named_values ~key:var ~data:init_typed;
     | Float _ ->
        (* For floats, create an alloca and store *)
        let the_function = block_parent (insertion_block builder) in
        let alloca = create_entry_block_alloca the_function var float_type in
        ignore (build_store init_typed.value alloca builder);
        let typed_alloca = { value = alloca; typ = Ast.FloatType } in
        Base.Hashtbl.set named_values ~key:var ~data:typed_alloca;
     | Bool _ ->
        (* For booleans, create an alloca and store *)
        let the_function = block_parent (insertion_block builder) in
        let alloca = create_entry_block_alloca the_function var int_type in
        ignore (build_store init_typed.value alloca builder);
        let typed_alloca = { value = alloca; typ = Ast.BoolType } in
        Base.Hashtbl.set named_values ~key:var ~data:typed_alloca;
     | Int _ ->
        (* For ints, create an alloca and store *)
        let the_function = block_parent (insertion_block builder) in
        let alloca = create_entry_block_alloca the_function var int_type in
        ignore (build_store init_typed.value alloca builder);
        let typed_alloca = { value = alloca; typ = Ast.IntType } in
        Base.Hashtbl.set named_values ~key:var ~data:typed_alloca;
     | _ ->
        (* For other types, create an alloca and store *)
        let the_function = block_parent (insertion_block builder) in
        let alloca = create_entry_block_alloca the_function var int_type in
        ignore (build_store init_typed.value alloca builder);
        let typed_alloca = { value = alloca; typ = Ast.IntType } in
        Base.Hashtbl.set named_values ~key:var ~data:typed_alloca);
    
    (* Generate code for the body *)
    let body_typed = codegen_stmt body in
    
    (* Restore the old binding if it existed *)
    (match old_binding with
     | Some old_value -> Base.Hashtbl.set named_values ~key:var ~data:old_value
     | None -> Base.Hashtbl.remove named_values var);
    
    (* Return the value of the body *)
    body_typed
  
  | If (cond, then_stmt, else_stmt) ->
    (* Generate code for the condition *)
    let cond_typed = codegen_expr cond in
    let cond_type = get_expr_type cond in
    
    (* Convert condition to a boolean value *)
    let cond_val = 
      if Base.Poly.(=) cond_type Ast.FloatType then
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
    
    { value = phi; typ = Ast.BoolType }
  
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
      if Base.Poly.(=) cond_type Ast.FloatType then
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
    { value = const_int int_type 0; typ = Ast.IntType }
  
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
      | Ast.StringType -> build_global_stringptr "%s\n" "fmt" builder
      | Ast.FloatType -> build_global_stringptr "%f\n" "fmt" builder
      | _ -> build_global_stringptr "%d\n" "fmt" builder
    in
    
    (* Call printf with the format string and value *)
    ignore (build_call printf_type printf [| format_str; value_typed.value |] "printf" builder);
    
    (* Return a constant 0 instead of the value, to avoid affecting program return *)
    { value = const_int int_type 0; typ = Ast.IntType }
  
  | Return None ->
      (* Return void *)
      ignore (build_ret_void builder);
      
      (* Return a dummy value since control flow ends *)
      { value = const_int int_type 0; typ = Ast.IntType }
      
  | Return (Some expr) ->
      (* Check if we're inside a function *)
      let current_fn = block_parent (insertion_block builder) in
      
      (* Generate code for the return value *)
      let ret_val = codegen_expr expr in
      
      (* Get function return type *)
      let ret_ty = return_type (type_of current_fn) in
      
      (* Convert the return value to the function's return type if needed *)
      let ret_val_to_return =
        if ret_ty = float_type && ret_val.typ = Ast.IntType then
          build_sitofp ret_val.value float_type "float_cast" builder
        else
          ret_val.value
      in
      
      (* Create the return instruction *)
      ignore (build_ret ret_val_to_return builder);
      
      (* Return a dummy value since control flow ends *)
      { value = ret_val.value; typ = ret_val.typ }
  
  | Block stmts ->
    (* Execute each statement in the block and return the value of the last one *)
    let rec process_stmts = function
      | [] -> { value = const_int int_type 0; typ = Ast.IntType }  (* Empty block returns 0 *)
      | [last] -> codegen_stmt last  (* Last statement's value is returned *)
      | first :: rest ->
          ignore (codegen_stmt first);  (* Execute but ignore intermediate results *)
          process_stmts rest
    in
    process_stmts stmts

  | Function (name, params, ret_type_opt, body) ->
      (* Check if function already exists in module *)
      match lookup_function name the_module with
      | Some _ -> raise (Failure ("Function already defined: " ^ name))
      | None ->
          (* Create return type *)
          let ret_type_llvm = match ret_type_opt with
            | Some ret_type -> llvm_type_of_value_type ret_type
            | None -> int_type  (* Default to int if no return type specified *)
          in
          
          (* Create parameter types *)
          let param_types = 
            Array.of_list (List.map (fun (_, ty) -> llvm_type_of_value_type ty) params)
          in
          
          (* Create function type *)
          let func_type = function_type ret_type_llvm param_types in
          
          (* Create function *)
          let the_function = declare_function name func_type the_module in
          
          (* Create a new basic block to start insertion into *)
          let bb = append_block context "entry" the_function in
          position_at_end bb builder;
          
          (* Save old bindings to restore after function generation *)
          let old_bindings = Base.Hashtbl.to_alist named_values in
          
          (* Clear out the symbol table for new function *)
          Base.Hashtbl.clear named_values;
          
          (* Create parameter allocas and store values *)
          let param_idx = ref 0 in
          iter_params (fun llvm_param ->
              if !param_idx < List.length params then
                let (param_name, param_type) = List.nth params !param_idx in
                
                (* Create alloca for this variable *)
                let llvm_type = llvm_type_of_value_type param_type in
                let alloca = create_entry_block_alloca the_function param_name llvm_type in
                
                (* Store the initial value into the alloca *)
                ignore (build_store llvm_param alloca builder);
                
                (* Add arguments to the symbol table *)
                Base.Hashtbl.set named_values ~key:param_name ~data:{ value = alloca; typ = param_type };
                
                (* Move to next parameter *)
                param_idx := !param_idx + 1
          ) the_function;
          
          (* Codegen function body *)
          ignore (codegen_stmt body);
          
          (* Add a return instruction if the last instruction isn't already a return *)
          let bb = insertion_block builder in
          
          (* Check if the current block has a terminator *)
          (match block_terminator bb with
           | Some _ -> () (* Block already has a terminator, don't add a return *)
           | None ->
               (* No terminator, add a return instruction *)
               if ret_type_llvm = void_type context then
                 ignore (build_ret_void builder)
               else
                 (* For non-void functions without explicit return, return a default value *)
                 let default_ret_val = 
                   if ret_type_llvm = int_type then
                     const_int int_type 0
                   else if ret_type_llvm = float_type then
                     const_float float_type 0.0
                   else if ret_type_llvm = string_type then
                     build_global_stringptr "" "default_ret" builder
                   else
                     const_int int_type 0
                 in
                 ignore (build_ret default_ret_val builder)
          );
          
          (* Restore previous named values *)
          Base.Hashtbl.clear named_values;
          List.iter 
            (fun (name, value) -> Base.Hashtbl.set named_values ~key:name ~data:value)
            old_bindings;
          
          (* Verify generated code is well formed *)
          ignore (Llvm_analysis.verify_function the_function);
          
          { value = const_int int_type 0; typ = Ast.IntType }

(* Top-level function to compile a program *)
let compile program =
  match program with
  | Block stmts -> 
      (* Create main function *)
      let main_ty = function_type int_type [| |] in
      let main_fn = declare_function "main" main_ty the_module in
      
      (* Create entry block in main function *)
      let entry = append_block context "entry" main_fn in
      position_at_end entry builder;
      
      (* Generate code for each statement *)
      List.iter (fun stmt -> ignore (codegen_stmt stmt)) stmts;
      
      (* Return 0 from main function (standard success exit code) *)
      ignore (build_ret (const_int int_type 0) builder);
      
      (* Verify the module *)
      Llvm_analysis.assert_valid_module the_module;
      
      the_module
  | _ -> failwith "Expected program to be a block of statements"
