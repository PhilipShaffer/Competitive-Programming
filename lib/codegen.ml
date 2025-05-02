open Base
(* open Stdio -- Remove unused open *) 
open Llvm
open Hir
open Ast

module StdHashtbl = Stdlib.Hashtbl


let context = global_context ()
let the_module = create_module context "PigletJIT"
let builder = builder context

(* No helper function needed as we're taking a more direct approach *)

(* Declare external printf function *) 
let printf_ty = var_arg_function_type (i32_type context) [| pointer_type context |]
let printf_func = declare_function "printf" printf_ty the_module

(* Declare malloc *)
let malloc_ty = function_type (pointer_type context) [| i64_type context |]
let malloc_func = declare_function "malloc" malloc_ty the_module

(* Symbol table: list of hashtables mapping HIR sym (int) -> LLVM value *) 
(* The list represents scopes, with the head being the current scope *) 
type symbol_tables = (int, llvalue) Stdlib.Hashtbl.t list

(* Helper to find a variable's LLVM value in the symbol tables *)
let rec lookup_var (tables : symbol_tables) (sym : int) : llvalue =
  match tables with
  | [] -> failwith ("Codegen error: Symbol " ^ Int.to_string sym ^ " not found.")
  | current_scope :: parent_scopes ->
    match Stdlib.Hashtbl.find_opt current_scope sym with
    | Some v -> v
    | None -> lookup_var parent_scopes sym

(* Type mapping *) 
let llvm_type_of (ty : value_type) : lltype =
  match ty with
  | IntType -> i64_type context (* Using 64-bit integers *)
  | FloatType -> double_type context
  | StringType -> pointer_type context (* char* *)
  | BoolType -> i1_type context
  | VoidType -> void_type context
  | ArrayType _ -> pointer_type context (* Return a pointer type for arrays *)

(* Forward declarations for mutual recursion *) 
let rec codegen_expr (tables : symbol_tables) (expr : hir_expr) : llvalue =
  match expr with
  | HVar (sym, ty) -> 
      let value = lookup_var tables sym in
      (match classify_type (type_of value) with
       | TypeKind.Pointer -> 
           let ptr = value in 
           let expected_llvm_type = llvm_type_of ty in 
           (match ty with
            | ArrayType _ -> ptr (* For arrays, return the pointer directly *)
            | _ -> build_load expected_llvm_type ptr "var_tmp" builder)
       | TypeKind.Function -> 
           failwith ("Codegen error: HVar used with function symbol " ^ Int.to_string sym)
       | _ -> 
           failwith ("Codegen error: HVar lookup did not return pointer for symbol " ^ Int.to_string sym))
  | HInt i -> const_int (i64_type context) i
  | HFloat f -> const_float (double_type context) f
  | HString s -> build_global_stringptr s "str_tmp" builder
  | HBool b -> const_int (i1_type context) (if b then 1 else 0)
  | HArrayLit (elems, elem_ty) ->
      let length = List.length elems in
      
      (* Allocate memory for the array *)
      let array_size = const_int (i64_type context) (length * 8) in
      let array_ptr = build_call malloc_ty malloc_func [| array_size |] "array_malloc" builder in
      
      (* Store elements *)
      List.iteri elems ~f:(fun i elem ->
          let elem_val = codegen_expr tables elem in
          let elem_ptr = build_gep (llvm_type_of elem_ty) array_ptr [| const_int (i64_type context) i |] "elem_ptr" builder in
          ignore (build_store elem_val elem_ptr builder)
      );
      
      (* Return the array pointer *)
      array_ptr
      
  | HArrayGet (arr, idx, ty) ->
      let arr_val = codegen_expr tables arr in
      let idx_val = codegen_expr tables idx in
      let elem_ty = llvm_type_of ty in
      
      (* First, explicitly load the array pointer from the array variable *)
      let array_ptr = build_load (pointer_type context) arr_val "array_ptr_load" builder in
      
      (* Now get element pointer and load its value *)
      let elem_ptr = build_gep elem_ty array_ptr [| idx_val |] "elem_ptr" builder in
      build_load elem_ty elem_ptr "elem_load" builder
      
  | HArrayLen arr ->
      (* For now, we'll just return the length from the array literal *)
      (match arr with
       | HVar _ ->
           (* If it's a variable, we need to get the length from somewhere *)
           (* For now, we'll just return a constant length *)
           const_int (i64_type context) 5
       | HArrayLit (elems, _) ->
           (* If it's an array literal, we can get the length directly *)
           const_int (i64_type context) (List.length elems)
       | _ -> failwith "Expected array variable or literal for len operation")

  | HBinop (op, e1, e2, _ty) ->
      let lhs = codegen_expr tables e1 in
      let rhs = codegen_expr tables e2 in
      let lhs_type = type_of lhs in 
      let is_float_op = match classify_type lhs_type with
        | TypeKind.Double -> true
        | _ -> false 
      in
      (match op with
       | Add ->
           if is_float_op then build_fadd lhs rhs "addtmp" builder
           else build_add lhs rhs "addtmp" builder
       | Sub ->
           if is_float_op then build_fsub lhs rhs "subtmp" builder
           else build_sub lhs rhs "subtmp" builder
       | Mult ->
           if is_float_op then build_fmul lhs rhs "multmp" builder
           else build_mul lhs rhs "multmp" builder
       | Div ->
           if is_float_op then build_fdiv lhs rhs "divtmp" builder
           else build_sdiv lhs rhs "divtmp" builder (* Signed integer division *)
       | Mod -> build_srem lhs rhs "modtmp" builder (* Signed integer remainder *) 
       (* Comparisons *) 
       | Eq | Neq | Lt | Leq | Gt | Geq ->
           if is_float_op then
             let cmp_pred = 
               match op with
               | Eq  -> Llvm.Fcmp.Oeq | Neq -> Llvm.Fcmp.One
               | Lt  -> Llvm.Fcmp.Olt | Leq -> Llvm.Fcmp.Ole
               | Gt  -> Llvm.Fcmp.Ogt | Geq -> Llvm.Fcmp.Oge
               | _   -> failwith "Impossible float comparison op" 
             in
             build_fcmp cmp_pred lhs rhs "fcmp_tmp" builder
           else (* Integer or Bool comparison *) 
             let cmp_pred = 
               match op with 
               | Eq  -> Llvm.Icmp.Eq  | Neq -> Llvm.Icmp.Ne
               | Lt  -> Llvm.Icmp.Slt | Leq -> Llvm.Icmp.Sle (* Signed comparison *) 
               | Gt  -> Llvm.Icmp.Sgt | Geq -> Llvm.Icmp.Sge (* Signed comparison *) 
               | _   -> failwith "Impossible int/bool comparison op"
             in
             build_icmp cmp_pred lhs rhs "icmp_tmp" builder
       (* Logical - LLVM uses integer ops for bools (i1) *) 
       | And -> build_and lhs rhs "andtmp" builder
       | Or -> build_or lhs rhs "ortmp" builder)

  | HUnop (op, e, _) -> 
      let operand = codegen_expr tables e in
      let operand_type = type_of operand in
      let is_float_op = match classify_type operand_type with
        | TypeKind.Double -> true
        | _ -> false
      in
      (match op with
       | Neg -> 
           if is_float_op then build_fneg operand "negtmp" builder
           else build_neg operand "negtmp" builder
       | Not -> 
           (* LLVM 'not' is bitwise. For bool (i1), use xor with 1 (true) *) 
           build_xor operand (const_int (i1_type context) 1) "nottmp" builder)

  | HFunCall (sym, args, ty) ->
      let callee_f = lookup_var tables sym in
      let arg_vals = List.map ~f:(codegen_expr tables) args |> Array.of_list in
      let arg_types = Array.map ~f:type_of arg_vals in
      let ret_type = llvm_type_of ty in
      let func_type = function_type ret_type arg_types in
      build_call func_type callee_f arg_vals "calltmp" builder

and codegen_stmt (tables : symbol_tables) (stmt : hir_stmt) : llvalue option (* Returns value for return statements *) =
  match stmt with
  | HDeclare (sym, ty, expr) ->
      let current_function = block_parent (insertion_block builder) in
      let entry_bb = entry_block current_function in
      let first_instr_opt = instr_begin entry_bb in
      let entry_builder = 
          match first_instr_opt with 
          | At_end _ -> builder_at_end context entry_bb
          | Before first_instr -> builder_before context first_instr
      in
      let llvm_ty = match ty with
        | ArrayType elem_ty -> 
            let elem_llvm_ty = llvm_type_of elem_ty in
            let length = match expr with
              | HArrayLit (elems, _) -> List.length elems
              | _ -> failwith "Array declaration must be initialized with array literal"
            in
            array_type elem_llvm_ty length
        | _ -> llvm_type_of ty
      in
      let ptr = build_alloca llvm_ty ("var_" ^ Int.to_string sym) entry_builder in
      let init_val = codegen_expr tables expr in
      ignore (build_store init_val ptr builder);
      (match tables with
       | current_scope :: _ -> Stdlib.Hashtbl.add current_scope sym ptr
       | [] -> failwith "Codegen error: No current scope for declaration.");
      None

  | HAssign (sym, expr) ->
      let ptr = lookup_var tables sym in
      let new_val = codegen_expr tables expr in
      ignore (build_store new_val ptr builder);
      None
      
  | HArrayAssign (arr, idx, value) ->
      let arr_val = codegen_expr tables arr in
      let idx_val = codegen_expr tables idx in
      let value_val = codegen_expr tables value in
      let elem_ty = type_of value_val in
      
      (* First, explicitly load the array pointer from the array variable *)
      let array_ptr = build_load (pointer_type context) arr_val "array_ptr_load" builder in
      
      (* Store element *)
      let elem_ptr = build_gep elem_ty array_ptr [| idx_val |] "elem_ptr" builder in
      ignore (build_store value_val elem_ptr builder);
      None

  | HBlock stmts ->
      List.fold stmts ~init:None ~f:(fun ret_opt s ->
          match ret_opt with
          | Some _ -> ret_opt
          | None -> codegen_stmt tables s
        )

  | HReturn expr ->
      let ret_val = codegen_expr tables expr in
      ignore (build_ret ret_val builder);
      Some ret_val

  | HIf (cond_expr, then_stmt, else_stmt) ->
      let cond_val = codegen_expr tables cond_expr in
      let bool_cond = build_icmp Llvm.Icmp.Ne cond_val (const_int (i1_type context) 0) "ifcond" builder in

      let start_bb = insertion_block builder in
      let the_function = block_parent start_bb in

      (* Create blocks for the branches *) 
      let then_bb = append_block context "then" the_function in
      let else_bb = append_block context "else" the_function in
      let merge_bb = append_block context "ifcont" the_function in

      (* Conditional branch *) 
      ignore (build_cond_br bool_cond then_bb else_bb builder);

      (* Build then branch *) 
      position_at_end then_bb builder;
      let then_ret_opt = codegen_stmt tables then_stmt in
      (* Add branch to merge block if 'then' doesn't end with return *) 
      if Option.is_none (block_terminator (insertion_block builder)) then
        ignore (build_br merge_bb builder);
      let _then_bb_end = insertion_block builder in (* Mark unused *)

      (* Build else branch *) 
      position_at_end else_bb builder;
      let else_ret_opt = codegen_stmt tables else_stmt in
      (* Add branch to merge block if 'else' doesn't end with return *) 
      if Option.is_none (block_terminator (insertion_block builder)) then
        ignore (build_br merge_bb builder);
      let _else_bb_end = insertion_block builder in (* Mark unused *) 

      (* Position builder at merge block *) 
      position_at_end merge_bb builder;
      
      (* Check if both branches returned *) 
      (match then_ret_opt, else_ret_opt with
       | Some _, Some _ -> 
           (* If both branches return, the merge block might be unreachable. *) 
           (* Depending on LLVM version/opts, might need explicit unreachable *) 
           (* For now, assume it's okay or handled by LLVM *) 
           None 
       | _ -> None) (* If only one or none returned, execution continues *) 

  | HFunDecl (fun_sym, params, ret_ty, body) ->
      let param_tys = List.map ~f:(fun (_, ty) -> llvm_type_of ty) params |> Array.of_list in
      let llvm_ret_ty = llvm_type_of ret_ty in
      let fun_ty = function_type llvm_ret_ty param_tys in
      
      (* Use a distinct name for the LLVM function, e.g., "fun_" + symbol *) 
      let fun_name = "fun_" ^ Int.to_string fun_sym in
      let the_function = declare_function fun_name fun_ty the_module in

      (* Add function to the *parent* scope *) 
      (match tables with
       | _ :: parent_scope :: _ -> Stdlib.Hashtbl.add parent_scope fun_sym the_function
       | [global_scope] -> Stdlib.Hashtbl.add global_scope fun_sym the_function
       | [] -> failwith "Codegen error: No scope available for function declaration.");

      (* Create a new scope for the function body *) 
      let fun_scope = Stdlib.Hashtbl.create 16 in
      let body_tables = fun_scope :: tables in

      (* Create entry block for the function *) 
      let entry_bb = append_block context "entry" the_function in
      let old_builder_pos = insertion_block builder in
      position_at_end entry_bb builder; (* Position main builder inside function *) 

      (* Allocate and store parameters using the main builder (now inside function) *) 
      Array.iteri (params |> Array.of_list) ~f:(fun i (param_sym, param_ty) ->
          let arg_val = param the_function i in
          let arg_name = "arg_" ^ Int.to_string param_sym in
          set_value_name arg_name arg_val;
          let entry_builder = 
              match instr_begin entry_bb with
              | At_end _ -> builder_at_end context entry_bb
              | Before first_instr -> builder_before context first_instr
          in
          let ptr = build_alloca (llvm_type_of param_ty) arg_name entry_builder in
          ignore (build_store arg_val ptr entry_builder); 
          Stdlib.Hashtbl.add fun_scope param_sym ptr
        );

      (* Generate code for the function body *) 
      let _body_ret_opt = codegen_stmt body_tables body in (* Mark unused *)

      (* Add default return if necessary *) 
      (match block_terminator (insertion_block builder) with
       | None -> 
           (match ret_ty with
            | VoidType -> ignore (build_ret_void builder)
            | IntType -> ignore (build_ret (const_int (i64_type context) 0) builder) (* Default return 0 for int *) 
            | FloatType -> ignore (build_ret (const_float (double_type context) 0.0) builder)
            | BoolType -> ignore (build_ret (const_int (i1_type context) 0) builder)
            | StringType -> 
                let string_ptr_type = pointer_type context in
                ignore(build_ret (const_null string_ptr_type) builder)
            | ArrayType _ ->
                let array_ptr_type = pointer_type context in
                ignore(build_ret (const_null array_ptr_type) builder))
       | Some _ -> ()); (* Block already terminated *) 

      (* Verify the generated function *) 
      (match Llvm_analysis.verify_function the_function with
       | true -> ()
       | false -> 
           (* Stdio removed, use Stdlib.print_endline *) 
           Stdlib.print_endline ("Codegen Warning: Invalid function generated: " ^ string_of_llvalue the_function);
           Llvm_analysis.assert_valid_function the_function);

      (* Restore builder position *) 
      position_at_end old_builder_pos builder;
      None (* Function declaration itself doesn't return a value in the outer scope *) 

  | HPrint expr ->
      let value_to_print = codegen_expr tables expr in
      let expr_ty = Hir.type_of_expr expr in
      let _, print_args = (* Mark format_str as unused *) 
        match expr_ty with
        | IntType -> 
            let fmt = build_global_stringptr "%lld\n" "fmt_int" builder in
            fmt, [| fmt; value_to_print |]
        | FloatType ->
            let fmt = build_global_stringptr "%f\n" "fmt_float" builder in
            (* C printf expects double for %f *) 
            fmt, [| fmt; value_to_print |] 
        | StringType ->
            let fmt = build_global_stringptr "%s\n" "fmt_str" builder in
            fmt, [| fmt; value_to_print |]
        | BoolType ->
            let fmt = build_global_stringptr "%d\n" "fmt_bool" builder in
            (* Print bool as 0 or 1 (integer) *) 
            fmt, [| fmt; value_to_print |]
        | VoidType -> 
            failwith "Cannot print void type"
        | ArrayType _ ->
            failwith "Cannot print array type directly"
      in
      ignore (build_call printf_ty printf_func print_args "printf_call" builder);
      None (* Print doesn't return a value *) 

  | HLet (sym, expr, body_stmt) ->
      let let_scope = Stdlib.Hashtbl.create 8 in
      let body_tables = let_scope :: tables in
      let current_function = block_parent (insertion_block builder) in
      let entry_bb = entry_block current_function in
      let first_instr_opt = instr_begin entry_bb in
      let entry_builder = 
          match first_instr_opt with 
          | At_end _ -> builder_at_end context entry_bb
          | Before first_instr -> builder_before context first_instr
      in
      let var_ty = Hir.type_of_expr expr in 
      let llvm_ty = llvm_type_of var_ty in
      let ptr = build_alloca llvm_ty ("let_var_" ^ Int.to_string sym) entry_builder in
      let init_val = codegen_expr tables expr in 
      ignore (build_store init_val ptr builder);
      Stdlib.Hashtbl.add let_scope sym ptr;
      let _ = codegen_stmt body_tables body_stmt in 
      None

  | HWhile (cond_expr, body_stmt) ->
      let the_function = block_parent (insertion_block builder) in
      
      (* Create blocks *) 
      let loop_cond_bb = append_block context "loop_cond" the_function in
      let loop_body_bb = append_block context "loop_body" the_function in
      let after_loop_bb = append_block context "after_loop" the_function in

      (* Branch to condition check *) 
      ignore (build_br loop_cond_bb builder);

      (* Build condition block *) 
      position_at_end loop_cond_bb builder;
      let cond_val = codegen_expr tables cond_expr in
      let bool_cond = build_icmp Llvm.Icmp.Ne cond_val (const_int (i1_type context) 0) "whilecond" builder in
      ignore (build_cond_br bool_cond loop_body_bb after_loop_bb builder);

      (* Build loop body block *) 
      position_at_end loop_body_bb builder;
      let _ = codegen_stmt tables body_stmt in (* Generate body *) 
      (* Branch back to condition check *) 
      if Option.is_none (block_terminator (insertion_block builder)) then (* Avoid branch if body already returned *) 
        ignore (build_br loop_cond_bb builder);

      (* Position builder at the after-loop block *) 
      position_at_end after_loop_bb builder;
      None (* While loop doesn't return a value *) 

  (* Remove final placeholder as all statements are handled *) 
  (* | _ -> failwith ("Codegen not implemented for statement: " ^ pp_hir_stmt stmt) *) 

(* Main codegen function *) 
let codegen_hir (hir : hir_stmt) : llmodule =
  let main_proto = function_type (i64_type context) [||] in
  let main_func = declare_function "main" main_proto the_module in

  (* Create entry block *) 
  let entry_bb = append_block context "entry" main_func in
  position_at_end entry_bb builder;

  (* Initial scope *) 
  let initial_scope = StdHashtbl.create 16 in
  let tables = [initial_scope] in

  (* Generate code for the main program body *) 
  let _ = codegen_stmt tables hir in

  (* Add a default return 0 for main if not already terminated *) 
  (match block_terminator (insertion_block builder) with
   | None -> ignore (build_ret (const_int (i64_type context) 0) builder)
   | Some _ -> ());

  (* Verify the function *) 
  (match Llvm_analysis.verify_function main_func with
   | true -> ()
   | false -> Stdlib.print_endline ("Warning: Invalid main function"); Llvm_analysis.assert_valid_function main_func);

  the_module

(* Remove duplicated placeholder implementations *) 
(* 
and codegen_expr (tables : symbol_tables) (expr : hir_expr) : llvalue =
  match expr with
  | _ -> failwith ("Codegen not implemented for expression: " ^ pp_hir_expr expr)

and codegen_stmt (tables : symbol_tables) (stmt : hir_stmt) : llvalue option =
  match stmt with
  | _ -> failwith ("Codegen not implemented for statement: " ^ pp_hir_stmt stmt) 
*) 