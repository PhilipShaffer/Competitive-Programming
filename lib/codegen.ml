open Base
(* open Stdio -- Remove unused open *) 
open Llvm
open Hir
open Ast

module StdHashtbl = Stdlib.Hashtbl

(* Notes:
 * - We use LLVM's i64 for all integer types, including bools.
 * - We use LLVM's double for float types.
 * - We use 8 bytes for all types in our arrays, which might be a problem for strings.
 * - Notice that the function "pointer_type" automatically uses the correct pointer size for the target architecture, so we don't have to cast it.
 *)

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
      
      (* C-compatible array implementation:
         We'll create a struct { size_t length; element_type data[]; }
         This keeps the array length with the array pointer for bounds checking *)
      
      (* First, allocate memory for length + elements 
         (8 bytes for length + element_size * length) *)
      let elem_size = 8 (* All our types use 64 bits/8 bytes in this implementation *) in
      let array_size = const_int (i64_type context) (8 + (length * elem_size)) in
      let array_mem = build_call malloc_ty malloc_func [| array_size |] "array_malloc" builder in
      
      (* Cast to appropriate pointer type *)
      let array_ptr = build_pointercast array_mem (pointer_type context) "array_ptr" builder in
      
      (* Store length at the beginning *)
      let length_ptr = build_bitcast array_ptr (pointer_type context) "length_ptr" builder in
      ignore (build_store (const_int (i64_type context) length) length_ptr builder);
      
      (* Calculate pointer to the first element (after the length) *)
      let data_ptr = build_gep (i64_type context) length_ptr [| const_int (i64_type context) 1 |] "data_ptr" builder in
      let data_ptr_cast = build_bitcast data_ptr (pointer_type context) "data_ptr_cast" builder in
      
      (* Store elements *)
      List.iteri elems ~f:(fun i elem ->
          let elem_val = codegen_expr tables elem in
          let elem_ptr = build_gep (llvm_type_of elem_ty) data_ptr_cast [| const_int (i64_type context) i |] "elem_ptr" builder in
          ignore (build_store elem_val elem_ptr builder)
      );
      
      (* Return the array pointer (points to the struct) *)
      array_ptr
      
  | HArrayGet (arr, idx, ty, bounds_checked) ->
      let arr_val = codegen_expr tables arr in
      let idx_val = codegen_expr tables idx in
      let elem_ty = llvm_type_of ty in
      
      (* Load the array pointer from the array variable *)
      let array_ptr = 
        match arr with
        | HVar _ -> build_load (pointer_type context) arr_val "array_ptr_load" builder
        | _ -> arr_val
      in
      
      (* Get array length for bounds checking and for computing data pointer *)
      let length_ptr = build_bitcast array_ptr (pointer_type context) "length_ptr" builder in
      
      (* Calculate pointer to the data part (after the length field) *)
      let data_ptr = build_gep (i64_type context) length_ptr [| const_int (i64_type context) 1 |] "data_ptr" builder in
      let data_ptr_cast = build_bitcast data_ptr (pointer_type context) "data_ptr_cast" builder in
      
      if bounds_checked then
        (* Skip runtime bounds checking if already verified at compile time *)
        let elem_ptr = build_gep elem_ty data_ptr_cast [| idx_val |] "elem_ptr" builder in
        build_load elem_ty elem_ptr "elem_load" builder
      else
        (* Need runtime bounds checking *)
        let length = build_load (i64_type context) length_ptr "array_length" builder in
        
        (* Add bounds checking *)
        (* First check if index is negative *)
        let zero = const_int (i64_type context) 0 in
        let is_negative = build_icmp Llvm.Icmp.Slt idx_val zero "is_negative" builder in
        
        (* Check if index >= length *)
        let is_too_large = build_icmp Llvm.Icmp.Sge idx_val length "is_too_large" builder in
        
        (* Combine both bounds checks *)
        let is_out_of_bounds = build_or is_negative is_too_large "is_out_of_bounds" builder in
        
        (* Create blocks for bounds checking *)
        let current_block = insertion_block builder in
        let parent = block_parent current_block in
        let out_of_bounds_block = append_block context "out_of_bounds" parent in
        let in_bounds_block = append_block context "in_bounds" parent in
        
        (* Branch based on bounds check *)
        ignore (build_cond_br is_out_of_bounds out_of_bounds_block in_bounds_block builder);
        
        (* Set up out of bounds block with error message *)
        position_at_end out_of_bounds_block builder;
        
        (* Create formatted error message that shows the index and array length *)
        let idx_ptr = build_alloca (i64_type context) "idx_ptr" builder in
        ignore (build_store idx_val idx_ptr builder);
        let length_val_ptr = build_alloca (i64_type context) "length_val_ptr" builder in
        ignore (build_store length length_val_ptr builder);
        
        let err_msg = build_global_stringptr "[Codegen] Array index out of bounds: index %lld, array length %lld\n" "bounds_err_msg" builder in
        
        (* Print error message with index and length values *)
        ignore (build_call printf_ty printf_func [| err_msg; idx_val; length |] "print_error" builder);
        
        (* Exit with error code *)
        let exit_func = declare_function "exit" (function_type (void_type context) [| i32_type context |]) the_module in
        ignore (build_call (function_type (void_type context) [| i32_type context |]) exit_func [| const_int (i32_type context) 1 |] "" builder);
        ignore (build_unreachable builder);
        
        (* Continue with actual access in the in-bounds case *)
        position_at_end in_bounds_block builder;
        
        (* Get element pointer and load its value *)
        let elem_ptr = build_gep elem_ty data_ptr_cast [| idx_val |] "elem_ptr" builder in
        build_load elem_ty elem_ptr "elem_load" builder
      
  | HArrayLen arr ->
      let arr_val = codegen_expr tables arr in
      
      (* For our C-compatible array implementation:
         We stored the length at the beginning of the allocated memory
         So we need to load this value *)
      let array_ptr = 
        match arr with
        | HVar _ -> build_load (pointer_type context) arr_val "array_ptr_load" builder
        | _ -> arr_val
      in
      
      (* Cast to int64 pointer to access the length field *)
      let length_ptr = build_bitcast array_ptr (pointer_type context) "length_ptr" builder in
      
      (* Load and return the length value *)
      build_load (i64_type context) length_ptr "array_length" builder

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
      
  | HArrayAssign (arr, idx, value, bounds_checked) ->
      let arr_val = codegen_expr tables arr in
      let idx_val = codegen_expr tables idx in
      let value_val = codegen_expr tables value in
      let elem_ty = type_of value_val in
      
      (* First, explicitly load the array pointer from the array variable *)
      let array_ptr = build_load (pointer_type context) arr_val "array_ptr_load" builder in
      
      (* Get length pointer for data pointer calculation *)
      let length_ptr = build_bitcast array_ptr (pointer_type context) "length_ptr" builder in
      
      (* Calculate pointer to the first element (after the length) *)
      let data_ptr = build_gep (i64_type context) length_ptr [| const_int (i64_type context) 1 |] "data_ptr" builder in
      let data_ptr_cast = build_bitcast data_ptr (pointer_type context) "data_ptr_cast" builder in
      
      if bounds_checked then
        (* Skip runtime bounds checking if already verified at compile time *)
        let elem_ptr = build_gep elem_ty data_ptr_cast [| idx_val |] "elem_assign_ptr" builder in
        ignore (build_store value_val elem_ptr builder);
        None
      else
        (* Need runtime bounds checking *)
        (* Get length for bounds checking *)
        let length = build_load (i64_type context) length_ptr "array_length" builder in
        
        (* Add bounds checking - compare with array length *)
        (* First check if index is negative *)
        let zero = const_int (i64_type context) 0 in
        let is_negative = build_icmp Llvm.Icmp.Slt idx_val zero "is_negative" builder in
        
        let is_too_large = build_icmp Llvm.Icmp.Sge idx_val length "is_too_large" builder in
        let is_out_of_bounds = build_or is_negative is_too_large "is_out_of_bounds" builder in
        
        (* Create blocks for bounds checking *)
        let current_block = insertion_block builder in
        let parent = block_parent current_block in
        let out_of_bounds_block = append_block context "assign_out_of_bounds" parent in
        let in_bounds_block = append_block context "assign_in_bounds" parent in
        
        (* Branch based on bounds check *)
        ignore (build_cond_br is_out_of_bounds out_of_bounds_block in_bounds_block builder);
        
        (* Set up out of bounds block with error message *)
        position_at_end out_of_bounds_block builder;
        let err_msg = build_global_stringptr "[Codegen] Array index out of bounds in assignment: index %lld, array length %lld\n" "assign_bounds_err_msg" builder in
        
        (* Print error message and exit with error code *)
        ignore (build_call printf_ty printf_func [| err_msg; idx_val; length |] "print_assign_error" builder);
        let exit_func = declare_function "exit" (function_type (void_type context) [| i32_type context |]) the_module in
        ignore (build_call (function_type (void_type context) [| i32_type context |]) exit_func [| const_int (i32_type context) 1 |] "" builder);
        ignore (build_unreachable builder);
        
        (* Continue with actual assignment in the in-bounds case *)
        position_at_end in_bounds_block builder;
        
        (* Store element at the correct index *)
        let elem_ptr = build_gep elem_ty data_ptr_cast [| idx_val |] "elem_assign_ptr" builder in
        ignore (build_store value_val elem_ptr builder);
        None

  | HArrayPut (arr, value) ->
      (* Get the array pointer and value to put *)
      let arr_val = codegen_expr tables arr in
      let value_val = codegen_expr tables value in
      let value_ty = type_of value_val in
      
      (* Load the array pointer *)
      let array_ptr = 
        match arr with
        | HVar _ -> build_load (pointer_type context) arr_val "array_ptr_load" builder
        | _ -> arr_val
      in
      
      (* Get current array length *)
      let length_ptr = build_bitcast array_ptr (pointer_type context) "length_ptr" builder in
      let current_length = build_load (i64_type context) length_ptr "current_length" builder in
      
      (* Calculate new length = current_length + 1 *)
      let new_length = build_add current_length (const_int (i64_type context) 1) "new_length" builder in
      
      (* Calculate size for new array (8 bytes for length + element_size * new_length) *)
      let elem_size = 8 (* All our types use 64 bits/8 bytes *) in
      let new_array_size = build_add 
        (const_int (i64_type context) 8) 
        (build_mul new_length (const_int (i64_type context) elem_size) "elements_size" builder)
        "array_size" builder in
      
      (* Allocate new memory *)
      let new_array_mem = build_call malloc_ty malloc_func [| new_array_size |] "new_array_malloc" builder in
      let new_array_ptr = build_pointercast new_array_mem (pointer_type context) "new_array_ptr" builder in
      
      (* Store new length *)
      let new_length_ptr = build_bitcast new_array_ptr (pointer_type context) "new_length_ptr" builder in
      ignore (build_store new_length new_length_ptr builder);
      
      (* Calculate data pointers for old and new array *)
      let old_data_ptr = build_gep (i64_type context) length_ptr [| const_int (i64_type context) 1 |] "old_data_ptr" builder in
      let new_data_ptr = build_gep (i64_type context) new_length_ptr [| const_int (i64_type context) 1 |] "new_data_ptr" builder in
      
      (* Cast to appropriate pointer types *)
      let old_data_ptr_cast = build_bitcast old_data_ptr (pointer_type context) "old_data_ptr_cast" builder in
      let new_data_ptr_cast = build_bitcast new_data_ptr (pointer_type context) "new_data_ptr_cast" builder in
      
      (* Copy existing elements - memcpy the old array data to the new array *)
      let memcpy_ty = function_type (void_type context) [| 
        pointer_type context; (* dst *)
        pointer_type context; (* src *)
        i64_type context;    (* size *)
        i1_type context      (* isvolatile - can be false *)
      |] in
      let memcpy_func = declare_function "llvm.memcpy.p0.p0.i64" memcpy_ty the_module in
      let bytes_to_copy = build_mul current_length (const_int (i64_type context) elem_size) "bytes_to_copy" builder in
      
      ignore(build_call memcpy_ty memcpy_func [|
        new_data_ptr_cast;
        old_data_ptr_cast;
        bytes_to_copy;
        const_int (i1_type context) 0   (* not volatile *)
      |] "" builder);
      
      (* Store the new value at the end of the array *)
      let last_elem_ptr = build_gep value_ty new_data_ptr_cast [| current_length |] "last_elem_ptr" builder in
      ignore (build_store value_val last_elem_ptr builder);
      
      (* Store the new array pointer back to the array variable *)
      ignore (build_store new_array_ptr arr_val builder);
      None

  | HArrayPop (arr) ->
      (* Get the array pointer *)
      let arr_val = codegen_expr tables arr in
      
      (* Load the array pointer *)
      let array_ptr = 
        match arr with
        | HVar _ -> build_load (pointer_type context) arr_val "array_ptr_load" builder
        | _ -> arr_val
      in
      
      (* Get current array length *)
      let length_ptr = build_bitcast array_ptr (pointer_type context) "length_ptr" builder in
      let current_length = build_load (i64_type context) length_ptr "current_length" builder in
      
      (* Create blocks for bounds checking - can't pop from empty array *)
      let current_block = insertion_block builder in
      let parent = block_parent current_block in
      let empty_array_block = append_block context "empty_array" parent in
      let valid_pop_block = append_block context "valid_pop" parent in
      let after_pop_block = append_block context "after_pop" parent in
      
      (* Check if array is empty *)
      let is_empty = build_icmp Llvm.Icmp.Eq current_length (const_int (i64_type context) 0) "is_empty" builder in
      ignore (build_cond_br is_empty empty_array_block valid_pop_block builder);
      
      (* Handle empty array case *)
      position_at_end empty_array_block builder;
      let err_msg = build_global_stringptr "[Codegen] Cannot pop from empty array\n" "empty_arr_err_msg" builder in
      ignore (build_call printf_ty printf_func [| err_msg |] "print_empty_error" builder);
      let exit_func = declare_function "exit" (function_type (void_type context) [| i32_type context |]) the_module in
      ignore (build_call (function_type (void_type context) [| i32_type context |]) exit_func [| const_int (i32_type context) 1 |] "" builder);
      ignore (build_unreachable builder);
      
      (* Handle valid pop case *)
      position_at_end valid_pop_block builder;
      
      (* Calculate new length = current_length - 1 *)
      let new_length = build_sub current_length (const_int (i64_type context) 1) "new_length" builder in
      
      (* Calculate size for new array (8 bytes for length + element_size * new_length) *)
      let elem_size = 8 in
      let new_array_size = build_add 
        (const_int (i64_type context) 8) 
        (build_mul new_length (const_int (i64_type context) elem_size) "elements_size" builder)
        "array_size" builder in
      
      (* Allocate new memory *)
      let new_array_mem = build_call malloc_ty malloc_func [| new_array_size |] "new_array_malloc" builder in
      let new_array_ptr = build_pointercast new_array_mem (pointer_type context) "new_array_ptr" builder in
      
      (* Store new length *)
      let new_length_ptr = build_bitcast new_array_ptr (pointer_type context) "new_length_ptr" builder in
      ignore (build_store new_length new_length_ptr builder);
      
      (* Calculate data pointers for old and new array *)
      let old_data_ptr = build_gep (i64_type context) length_ptr [| const_int (i64_type context) 1 |] "old_data_ptr" builder in
      let new_data_ptr = build_gep (i64_type context) new_length_ptr [| const_int (i64_type context) 1 |] "new_data_ptr" builder in
      
      (* Cast to appropriate pointer types *)
      let old_data_ptr_cast = build_bitcast old_data_ptr (pointer_type context) "old_data_ptr_cast" builder in
      let new_data_ptr_cast = build_bitcast new_data_ptr (pointer_type context) "new_data_ptr_cast" builder in
      
      (* Copy only the N-1 elements - don't include the last element *)
      let bytes_to_copy = build_mul new_length (const_int (i64_type context) elem_size) "bytes_to_copy" builder in
      
      let memcpy_ty = function_type (void_type context) [| 
        pointer_type context; (* dst *)
        pointer_type context; (* src *)
        i64_type context;    (* size *)
        i1_type context      (* isvolatile - can be false *)
      |] in
      let memcpy_func = declare_function "llvm.memcpy.p0.p0.i64" memcpy_ty the_module in
      
      ignore (build_call memcpy_ty memcpy_func [|
        new_data_ptr_cast;
        old_data_ptr_cast;
        bytes_to_copy;
        const_int (i1_type context) 0   (* not volatile *)
      |] "" builder);
      
      (* Store the new array pointer back to the array variable *)
      ignore (build_store new_array_ptr arr_val builder);
      ignore (build_br after_pop_block builder);
      
      (* Continue execution after pop *)
      position_at_end after_pop_block builder;
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