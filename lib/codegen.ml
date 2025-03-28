open Llvm
open Base
open Ast
open Stdio

let context = global_context ()
let the_module = create_module context "main"
let builder = builder context
let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create (module String)
let int_type = i32_type context

(* Create an alloca instruction in the entry block of the function. This
 * is used for mutable variables etc. *)
 let create_entry_block_alloca the_function var_name =
  let builder =
    builder_at context (instr_begin (entry_block the_function))
  in
  build_alloca int_type var_name builder
;;

let rec codegen_expr = function
  | Var name ->
    (* Look up the name in the symbol table *)
    (match Hashtbl.find named_values name with
     | Some value -> build_load int_type value name builder  (* Load the value from the alloca *)
     | None -> raise (Failure ("unknown variable name: " ^ name)))
  
  | Int n -> const_int int_type n
  
  | Bool b -> const_int int_type (if b then 1 else 0)
  
  | Binop (op, lhs, rhs) ->
    let lhs_val = codegen_expr lhs in
    let rhs_val = codegen_expr rhs in
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
     | Neg -> build_neg expr_val "negtmp" builder
     | Not ->
        (* For logical not, we check if the expression is equal to 0 (false) *)
        let cmp = build_icmp Icmp.Eq expr_val (const_int int_type 0) "cmptmp" builder in
        build_zext cmp int_type "booltmp" builder)

and codegen_stmt = function
  | Assign (var, expr) ->
    (* Look up the name or create it if this is the first use *)
    let value = codegen_expr expr in
    (match Hashtbl.find named_values var with
     | Some alloca ->
        (* Existing variable, update its value *)
        ignore (build_store value alloca builder);
        value
     | None ->
        (* New variable, allocate space and store *)
        let the_function = block_parent (insertion_block builder) in
        let alloca = create_entry_block_alloca the_function var in
        Hashtbl.set named_values ~key:var ~data:alloca;
        ignore (build_store value alloca builder);
        value)
  
  | Let (var, expr, body) ->
    (* Evaluate the initializer *)
    let init_val = codegen_expr expr in
    
    (* Create an alloca for this variable *)
    let the_function = block_parent (insertion_block builder) in
    let alloca = create_entry_block_alloca the_function var in
    
    (* Store the initial value *)
    ignore (build_store init_val alloca builder);
    
    (* Save the old variable binding if it exists *)
    let old_binding = Hashtbl.find named_values var in
    
    (* Add the new binding to the symbol table *)
    Hashtbl.set named_values ~key:var ~data:alloca;
    
    (* Generate code for the body *)
    let body_val = codegen_stmt body in
    
    (* Restore the old binding if it existed *)
    (match old_binding with
     | Some old_value -> Hashtbl.set named_values ~key:var ~data:old_value
     | None -> Hashtbl.remove named_values var);
    
    (* Return the value of the body *)
    body_val
  
  | If (cond, then_stmt, else_stmt) ->
    (* Generate code for the condition *)
    let cond_val = codegen_expr cond in
    
    (* Convert condition to a boolean value *)
    let zero = const_int int_type 0 in
    let cond_val = build_icmp Icmp.Ne cond_val zero "ifcond" builder in
    
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
    let zero = const_int int_type 0 in
    let cond_val = build_icmp Icmp.Ne cond_val zero "whilecond" builder in
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
    
    (* Get the putchar function *)
    let putchar_type = function_type int_type [| int_type |] in
    let putchar = declare_function "putchar" putchar_type the_module in
    
    (* Convert an integer to its ASCII representation and print it 
       This is a very minimal version that only prints single digits 0-9 directly *)
    let digit_to_ascii digit = build_add digit (const_int int_type 48) "ascii" builder in
    
    (* Print a single ASCII digit *)
    ignore (build_call putchar_type putchar [| digit_to_ascii value |] "print" builder);
    
    (* Print a newline *)
    ignore (build_call putchar_type putchar [| const_int int_type 10 |] "newline" builder);
    
    (* Return the value that was printed *)
    value
  
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
  let ret_val = codegen_stmt program in
  
  (* Return from main function *)
  ignore (build_ret ret_val builder);
  
  (* Verify the module *)
  Llvm_analysis.assert_valid_module the_module;
  
  (* Output the LLVM IR to a file *)
  let output_file = "output.ll" in
  let out_str = Llvm.string_of_llmodule the_module in
  let oc = Out_channel.create output_file in
  Out_channel.output_string oc out_str;
  Out_channel.close oc;
  
  the_module
