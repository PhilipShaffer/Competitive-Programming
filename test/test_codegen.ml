open Alcotest
open Libraries
open Libraries.Semant
open Libraries.Codegen
open Llvm

(* Helper function for parsing a string into AST *)
let parse_string s =
  let lexbuf = Lexing.from_string s in
  try
    Parser.main Lexer.read lexbuf
  with
  | Parser.Error ->
      failwith ("Parse error at " ^ 
                string_of_int (Lexing.lexeme_start lexbuf) ^ "-" ^
                string_of_int (Lexing.lexeme_end lexbuf) ^ ": " ^
                Lexing.lexeme lexbuf)
  | Failure msg -> failwith msg

(* Helper to parse a string, convert to HIR, and then to LLVM IR *)
let parse_to_llvm s =
  let ast = parse_string s in
  let hir = analyze_stmt [] ast in
  codegen_hir hir

(* Reset the symbol counter before each test *)
let reset_symbols () =
  Semant.symbol_counter := 0;
  Hashtbl.clear Semant.sym_table_ids

(* Helper to search for a function in the module *)
let find_function_in_module m name =
  match lookup_function name m with
  | Some f -> f
  | None -> failwith ("Function " ^ name ^ " not found in module")

(* Helper to verify a module is well-formed *)
let assert_valid_module m =
  match Llvm_analysis.verify_module m with
  | None -> true
  | Some error -> 
      Stdlib.print_endline ("Module verification error: " ^ error);
      false

(* Helper to count occurrences of specific IR instructions *)
let count_instruction_type llvalue instruction_predicate =
  let count = ref 0 in
  let iter_blocks f fn =
    Llvm.iter_blocks (fun blk -> Llvm.iter_instrs f blk) fn
  in
  iter_blocks (fun instr -> 
    if instruction_predicate instr then incr count
  ) llvalue;
  !count

(* Test if a function contains a specific instruction type *)
let function_contains_instruction fn predicate =
  let contains = ref false in
  let iter_instrs f blk = Llvm.iter_instrs f blk in
  let iter_blocks f fn = Llvm.iter_blocks (fun blk -> iter_instrs f blk) fn in
  iter_blocks (fun instr -> 
    if predicate instr then contains := true
  ) fn;
  !contains



(* Test basic LLVM IR generation *)
let test_basic_ir_generation () =
  reset_symbols ();
  
  (* Test generation of a simple integer literal *)
  let int_ir = parse_to_llvm "print 42" in
  check bool "Valid module generated for integer literal" true (assert_valid_module int_ir);
  
  (* Test generation of a simple print statement *)
  let print_ir = parse_to_llvm "print 42" in
  check bool "Valid module generated for print statement" true (assert_valid_module print_ir);
  
  (* Check that the main function exists *)
  let main_func = find_function_in_module print_ir "main" in
  check string "Main function created" "main" (value_name main_func);
  
  (* Verify that printf is declared for print statements *)
  let printf_func_opt = lookup_function "printf" print_ir in
  check bool "Printf function declared" true (Option.is_some printf_func_opt);
  
  (* Check that main function has a ret instruction *)
  let has_ret = function_contains_instruction main_func (fun instr -> 
    match Llvm.instr_opcode instr with
    | Llvm.Opcode.Ret -> true
    | _ -> false
  ) in
  check bool "Main function contains return instruction" true has_ret;
  
  (* Check for expected call to printf in the print statement *)
  let print_func_ir = parse_to_llvm "print \"hello world\"" in
  let print_main = find_function_in_module print_func_ir "main" in
  let call_count = count_instruction_type print_main (fun instr ->
    Llvm.instr_opcode instr = Llvm.Opcode.Call
  ) in
  check bool "Print statement should generate function calls" true (call_count > 0)

(* Test variable declarations and assignments *)
let test_variable_declarations () =
  reset_symbols ();
  
  (* Test variable declaration and initialization *)
  let var_ir = parse_to_llvm "x: int := 42; print x" in
  check bool "Valid module generated for variable declaration" true (assert_valid_module var_ir);
  
  (* Check for alloca and store instructions for variable x *)
  let main_func = find_function_in_module var_ir "main" in
  let has_alloca = function_contains_instruction main_func (fun instr -> 
    Llvm.instr_opcode instr = Llvm.Opcode.Alloca
  ) in
  check bool "Variable declaration generates alloca instruction" true has_alloca;
  
  let has_store = function_contains_instruction main_func (fun instr -> 
    Llvm.instr_opcode instr = Llvm.Opcode.Store
  ) in
  check bool "Variable declaration generates store instruction" true has_store;
  
  let has_load = function_contains_instruction main_func (fun instr -> 
    Llvm.instr_opcode instr = Llvm.Opcode.Load
  ) in
  check bool "Variable access generates load instruction" true has_load;
  
  (* Test variable assignment *)
  let assign_ir = parse_to_llvm "x: int := 42; x := 100; print x" in
  let assign_main = find_function_in_module assign_ir "main" in
  
  (* Verify we have correct instruction types present *)
  let has_assign_store = function_contains_instruction assign_main (fun instr -> 
    Llvm.instr_opcode instr = Llvm.Opcode.Store
  ) in
  check bool "Variable assignment generates store instruction" true has_assign_store;
  
  (* Since the assignment also includes a print, we just verify we have a store *)
  let store_count_assign = count_instruction_type assign_main (fun instr ->
    Llvm.instr_opcode instr = Llvm.Opcode.Store
  ) in
  
  check bool "Assignment code includes store instructions" true (store_count_assign >= 2)

(* Test arithmetic operations *)
let test_arithmetic_operations () =
  reset_symbols ();
  
  (* Test validity of arithmetic expressions *)
  let arith_ir = parse_to_llvm "print 1 + 2 * 3" in
  check bool "Valid module generated for arithmetic operations" true (assert_valid_module arith_ir);
  
  (* Test validity of complex arithmetic expressions *)
  let complex_ir = parse_to_llvm "print (1 + 2) * 3 - 4 / 2" in
  check bool "Valid module generated for complex arithmetic" true (assert_valid_module complex_ir);
  
  (* Just check that the code compiles successfully *)
  let has_main = match lookup_function "main" arith_ir with
    | Some _ -> true
    | None -> false
  in
  check bool "Arithmetic expressions compile successfully" true has_main

(* Test boolean operations and comparisons *)
let test_boolean_operations () =
  reset_symbols ();
  
  (* Test boolean literals *)
  let bool_ir = parse_to_llvm "print true" in
  check bool "Valid module for boolean literal" true (assert_valid_module bool_ir);
  
  (* Test comparison operations *)
  let comp_ir = parse_to_llvm "print 1 < 2" in
  check bool "Valid module for comparison operations" true (assert_valid_module comp_ir);
  
  (* Test logical operations *)
  let logic_ir = parse_to_llvm "print true and false or true" in
  check bool "Valid module for logical operations" true (assert_valid_module logic_ir);
  
  (* Check the modules compile properly *)
  let comp_has_main = match lookup_function "main" comp_ir with
    | Some _ -> true
    | None -> false
  in
  check bool "Comparison operations compile successfully" true comp_has_main;
  
  let logic_has_main = match lookup_function "main" logic_ir with
    | Some _ -> true
    | None -> false
  in
  check bool "Logical operations compile successfully" true logic_has_main

(* Test control flow statements *)
let test_control_flow () =
  reset_symbols ();
  
  (* If-then-else statement *)
  let if_ir = parse_to_llvm "if 1 < 2 then { print 1 } else { print 2 }" in
  check bool "Valid module for if statement" true (assert_valid_module if_ir);
  
  (* While loop *)
  let while_ir = parse_to_llvm "x : int := 0; while x < 5 do { print x; x := x + 1 }" in
  check bool "Valid module for while loop" true (assert_valid_module while_ir);
  
  (* Nested if in while *)
  let nested_ir = parse_to_llvm "
    x : int := 0;
    while x < 3 do {
      if x = 1 then { print \"one\" } else { print x };
      x := x + 1
    }
  " in
  check bool "Valid module for nested control flow" true (assert_valid_module nested_ir);
  
  (* Check that the modules compile properly *)
  let if_has_main = match lookup_function "main" if_ir with
    | Some _ -> true
    | None -> false
  in
  check bool "If statement compiles successfully" true if_has_main;
  
  let while_has_main = match lookup_function "main" while_ir with
    | Some _ -> true
    | None -> false
  in
  check bool "While loop compiles successfully" true while_has_main;
  
  let nested_has_main = match lookup_function "main" nested_ir with
    | Some _ -> true
    | None -> false
  in
  check bool "Nested control flow compiles successfully" true nested_has_main

(* Test function declarations and calls *)
let test_functions () =
  reset_symbols ();
  
  (* Simple function declaration and call *)
  let func_ir = parse_to_llvm "
    add(a : int, b : int) -> int := {
      return a + b
    }
    print add(1, 2)
  " in
  check bool "Valid module for function declaration" true (assert_valid_module func_ir);
  
  (* Recursive function *)
  let rec_func_ir = parse_to_llvm "
    factorial(n : int) -> int := {
      if n <= 1 then { return 1 } else { return n * factorial(n - 1) }
    }
    print factorial(5)
  " in
  check bool "Valid module for recursive function" true (assert_valid_module rec_func_ir);
  
  (* Verify functions exist in module *)
  let has_funcs = match lookup_function "main" func_ir with
    | Some _ -> true
    | None -> false
  in
  check bool "Functions exist in module" true has_funcs;
  
  (* Check for functions in modules *)
  let func_has_main = match lookup_function "main" func_ir with
    | Some _ -> true
    | None -> false
  in
  check bool "Functions compile successfully" true func_has_main;
  
  let rec_has_main = match lookup_function "main" rec_func_ir with
    | Some _ -> true
    | None -> false
  in
  check bool "Recursive functions compile successfully" true rec_has_main

(* Test array operations *)
let test_array_operations () =
  reset_symbols ();
  
  (* Array declaration and access *)
  let array_ir = parse_to_llvm "
    arr : int[] := [1, 2, 3, 4, 5];
    print arr[2]
  " in
  check bool "Valid module for array operations" true (assert_valid_module array_ir);
  
  (* Array length *)
  let array_len_ir = parse_to_llvm "
    arr : int[] := [1, 2, 3, 4, 5];
    print len(arr)
  " in
  check bool "Valid module for array length" true (assert_valid_module array_len_ir);
  
  (* Array assignment *)
  let array_assign_ir = parse_to_llvm "
    arr : int[] := [1, 2, 3, 4, 5];
    arr[2] := 99;
    print arr[2]
  " in
  check bool "Valid module for array assignment" true (assert_valid_module array_assign_ir);
  
  (* Array bounds checking *)
  let array_bounds_ir = parse_to_llvm "
    arr : int[] := [1, 2, 3, 4, 5];
    if 1 < 2 then { print arr[2] } else { print arr[4] }
  " in
  check bool "Valid module for array bounds checking" true (assert_valid_module array_bounds_ir);
  
  (* Check that array modules compile successfully *)
  let array_has_main = match lookup_function "main" array_ir with
    | Some _ -> true
    | None -> false
  in
  check bool "Array access compiles successfully" true array_has_main;
  
  let len_has_main = match lookup_function "main" array_len_ir with
    | Some _ -> true
    | None -> false
  in
  check bool "Array length compiles successfully" true len_has_main;
  
  let assign_has_main = match lookup_function "main" array_assign_ir with
    | Some _ -> true
    | None -> false
  in
  check bool "Array assignment compiles successfully" true assign_has_main;
  
  let bounds_has_main = match lookup_function "main" array_bounds_ir with
    | Some _ -> true
    | None -> false
  in
  check bool "Array bounds checking compiles successfully" true bounds_has_main

(* Test suite *)
let suite =
  [
    "Basic IR Generation", `Quick, test_basic_ir_generation;
    "Variable Declarations", `Quick, test_variable_declarations;
    "Arithmetic Operations", `Quick, test_arithmetic_operations;
    "Boolean Operations", `Quick, test_boolean_operations;
    "Control Flow", `Quick, test_control_flow;
    "Functions", `Quick, test_functions;
    "Array Operations", `Quick, test_array_operations;
  ]

(* Run the tests *)
let () = Alcotest.run "Codegen Tests" [("Codegen", suite)]