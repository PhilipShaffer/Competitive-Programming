open Alcotest
open Libraries
open Semant

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

(* Helper to analyze a string and expect success *)
let analyze_success s =
  let ast = parse_string s in
  try
    let _hir = analyze_stmt [] ast in
    true
  with
  | Semantic_error _ -> false

(* Helper to analyze a string and expect a specific error *)
let analyze_error s expected_error =
  let ast = parse_string s in
  try
    let _hir = analyze_stmt [] ast in
    false (* No error occurred when one was expected *)
  with
  | Semantic_error msg -> 
      if expected_error = "" then true  (* Any error is ok if no specific message is expected *)
      else 
        let lc_msg = String.lowercase_ascii msg in
        let lc_expected = String.lowercase_ascii expected_error in
        let rec check_contains s1 s2 =
          if String.length s2 > String.length s1 then false
          else if String.sub s1 0 (String.length s2) = s2 then true
          else if String.length s1 > 0 then 
            check_contains (String.sub s1 1 (String.length s1 - 1)) s2
          else false
        in
        check_contains lc_msg lc_expected

(* Test cases *)

(* Variable Declaration and Scoping Tests *)
let test_variable_declaration () =
  (* Basic variable declaration *)
  check bool "simple declaration"
    true
    (analyze_success "x: int := 42");
  
  (* Double declaration in the same scope (should fail) *)
  check bool "double declaration"
    true
    (analyze_error "x: int := 42; x: int := 43" "already defined");
    
  (* Type mismatch in declaration *)
  check bool "type mismatch in declaration" 
    true
    (analyze_error "x: int := \"hello\"" "Type mismatch")

let test_variable_scoping () =
  (* Variables in nested scopes *)
  check bool "nested scope access"
    true
    (analyze_success "x: int := 10; { print x }");
    
  (* Shadow variable in inner scope *)
  check bool "variable shadowing"
    true
    (analyze_success "x: int := 10; { x: int := 20; print x }");
    
  (* Block-level scoping: cannot access variable outside its scope *)
  check bool "variable not accessible outside block" 
    true
    (analyze_error "{ y: int := 5; print y }; print y" "Undeclared variable");
  
  (* Test if variables declared in if/else body are properly scoped *)
  check bool "variables in if statement" 
    true
    (analyze_error 
      "if true then { x: int := 10; print x } else { }; print x" 
      "Undeclared variable");
      
  (* Test variables in while body scope *)
  check bool "variables in while body"
    true
    (analyze_error 
      "while true do { x: int := 10; print x }; print x" 
      "Undeclared variable")

(* Type Checking Tests *)
let test_type_checking () =
  (* Assignment type matching *)
  check bool "assignment type match"
    true
    (analyze_success "x: int := 10; x := 20");
    
  (* Assignment type mismatch *)
  check bool "assignment type mismatch"
    true
    (analyze_error "x: int := 10; x := \"hello\"" "Type mismatch");
    
  (* Binary operation type checking *)
  check bool "arithmetic operation type"
    true
    (analyze_success "x: int := 10 + 20 * 30");
    
  (* Binary operation type mismatch *)
  check bool "arithmetic operation type mismatch"
    true
    (analyze_error "x: int := 10 + true" "same type");
    
  (* Condition type must be boolean *)
  check bool "if condition type"
    true
    (analyze_error "if 42 then { print 1 } else { print 2 }" "must be bool")
  
(* Function Tests *)
let test_functions () =
  (* Basic function declaration and call *)
  check bool "function declaration and call"
    true
    (analyze_success 
      "add(a: int, b: int) -> int := { return a + b }; print add(1, 2)");
      
  (* Function call with wrong number of arguments *)
  check bool "function call wrong arity"
    true
    (analyze_error 
      "add(a: int, b: int) -> int := { return a + b }; print add(1)" 
      "Arity mismatch");
      
  (* Function call with wrong argument types *)
  check bool "function call wrong arg types"
    true
    (analyze_error 
      "add(a: int, b: int) -> int := { return a + b }; print add(1, true)" 
      "type mismatch");
      
  (* Function recursion *)
  check bool "recursive function"
    true
    (analyze_success 
      "factorial(n: int) -> int := { if n = 0 then { return 1 } else { return n * factorial(n-1) } }");
      
  (* Nested function with access to outer scope *)
  check bool "nested function scope access"
    true
    (analyze_success 
      "outer(x: int) -> int := { inner(y: int) -> int := { return x + y }; return inner(5) }")

(* Test suite *)
let suite =
  [
    "Variable Declaration", `Quick, test_variable_declaration;
    "Variable Scoping", `Quick, test_variable_scoping;
    "Type Checking", `Quick, test_type_checking;
    "Functions", `Quick, test_functions;
  ]

(* Run the tests *)
let () = Alcotest.run "Semantic Analyzer Tests" [("Semant", suite)]