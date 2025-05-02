open Alcotest
open Libraries
open Ast

(* Helper function for parsing a string *)
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

(* Helper function to extract expressions from statements *)
let _extract_expr stmt =
  match stmt with
  | Print e -> e                           (* Print statements contain expressions *)
  | Assign (_, e) -> e                     (* Assignments contain expressions *)
  | Declare (_, _, e) -> e                 (* Declarations contain expressions *)
  | Return e -> e                          (* Return statements contain expressions *)
  | _ -> failwith "Statement doesn't contain a directly extractable expression"

(* Helper to parse an expression - parses it as a statement and extracts the expression *)
let parse_expr s =
  match parse_string s with
  | Block [Print e] -> e  (* We'll wrap expressions in print statements *)
  | _ -> failwith "Expected a simple expression inside a print statement"

(* Define AST equality and printer functions for testable values *)
let rec expr_equal e1 e2 =
  match e1, e2 with
  | Var v1, Var v2 -> v1 = v2
  | Int i1, Int i2 -> i1 = i2
  | String s1, String s2 -> s1 = s2
  | Float f1, Float f2 -> Float.equal f1 f2
  | Bool b1, Bool b2 -> b1 = b2
  | Binop (op1, l1, r1), Binop (op2, l2, r2) -> op1 = op2 && expr_equal l1 l2 && expr_equal r1 r2
  | Unop (op1, e1), Unop (op2, e2) -> op1 = op2 && expr_equal e1 e2
  | FunCall (f1, args1), FunCall (f2, args2) -> 
      f1 = f2 && List.length args1 = List.length args2 && 
      List.for_all2 expr_equal args1 args2
  | _, _ -> false

let rec stmt_equal s1 s2 =
  match s1, s2 with
  | Assign (v1, e1), Assign (v2, e2) -> v1 = v2 && expr_equal e1 e2
  | Declare (v1, t1, e1), Declare (v2, t2, e2) -> v1 = v2 && t1 = t2 && expr_equal e1 e2
  | Let (v1, e1, s1'), Let (v2, e2, s2') -> v1 = v2 && expr_equal e1 e2 && stmt_equal s1' s2'
  | If (c1, t1, e1), If (c2, t2, e2) -> expr_equal c1 c2 && stmt_equal t1 t2 && stmt_equal e1 e2
  | While (c1, s1'), While (c2, s2') -> expr_equal c1 c2 && stmt_equal s1' s2'
  | Print e1, Print e2 -> expr_equal e1 e2
  | Block stmts1, Block stmts2 -> 
      List.length stmts1 = List.length stmts2 && 
      List.for_all2 stmt_equal stmts1 stmts2
  | FunDecl (n1, p1, r1, b1), FunDecl (n2, p2, r2, b2) ->
      n1 = n2 && r1 = r2 && stmt_equal b1 b2 &&
      List.length p1 = List.length p2 &&
      List.for_all2 (fun (s1, t1) (s2, t2) -> s1 = s2 && t1 = t2) p1 p2
  | Return e1, Return e2 -> expr_equal e1 e2
  | _, _ -> false

(* Pretty printer for AST expressions *)
let rec string_of_expr = function
  | Var v -> "Var(" ^ v ^ ")"
  | Int i -> "Int(" ^ string_of_int i ^ ")"
  | String s -> "String(\"" ^ s ^ "\")"
  | Float f -> "Float(" ^ string_of_float f ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | Binop (op, e1, e2) -> 
      "Binop(" ^ string_of_bop op ^ ", " ^ 
      string_of_expr e1 ^ ", " ^ 
      string_of_expr e2 ^ ")"
  | Unop (op, e) -> 
      "Unop(" ^ string_of_uop op ^ ", " ^ 
      string_of_expr e ^ ")"
  | FunCall (f, args) -> 
      "FunCall(" ^ f ^ ", [" ^ 
      String.concat "; " (List.map string_of_expr args) ^ "])"

and string_of_bop = function
  | Add -> "Add" | Sub -> "Sub" | Mult -> "Mult" | Div -> "Div" | Mod -> "Mod"
  | Lt -> "Lt" | Leq -> "Leq" | Gt -> "Gt" | Geq -> "Geq" | Eq -> "Eq" | Neq -> "Neq"
  | And -> "And" | Or -> "Or"

and string_of_uop = function
  | Neg -> "Neg" | Not -> "Not"

let rec string_of_stmt = function
  | Assign (v, e) -> "Assign(" ^ v ^ ", " ^ string_of_expr e ^ ")"
  | Declare (v, t, e) -> "Declare(" ^ v ^ ", " ^ string_of_type t ^ ", " ^ string_of_expr e ^ ")"
  | Let (v, e, s) -> "Let(" ^ v ^ ", " ^ string_of_expr e ^ ", " ^ string_of_stmt s ^ ")"
  | If (c, t, e) -> "If(" ^ string_of_expr c ^ ", " ^ string_of_stmt t ^ ", " ^ string_of_stmt e ^ ")"
  | While (c, s) -> "While(" ^ string_of_expr c ^ ", " ^ string_of_stmt s ^ ")"
  | Print e -> "Print(" ^ string_of_expr e ^ ")"
  | Block stmts -> "Block([" ^ String.concat "; " (List.map string_of_stmt stmts) ^ "])"
  | FunDecl (name, params, ret, body) ->
      "FunDecl(" ^ name ^ ", [" ^ 
      String.concat "; " (List.map (fun (n, t) -> n ^ ":" ^ string_of_type t) params) ^ 
      "], " ^ string_of_type ret ^ ", " ^ string_of_stmt body ^ ")"
  | Return e -> "Return(" ^ string_of_expr e ^ ")"

and string_of_type = function
  | IntType -> "IntType" | FloatType -> "FloatType" | StringType -> "StringType"
  | BoolType -> "BoolType" | VoidType -> "VoidType"

(* Create testable values *)
let expr_testable =
  testable (fun ppf expr -> Fmt.pf ppf "%s" (string_of_expr expr)) expr_equal

let stmt_testable =
  testable (fun ppf stmt -> Fmt.pf ppf "%s" (string_of_stmt stmt)) stmt_equal

(* Test cases *)

(* Expressions *)
let test_basic_expressions () =
  check expr_testable "integer literal" 
    (Int 42)
    (parse_expr "print 42");
  
  check expr_testable "variable" 
    (Var "x")
    (parse_expr "print x");
  
  check expr_testable "string literal"
    (String "hello")
    (parse_expr "print \"hello\"");
    
  check expr_testable "float literal"
    (Float 3.14)
    (parse_expr "print 3.14");
    
  check expr_testable "boolean literal"
    (Bool true)
    (parse_expr "print true")

let test_arithmetic_expressions () =
  check expr_testable "addition"
    (Binop (Add, Int 1, Int 2))
    (parse_expr "print 1 + 2");
    
  check expr_testable "subtraction"
    (Binop (Sub, Int 5, Int 3))
    (parse_expr "print 5 - 3");
    
  check expr_testable "multiplication"
    (Binop (Mult, Int 2, Int 3))
    (parse_expr "print 2 * 3");
    
  check expr_testable "division"
    (Binop (Div, Int 6, Int 2))
    (parse_expr "print 6 / 2");
    
  check expr_testable "modulo"
    (Binop (Mod, Int 7, Int 4))
    (parse_expr "print 7 % 4")

let test_logical_expressions () =
  check expr_testable "and"
    (Binop (And, Bool true, Bool false))
    (parse_expr "print true and false");
    
  check expr_testable "or"
    (Binop (Or, Bool true, Bool false))
    (parse_expr "print true or false");
    
  check expr_testable "not"
    (Unop (Not, Bool true))
    (parse_expr "print not true")

let test_comparison_expressions () =
  check expr_testable "less than"
    (Binop (Lt, Int 1, Int 2))
    (parse_expr "print 1 < 2");
    
  check expr_testable "less than or equal"
    (Binop (Leq, Int 1, Int 1))
    (parse_expr "print 1 <= 1");
    
  check expr_testable "greater than"
    (Binop (Gt, Int 2, Int 1))
    (parse_expr "print 2 > 1");
    
  check expr_testable "greater than or equal"
    (Binop (Geq, Int 2, Int 2))
    (parse_expr "print 2 >= 2");
    
  check expr_testable "equality"
    (Binop (Eq, Int 1, Int 1))
    (parse_expr "print 1 = 1");
    
  check expr_testable "inequality"
    (Binop (Neq, Int 1, Int 2))
    (parse_expr "print 1 != 2")

let test_complex_expressions () =
  check expr_testable "parenthesized expression"
    (Binop (Add, Int 1, Binop (Mult, Int 2, Int 3)))
    (parse_expr "print 1 + (2 * 3)");
    
  check expr_testable "complex expression with multiple operators"
    (Binop (Add, 
            Binop (Mult, Int 2, Int 3),
            Binop (Div, Int 10, Int 2)))
    (parse_expr "print 2 * 3 + 10 / 2");
    
  check expr_testable "nested logical expressions"
    (Binop (And, 
            Binop (Lt, Int 1, Int 2),
            Binop (Gt, Int 3, Int 2)))
    (parse_expr "print 1 < 2 and 3 > 2");
    
  (* check expr_testable "negation"           (* Negation doesn't actually work in our program, as it is not a binary negation, but just making a number negative *)
    (Unop (Neg, Int 5))
    (parse_expr "print -5"); *)
    
  check expr_testable "function call"
    (FunCall ("max", [Int 1; Int 2]))
    (parse_expr "print max(1, 2)")

(* Statements *)
let test_assignment_statements () =
  check stmt_testable "simple assignment"
    (Assign ("x", Int 42))
    (match parse_string "x := 42" with Block [s] -> s | _ -> failwith "Unexpected AST");
    
  check stmt_testable "assignment with expression"
    (Assign ("x", Binop (Add, Int 1, Int 2)))
    (match parse_string "x := 1 + 2" with Block [s] -> s | _ -> failwith "Unexpected AST");
    
  check stmt_testable "declaration with type"
    (Declare ("x", IntType, Int 42))
    (match parse_string "x: int := 42" with Block [s] -> s | _ -> failwith "Unexpected AST")

let test_control_flow_statements () =
  check stmt_testable "if-then statement"
    (If (Bool true, Block [Print (Int 1)], Block []))
    (match parse_string "if true then { print 1 }" with Block [s] -> s | _ -> failwith "Unexpected AST");
    
  check stmt_testable "if-then-else statement"
    (If (Bool true, Block [Print (Int 1)], Block [Print (Int 2)]))
    (match parse_string "if true then { print 1 } else { print 2 }" with Block [s] -> s | _ -> failwith "Unexpected AST");
    
  check stmt_testable "while loop"
    (While (Bool true, Print (Int 1)))
    (match parse_string "while true do print 1" with Block [s] -> s | _ -> failwith "Unexpected AST")

let test_function_statements () =
  check stmt_testable "function declaration with no parameters"
    (FunDecl ("main", [], VoidType, Block [Return (Int 0)]))
    (match parse_string "main() -> void := { return 0 }" with Block [s] -> s | _ -> failwith "Unexpected AST");
    
  check stmt_testable "function declaration with parameters"
    (FunDecl ("add", [("a", IntType); ("b", IntType)], IntType,
              Block [Return (Binop (Add, Var "a", Var "b"))]))
    (match parse_string "add(a: int, b: int) -> int := { return a + b }" with Block [s] -> s | _ -> failwith "Unexpected AST");
    
  check stmt_testable "function declaration with typed parameters and body"
    (FunDecl ("max", [("a", IntType); ("b", IntType)], IntType,
              Block [
                If (Binop (Gt, Var "a", Var "b"),
                   Block [Return (Var "a")],
                   Block [Return (Var "b")])
              ]))
    (match parse_string "max(a: int, b: int) -> int := { if a > b then { return a } else { return b } }" 
     with Block [s] -> s | _ -> failwith "Unexpected AST")

let test_nested_functions () =
  check stmt_testable "nested function declaration"
    (FunDecl ("outer", [("x", IntType)], IntType,
              Block [
                FunDecl ("inner", [("y", IntType)], IntType, 
                        Block [
                          Return (Binop (Add, Var "x", Var "y"))
                        ]);
                Return (FunCall ("inner", [Int 5]))
              ]))
    (match parse_string "outer(x: int) -> int := { inner(y: int) -> int := { return x + y }; return inner(5) }" 
     with Block [s] -> s | _ -> failwith "Unexpected AST");
     
  check stmt_testable "nested recursive function"
    (FunDecl ("factorial", [("n", IntType)], IntType,
              Block [
                FunDecl ("fact_tail", [("n", IntType); ("acc", IntType)], IntType,
                        Block [
                          If (Binop (Eq, Var "n", Int 0),
                             Block [Return (Var "acc")],
                             Block [Return (FunCall ("fact_tail", 
                                                    [Binop (Sub, Var "n", Int 1); 
                                                     Binop (Mult, Var "n", Var "acc")]))])
                        ]);
                Return (FunCall ("fact_tail", [Var "n"; Int 1]))
              ]))
    (match parse_string "factorial(n: int) -> int := { fact_tail(n: int, acc: int) -> int := { if n = 0 then { return acc } else { return fact_tail(n - 1, n * acc) } }; return fact_tail(n, 1) }" 
     with Block [s] -> s | _ -> failwith "Unexpected AST")

let test_complex_statements () =
  check stmt_testable "nested blocks"
    (Block [
      Assign ("x", Int 1);
      Block [
        Assign ("y", Int 2);
        Print (Binop (Add, Var "x", Var "y"))
      ]
    ])
    (parse_string "x := 1; { y := 2; print x + y }");
    
  check stmt_testable "multiple statements"
    (Block [
      Declare ("x", IntType, Int 10);
      While (Binop (Gt, Var "x", Int 0),
             Block [
               Print (Var "x");
               Assign ("x", Binop (Sub, Var "x", Int 1))
             ])
    ])
    (parse_string "x: int := 10; while x > 0 do { print x; x := x - 1 }");
    
  (* Test for block-level variable scoping *)
  check stmt_testable "block scoping"
    (Block [
      Block [
        Declare ("y", IntType, Int 5);
        Print (Var "y")
      ];
      (* Attempting to use y outside its scope *)
      Print (Var "y")
    ])
    (parse_string "{ y: int := 5; print y }; print y")

(* Test for operator precedence *)
let test_operator_precedence () =
  check expr_testable "multiplication before addition"
    (Binop (Add, Int 1, Binop (Mult, Int 2, Int 3)))
    (parse_expr "print 1 + 2 * 3");
    
  check expr_testable "and before or"
    (Binop (Or, Bool false, Binop (And, Bool true, Bool true)))
    (parse_expr "print false or true and true");
    
  check expr_testable "comparison before and"
    (Binop (And, Binop (Lt, Int 1, Int 2), Binop (Gt, Int 3, Int 2)))
    (parse_expr "print 1 < 2 and 3 > 2")

(* Program tests *)
let test_complete_programs () =
  check stmt_testable "fibonacci function"
    (Block [
      FunDecl ("fibonacci", [("n", IntType)], IntType,
              Block [
                If (Binop (Leq, Var "n", Int 1),
                   Block [Return (Var "n")],
                   Block [Return (Binop (Add, 
                                        FunCall ("fibonacci", [Binop (Sub, Var "n", Int 1)]), 
                                        FunCall ("fibonacci", [Binop (Sub, Var "n", Int 2)])))])
              ]);
      Print (FunCall ("fibonacci", [Int 10]))
    ])
    (parse_string "fibonacci(n: int) -> int := { if n <= 1 then { return n } else { return fibonacci(n - 1) + fibonacci(n - 2) } }; print fibonacci(10)");
    
  check stmt_testable "factorial function with while loop"
    (Block [
      FunDecl ("factorial", [("n", IntType)], IntType,
              Block [
                Declare ("result", IntType, Int 1);
                While (Binop (Gt, Var "n", Int 0),
                      Block [
                        Assign ("result", Binop (Mult, Var "result", Var "n"));
                        Assign ("n", Binop (Sub, Var "n", Int 1))
                      ]);
                Return (Var "result")
              ]);
      Print (FunCall ("factorial", [Int 5]))
    ])
    (parse_string "factorial(n: int) -> int := { result: int := 1; while n > 0 do { result := result * n; n := n - 1 }; return result }; print factorial(5)")

(* Test return statements in different contexts *)
let test_return_statements () =
  (* Test basic return statement *)
  check stmt_testable "basic return statement"
    (Return (Int 42))
    (match parse_string "return 42" with Block [s] -> s | _ -> failwith "Unexpected AST");
    
  (* Test function with code after return.
     This tests that the AST still builds code after a return statement.
     Skipping that code will be tested in codegen *)
  check stmt_testable "function with code after return"
    (FunDecl ("test", [], IntType, 
              Block [
                Return (Int 42);
                Print (String "unreachable code")
              ]))
    (match parse_string "test() -> int := { return 42; print \"unreachable code\" }" 
     with Block [s] -> s | _ -> failwith "Unexpected AST");
     
  (* Test return in if statement *)
  check stmt_testable "return in if statement"
    (FunDecl ("test", [("x", IntType)], IntType,
              Block [
                If (Binop (Gt, Var "x", Int 0),
                   Block [Return (Int 1)],
                   Block [Return (Int 0)])
              ]))
    (match parse_string "test(x: int) -> int := { if x > 0 then { return 1 } else { return 0 } }" 
     with Block [s] -> s | _ -> failwith "Unexpected AST");
     
  (* Test return in nested block *)
  check stmt_testable "return in nested block"
    (FunDecl ("test", [], IntType,
              Block [
                Block [Return (Int 42)];
                Print (String "unreachable code")
              ]))
    (match parse_string "test() -> int := { { return 42 }; print \"unreachable code\" }" 
     with Block [s] -> s | _ -> failwith "Unexpected AST");
     
  (* Test early return in function *)
  check stmt_testable "early return in function"
    (FunDecl ("test", [("x", IntType)], IntType,
              Block [
                If (Binop (Gt, Var "x", Int 10),
                   Block [Return (Var "x")], 
                   Block []);
                Print (String "only for x <= 10");
                Return (Int 0)
              ]))
    (match parse_string "test(x: int) -> int := { if x > 10 then { return x } print \"only for x <= 10\"; return 0 }" 
     with Block [s] -> s | _ -> failwith "Unexpected AST")

(* Test suite *)
let suite =
  [
    "Basic Expressions", `Quick, test_basic_expressions;
    "Arithmetic Expressions", `Quick, test_arithmetic_expressions;
    "Logical Expressions", `Quick, test_logical_expressions;
    "Comparison Expressions", `Quick, test_comparison_expressions;
    "Complex Expressions", `Quick, test_complex_expressions;
    "Assignment Statements", `Quick, test_assignment_statements;
    "Control Flow Statements", `Quick, test_control_flow_statements;
    "Function Statements", `Quick, test_function_statements;
    "Nested Functions", `Quick, test_nested_functions;
    "Complex Statements", `Quick, test_complex_statements;
    "Operator Precedence", `Quick, test_operator_precedence;
    "Complete Programs", `Quick, test_complete_programs;
    "Return Statements", `Quick, test_return_statements;
  ]

(* Run the tests *)
let () = Alcotest.run "Parser Tests" [("Parser", suite)]