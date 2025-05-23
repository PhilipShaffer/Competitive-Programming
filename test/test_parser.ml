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
  | ArrayLit el1, ArrayLit el2 ->
      List.length el1 = List.length el2 &&
      List.for_all2 expr_equal el1 el2
  | ArrayGet (arr1, idx1), ArrayGet (arr2, idx2) ->
      expr_equal arr1 arr2 && expr_equal idx1 idx2
  | ArrayLen arr1, ArrayLen arr2 -> expr_equal arr1 arr2
  | CastInt e1, CastInt e2 -> expr_equal e1 e2
  | CastFloat e1, CastFloat e2 -> expr_equal e1 e2
  | CastString e1, CastString e2 -> expr_equal e1 e2
  | _, _ -> false

let rec stmt_equal s1 s2 =
  match s1, s2 with
  | Assign (v1, e1), Assign (v2, e2) -> v1 = v2 && expr_equal e1 e2
  | Declare (v1, t1, e1), Declare (v2, t2, e2) -> v1 = v2 && t1 = t2 && expr_equal e1 e2
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
  | ArrayPut (arr1, val1), ArrayPut (arr2, val2) -> expr_equal arr1 arr2 && expr_equal val1 val2
  | ArrayPop arr1, ArrayPop arr2 -> expr_equal arr1 arr2
  | ArrayAssign (arr1, idx1, val1), ArrayAssign (arr2, idx2, val2) -> expr_equal arr1 arr2 && expr_equal idx1 idx2 && expr_equal val1 val2
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
  | ArrayLit elements ->
      "ArrayLit([" ^ String.concat "; " (List.map string_of_expr elements) ^ "])"
  | ArrayGet (arr, idx) ->
      "ArrayGet(" ^ string_of_expr arr ^ ", " ^ string_of_expr idx ^ ")"
  | ArrayLen arr ->
      "ArrayLen(" ^ string_of_expr arr ^ ")"
  | CastInt e -> "CastInt(" ^ string_of_expr e ^ ")"
  | CastFloat e -> "CastFloat(" ^ string_of_expr e ^ ")"
  | CastString e -> "CastString(" ^ string_of_expr e ^ ")"

and string_of_bop = function
  | Add -> "Add" | Sub -> "Sub" | Mult -> "Mult" | Div -> "Div" | Mod -> "Mod"
  | Lt -> "Lt" | Leq -> "Leq" | Gt -> "Gt" | Geq -> "Geq" | Eq -> "Eq" | Neq -> "Neq"
  | And -> "And" | Or -> "Or"

and string_of_uop = function
  | Neg -> "Neg" | Not -> "Not"

let rec string_of_stmt = function
  | Assign (v, e) -> "Assign(" ^ v ^ ", " ^ string_of_expr e ^ ")"
  | Declare (v, t, e) -> "Declare(" ^ v ^ ", " ^ string_of_type t ^ ", " ^ string_of_expr e ^ ")"
  | If (c, t, e) -> "If(" ^ string_of_expr c ^ ", " ^ string_of_stmt t ^ ", " ^ string_of_stmt e ^ ")"
  | While (c, s) -> "While(" ^ string_of_expr c ^ ", " ^ string_of_stmt s ^ ")"
  | Print e -> "Print(" ^ string_of_expr e ^ ")"
  | Block stmts -> "Block([" ^ String.concat "; " (List.map string_of_stmt stmts) ^ "])"
  | FunDecl (name, params, ret, body) ->
      "FunDecl(" ^ name ^ ", [" ^ 
      String.concat "; " (List.map (fun (n, t) -> n ^ ":" ^ string_of_type t) params) ^ 
      "], " ^ string_of_type ret ^ ", " ^ string_of_stmt body ^ ")"
  | Return e -> "Return(" ^ string_of_expr e ^ ")"
  | ArrayAssign (arr, idx, e) -> "ArrayAssign(" ^ string_of_expr arr ^ ", " ^ 
                                string_of_expr idx ^ ", " ^ string_of_expr e ^ ")"
  | ArrayPut (arr, value) -> "ArrayPut(" ^ string_of_expr arr ^ ", " ^ string_of_expr value ^ ")"
  | ArrayPop arr -> "ArrayPop(" ^ string_of_expr arr ^ ")"

and string_of_type = function
  | IntType -> "IntType" | FloatType -> "FloatType" | StringType -> "StringType"
  | BoolType -> "BoolType" | VoidType -> "VoidType" | ArrayType t -> "ArrayType(" ^ string_of_type t ^ ")"

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

let test_array_expressions () =
  check expr_testable "empty array literal"
    (ArrayLit [])
    (parse_expr "print []");
    
  check expr_testable "array literal with integers"
    (ArrayLit [Int 1; Int 2; Int 3])
    (parse_expr "print [1, 2, 3]");
    
  check expr_testable "array literal with expressions"
    (ArrayLit [Int 1; Binop (Add, Int 2, Int 3); Var "x"])
    (parse_expr "print [1, 2 + 3, x]");
    
  check expr_testable "array access"
    (ArrayGet (Var "arr", Int 0))
    (parse_expr "print arr[0]");
    
  check expr_testable "array access with expression index"
    (ArrayGet (Var "arr", Binop (Add, Int 1, Int 2)))
    (parse_expr "print arr[1 + 2]");
    
  check expr_testable "array length"
    (ArrayLen (Var "arr"))
    (parse_expr "print len(arr)");
    
  check expr_testable "array length of array literal"
    (ArrayLen (ArrayLit [Int 1; Int 2; Int 3]))
    (parse_expr "print len([1, 2, 3])") (* ; (Needs to be uncommented with the comment below) *)
    
  (* Nested arrays have not been implemented yet *)
  (* check expr_testable "nested array access"
    (ArrayGet (ArrayGet (Var "matrix", Int 0), Int 1))
    (parse_expr "print matrix[0][1]") *)

let test_type_casting () =
  check expr_testable "cast to int"
    (CastInt (Float 3.14))
    (parse_expr "print int(3.14)");

  check expr_testable "cast to float"
    (CastFloat (Int 42))
    (parse_expr "print float(42)");

  check expr_testable "cast to string"
    (CastString (Int 42))
    (parse_expr "print string(42)");

  check expr_testable "nested type casting"
    (CastString (CastFloat (Int 42)))
    (parse_expr "print string(float(42))")

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
    (match parse_string "if true { print 1 }" with Block [s] -> s | _ -> failwith "Unexpected AST");
    
  check stmt_testable "if-then-else statement"
    (If (Bool true, Block [Print (Int 1)], Block [Print (Int 2)]))
    (match parse_string "if true { print 1 } else { print 2 }" with Block [s] -> s | _ -> failwith "Unexpected AST");
    
  check stmt_testable "while loop"
    (While (Bool true, Block [Print (Int 1)]))
    (match parse_string "while true { print 1 }" with Block [s] -> s | _ -> failwith "Unexpected AST")

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
    (match parse_string "max(a: int, b: int) -> int := { if a > b { return a } else { return b } }" 
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
    (match parse_string "factorial(n: int) -> int := { fact_tail(n: int, acc: int) -> int := { if n = 0 { return acc } else { return fact_tail(n - 1, n * acc) } }; return fact_tail(n, 1) }" 
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
    (parse_string "x: int := 10; while x > 0 { print x; x := x - 1 }")

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
    (parse_string "fibonacci(n: int) -> int := { if n <= 1 { return n } else { return fibonacci(n - 1) + fibonacci(n - 2) } }; print fibonacci(10)");
    
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
    (parse_string "factorial(n: int) -> int := { result: int := 1; while n > 0 { result := result * n; n := n - 1 }; return result }; print factorial(5)")

let test_array_put_pop () =
  (* ArrayPut tests - put(array, value) *)
  check stmt_testable "array put operation with variable"
    (ArrayPut(Var "arr", Int 42))
    (match parse_string "put(arr, 42)" with
     | Block [stmt] -> stmt
     | _ -> failwith "Expected a single statement");
    
  check stmt_testable "array put operation with array literal"
    (ArrayPut(ArrayLit [Int 1; Int 2], Int 3))
    (match parse_string "put([1, 2], 3)" with
     | Block [stmt] -> stmt
     | _ -> failwith "Expected a single statement");
    
  check stmt_testable "array put operation with expressions"
    (ArrayPut(Var "arr", Binop(Add, Int 10, Int 5)))
    (match parse_string "put(arr, 10 + 5)" with
     | Block [stmt] -> stmt
     | _ -> failwith "Expected a single statement");
    
  (* ArrayPop tests - pop(array) *)
  check stmt_testable "array pop operation with variable"
    (ArrayPop(Var "arr"))
    (match parse_string "pop(arr)" with
     | Block [stmt] -> stmt
     | _ -> failwith "Expected a single statement");
    
  check stmt_testable "array pop operation with array literal"
    (ArrayPop(ArrayLit [Int 1; Int 2; Int 3]))
    (match parse_string "pop([1, 2, 3])" with
     | Block [stmt] -> stmt
     | _ -> failwith "Expected a single statement");
    
  check stmt_testable "array pop operation with array access"
    (ArrayPop(ArrayGet(Var "matrix", Int 0)))
    (match parse_string "pop(matrix[0])" with
     | Block [stmt] -> stmt
     | _ -> failwith "Expected a single statement")

let test_array_assign_statements () =
  check stmt_testable "simple array assignment"
    (ArrayAssign (Var "arr", Int 0, Int 42))
    (match parse_string "arr[0] := 42" with Block [s] -> s | _ -> failwith "Unexpected AST");
    
  check stmt_testable "array assignment with variable index"
    (ArrayAssign (Var "arr", Var "i", Int 42))
    (match parse_string "arr[i] := 42" with Block [s] -> s | _ -> failwith "Unexpected AST");
    
  check stmt_testable "array assignment with expression value"
    (ArrayAssign (Var "arr", Int 0, Binop (Add, Int 1, Int 2)))
    (match parse_string "arr[0] := 1 + 2" with Block [s] -> s | _ -> failwith "Unexpected AST");
    
  check stmt_testable "array assignment with expression index"
    (ArrayAssign (Var "arr", Binop (Add, Int 1, Int 2), Int 42))
    (match parse_string "arr[1 + 2] := 42" with Block [s] -> s | _ -> failwith "Unexpected AST");
    
  check stmt_testable "nested array assignment"
    (ArrayAssign (ArrayGet (Var "matrix", Int 0), Int 1, Int 42))
    (match parse_string "matrix[0][1] := 42" with Block [s] -> s | _ -> failwith "Unexpected AST")

let test_array_assign () =
  (* Test array assignment with variable access *)
  check stmt_testable "array assignment with variable"
    (ArrayAssign(Var "arr", Int 0, Int 42))
    (match parse_string "arr[0] := 42" with
     | Block [stmt] -> stmt
     | _ -> failwith "Expected a single statement");
    
  (* Test array assignment with expression index *)
  check stmt_testable "array assignment with expression index"
    (ArrayAssign(Var "arr", Binop(Add, Int 1, Int 2), Int 42))
    (match parse_string "arr[1 + 2] := 42" with
     | Block [stmt] -> stmt
     | _ -> failwith "Expected a single statement");
    
  (* Test array assignment with expression value *)
  check stmt_testable "array assignment with expression value"
    (ArrayAssign(Var "arr", Int 0, Binop(Mult, Int 6, Int 7)))
    (match parse_string "arr[0] := 6 * 7" with
     | Block [stmt] -> stmt
     | _ -> failwith "Expected a single statement");
    
  (* Test array assignment with complex expressions *)
  check stmt_testable "array assignment with complex expressions"
    (ArrayAssign(
      ArrayGet(Var "matrix", Int 0),
      Binop(Add, Var "i", Int 1),
      FunCall("max", [Var "x"; Var "y"])
    ))
    (match parse_string "matrix[0][i + 1] := max(x, y)" with
     | Block [stmt] -> stmt
     | _ -> failwith "Expected a single statement")

let test_array_length_update () =
  (* Test that the array length updates after put operations *)
  check stmt_testable "array length update after put"
    (Block [
      Declare ("arr", ArrayType IntType, ArrayLit []);
      ArrayPut (Var "arr", Int 1);
      Print (ArrayLen (Var "arr"))
    ])
    (parse_string "arr: int[] := []; put(arr, 1); print len(arr)");
    
  (* Test that the array length updates after pop operations *)
  check stmt_testable "array length update after pop"
    (Block [
      Declare ("arr", ArrayType IntType, ArrayLit [Int 1; Int 2; Int 3]);
      ArrayPop (Var "arr");
      Print (ArrayLen (Var "arr"))
    ])
    (parse_string "arr: int[] := [1, 2, 3]; pop(arr); print len(arr)")

(* Test suite *)
let suite =
  [
    "Basic Expressions", `Quick, test_basic_expressions;
    "Arithmetic Expressions", `Quick, test_arithmetic_expressions;
    "Logical Expressions", `Quick, test_logical_expressions;
    "Comparison Expressions", `Quick, test_comparison_expressions;
    "Complex Expressions", `Quick, test_complex_expressions;
    "Array Expressions", `Quick, test_array_expressions;
    "Type Casting", `Quick, test_type_casting;
    "Assignment Statements", `Quick, test_assignment_statements;
    "Control Flow Statements", `Quick, test_control_flow_statements;
    "Function Statements", `Quick, test_function_statements;
    "Nested Functions", `Quick, test_nested_functions;
    "Complex Statements", `Quick, test_complex_statements;
    "Operator Precedence", `Quick, test_operator_precedence;
    "Complete Programs", `Quick, test_complete_programs;
    "Array Put and Pop Operations", `Quick, test_array_put_pop;
    "Array Assign Statements", `Quick, test_array_assign_statements;
    "Array Assign", `Quick, test_array_assign;
    "Array Length Update", `Quick, test_array_length_update;
  ]

(* Run the tests *)
let () = Alcotest.run "Parser Tests" [("Parser", suite)]