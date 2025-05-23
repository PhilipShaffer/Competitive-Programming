open Alcotest
open Libraries (* Access Lexer, Parser, Parser_utils *)
open Parser (* Need this for token types *)

(* Add this function definition before the test cases *)
let token_to_string = function
  | IF -> "IF" | ELSE -> "ELSE" | PRINT -> "PRINT"
  | WHILE -> "WHILE" | AND -> "AND" | OR -> "OR"
  | NOT -> "NOT" | RETURN -> "RETURN" | ARROW -> "ARROW" | COMMA -> "COMMA"
  | VOIDTYPE -> "VOIDTYPE" | INTTYPE -> "INTTYPE" | FLOATTYPE -> "FLOATTYPE"
  | STRINGTYPE -> "STRINGTYPE" | BOOLTYPE -> "BOOLTYPE" | ASSIGN -> "ASSIGN"
  | LPAREN -> "LPAREN" | RPAREN -> "RPAREN" | LBRACE -> "LBRACE" | RBRACE -> "RBRACE"
  | SEMICOLON -> "SEMICOLON" | COLON -> "COLON" | EOF -> "EOF"
  | LBRACKET -> "LBRACKET" | RBRACKET -> "RBRACKET" | LEN -> "LEN" | PUT -> "PUT" | POP -> "POP"
  | ID s -> Printf.sprintf "ID(%s)" s
  | INT i -> Printf.sprintf "INT(%d)" i
  | STRING s -> Printf.sprintf "STRING(%s)" s
  | FLOAT f -> Printf.sprintf "FLOAT(%f)" f
  | BOOL b -> Printf.sprintf "BOOL(%b)" b
  | PLUS -> "PLUS" | MINUS -> "MINUS" | MULT -> "MULT" | DIV -> "DIV" | MOD -> "MOD"
  | LT -> "LT" | LEQ -> "LEQ" | GT -> "GT" | GEQ -> "GEQ" | EQ -> "EQ" | NEQ -> "NEQ"

(* Helper function to lex a string and return a list of tokens *)
let lex_string s =
  let lexbuf = Lexing.from_string s in
  let rec aux acc =
    match Lexer.read lexbuf with
    | EOF -> List.rev acc
    | token -> aux (token :: acc)
  in
  aux []

(* Define the token testable for Alcotest *)
let token_testable =
  testable (fun ppf tok -> Fmt.pf ppf "%s" (token_to_string tok)) (=)

(* Test cases *)
let test_keywords () =
  check (list token_testable) "keywords"
    [IF; ELSE; PRINT; WHILE; AND; OR; NOT; LEN; PUT; POP; RETURN; VOIDTYPE; INTTYPE; FLOATTYPE; STRINGTYPE; BOOLTYPE]
    (lex_string "if else print while and or not len put pop return void int float string bool")

let test_operators () =
  check (list token_testable) "operators"
    [PLUS; MINUS; MULT; DIV; MOD; LT; LEQ; GT; GEQ; EQ; NEQ; ASSIGN; ARROW; COMMA; LPAREN; RPAREN; LBRACE; RBRACE; SEMICOLON; COLON]
    (lex_string "+ - * / % < <= > >= = != := -> , ( ) { } ; :")

let test_literals () =
  check (list token_testable) "literals"
    [INT 123; INT (-45); FLOAT 3.14; FLOAT (-0.5); BOOL true; BOOL false; STRING "hello"; STRING "with\nescape"; STRING ""]
    (lex_string "123 -45 3.14 -0.5 true false \"hello\" \"with\\nescape\" \"\"")

let test_identifiers () =
  check (list token_testable) "identifiers"
    [ID "x"; ID "myVar"; ID "anotherID123"; INT 123; ID "acd"]
    (lex_string "x myVar anotherID123 123acd")

let test_whitespace_and_newlines () =
  check (list token_testable) "whitespace and newlines"
    [INT 1; ID "a"; BOOL true]
    (lex_string "  1
	 a 
 true ")

let test_mixed () =
  check (list token_testable) "mixed input"
    [INTTYPE; ID "main"; LPAREN; RPAREN; LBRACE; PRINT; LPAREN; STRING "Hello"; RPAREN; SEMICOLON; RETURN; INT 0; SEMICOLON; RBRACE]
    (lex_string "int main() { print(\"Hello\"); return 0; }")

(* Additional test cases *)

let test_string_escapes () =
  check (list token_testable) "string escape sequences"
    [STRING "tab\tcharacter"; STRING "newline\ncharacter"; STRING "quote\"character"; STRING "backslash\\character"]
    (lex_string "\"tab\\tcharacter\" \"newline\\ncharacter\" \"quote\\\"character\" \"backslash\\\\character\"")

let test_keyword_like_identifiers () =
  check (list token_testable) "keyword-like identifiers"
    [ID "ifs"; ID "whiles"; ID "printable"; ID "ifelse"; ID "intx"]
    (lex_string "ifs whiles printable ifelse intx")

let test_function_declaration () =
  check (list token_testable) "function declaration"
    [ID "fibonacci"; LPAREN; ID "n"; COLON; INTTYPE; RPAREN; ARROW; INTTYPE; ASSIGN; LBRACE; RETURN; ID "n"; RBRACE]
    (lex_string "fibonacci(n: int) -> int := { return n }")

let test_complex_expressions () =
  check (list token_testable) "complex expressions"
    [ID "x"; ASSIGN; LPAREN; ID "y"; PLUS; INT 2; RPAREN; MULT; LPAREN; ID "z"; MINUS; INT 3; RPAREN; SEMICOLON]
    (lex_string "x := (y + 2) * (z - 3);")

let test_edge_case_numbers () =
  check (list token_testable) "edge case numbers"
    [INT 0; FLOAT 0.0; FLOAT 0.123; FLOAT 3.0; INT 999999]
    (lex_string "0 0.0 0.123 3.0 999999")

let test_array_tokens () =
  check (list token_testable) "array tokens"
    [LBRACKET; RBRACKET; LEN]
    (lex_string "[ ] len")

let test_array_expressions () =
  check (list token_testable) "array expressions"
    [ID "arr"; ASSIGN; LBRACKET; INT 1; COMMA; INT 2; COMMA; INT 3; RBRACKET; SEMICOLON; 
     ID "x"; ASSIGN; ID "arr"; LBRACKET; INT 0; RBRACKET; SEMICOLON;
     ID "l"; ASSIGN; LEN; ID "arr"]
    (lex_string "arr := [1, 2, 3]; x := arr[0]; l := len arr")

let test_array_put_and_pop () =
  check (list token_testable) "array put and pop operations"
    [PUT; ID "arr"; COMMA; INT 42; SEMICOLON;
     POP; ID "arr"; SEMICOLON;
     ID "x"; ASSIGN; PUT; ID "arr"; COMMA; ID "value"; SEMICOLON;
     ID "y"; ASSIGN; POP; ID "arr"]
    (lex_string "put arr, 42; pop arr; x := put arr, value; y := pop arr")

let test_len () =
  check (list token_testable) "len"
    [LEN; ID "arr"; SEMICOLON]
    (lex_string "len arr;")

(* Test suite *)
let suite =
  [
    "Keywords", `Quick, test_keywords;
    "Operators", `Quick, test_operators;
    "Literals", `Quick, test_literals;
    "Identifiers", `Quick, test_identifiers;
    "Whitespace and Newlines", `Quick, test_whitespace_and_newlines;
    "Mixed Input", `Quick, test_mixed;
    "String Escape Sequences", `Quick, test_string_escapes;
    "Keyword-like Identifiers", `Quick, test_keyword_like_identifiers;
    "Function Declaration", `Quick, test_function_declaration;
    "Complex Expressions", `Quick, test_complex_expressions;
    "Edge Case Numbers", `Quick, test_edge_case_numbers;
    "Array Tokens", `Quick, test_array_tokens;
    "Array Expressions", `Quick, test_array_expressions;
    "Array Put and Pop Operations", `Quick, test_array_put_and_pop;
    "Len", `Quick, test_len;
  ]

(* Run the tests *)
let () = Alcotest.run "Lexer Tests" [("Lexer", suite)]

