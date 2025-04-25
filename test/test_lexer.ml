open Alcotest
open Libraries (* Access Lexer, Parser, Parser_utils *)
open Parser (* Need this for token types *)

(* Add this function definition before the test cases *)
let token_to_string = function
  | IF -> "IF" | THEN -> "THEN" | ELSE -> "ELSE" | PRINT -> "PRINT"
  | WHILE -> "WHILE" | DO -> "DO" | IN -> "IN" | AND -> "AND" | OR -> "OR"
  | NOT -> "NOT" | RETURN -> "RETURN" | ARROW -> "ARROW" | COMMA -> "COMMA"
  | VOIDTYPE -> "VOIDTYPE" | INTTYPE -> "INTTYPE" | FLOATTYPE -> "FLOATTYPE"
  | STRINGTYPE -> "STRINGTYPE" | BOOLTYPE -> "BOOLTYPE"
  | ID s -> Printf.sprintf "ID(%s)" s
  | INT i -> Printf.sprintf "INT(%d)" i
  | STRING s -> Printf.sprintf "STRING(%s)" s
  | FLOAT f -> Printf.sprintf "FLOAT(%f)" f
  | BOOL b -> Printf.sprintf "BOOL(%b)" b
  | PLUS -> "PLUS" | MINUS -> "MINUS" | MULT -> "MULT" | DIV -> "DIV" | MOD -> "MOD"
  | LT -> "LT" | LEQ -> "LEQ" | GT -> "GT" | GEQ -> "GEQ" | EQ -> "EQ" | NEQ -> "NEQ"
  | ASSIGN -> "ASSIGN" | LPAREN -> "LPAREN" | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE" | RBRACE -> "RBRACE" | SEMICOLON -> "SEMICOLON"
  | COLON -> "COLON" | EOF -> "EOF"
  (* Add cases for LET if it's in your tokens, based on parser.mly *)
  | LET -> "LET" (* Assuming LET is a token *)

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
    [IF; THEN; ELSE; PRINT; WHILE; DO; IN; AND; OR; NOT; RETURN; VOIDTYPE; INTTYPE; FLOATTYPE; STRINGTYPE; BOOLTYPE]
    (lex_string "if then else print while do in and or not return void int float string bool")

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
    [ID "x"; ID "myVar"; ID "anotherID123"]
    (lex_string "x myVar anotherID123")

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

(* Test suite *)
let suite =
  [
    "Keywords", `Quick, test_keywords;
    "Operators", `Quick, test_operators;
    "Literals", `Quick, test_literals;
    "Identifiers", `Quick, test_identifiers;
    "Whitespace and Newlines", `Quick, test_whitespace_and_newlines;
    "Mixed Input", `Quick, test_mixed;
  ]

(* Run the tests *)
let () = Alcotest.run "Lexer Tests" [("Lexer", suite)]

