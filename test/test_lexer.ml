open Alcotest
open Frontend (* Access Lexer, Parser, Parser_utils *)

(* Helper function to lex a string into a list of tokens *)
let lex_string s =
  let lexbuf = Lexing.from_string s in
  let rec tokens acc =
    match Lexer.read lexbuf with
    | Parser.EOF -> List.rev (Parser.EOF :: acc) (* Include EOF *)
    | token -> tokens (token :: acc)
  in
  tokens []

(* Define how to print and compare Parser.token for Alcotest *)
let token_testable =
  testable (fun ppf tok -> Fmt.pf ppf "%s" (Parser_utils.string_of_token tok)) (=)

(* --- Lexer Tests --- *)

let test_lexer_simple () =
  let input = "if x > 1 then y := 2 else z := 0;" in
  let expected =
    [ Parser.IF; Parser.ID "x"; Parser.GT; Parser.INT 1; Parser.THEN;
      Parser.ID "y"; Parser.ASSIGN; Parser.INT 2; Parser.ELSE;
      Parser.ID "z"; Parser.ASSIGN; Parser.INT 0; Parser.SEMICOLON; Parser.EOF ]
  in
  check (list token_testable) "Simple lex" expected (lex_string input)

let test_lexer_numbers () =
  let input = "123 -45 0 3.14 -0.5" in
  let expected =
    [ Parser.INT 123; Parser.INT (-45); Parser.INT 0;
      Parser.FLOAT 3.14; Parser.FLOAT (-0.5); Parser.EOF ]
  in
  check (list token_testable) "Numbers lex" expected (lex_string input)

let test_lexer_string () =
  let input = "\"hello world\" \"escaped \\\" \\n \\t \\\\\"" in
  let expected =
    [ Parser.STRING "hello world"; Parser.STRING "escaped \" \n \t \\"; Parser.EOF ]
  in
  check (list token_testable) "String lex" expected (lex_string input)

(* --- Test Suite --- *)

let lexer_suite =
  [
    ("Lexer", [
      test_case "Simple sequence" `Quick test_lexer_simple;
      test_case "Numbers" `Quick test_lexer_numbers;
      test_case "Strings" `Quick test_lexer_string;
    ]);
  ]

(* Run the tests *)
let () =
  Alcotest.run "Lexer Tests" lexer_suite