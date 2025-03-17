open Competitive_programming

let parse (s : string) : Ast.stmt =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.read lexbuf in
  ast

let () =
  let test_program = "let x = 5 in print x" in
  let _ = parse test_program in
  print_endline "Parsing successful!"