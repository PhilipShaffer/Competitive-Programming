open Ast

let parse (s : string) : stmt =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.read lexbuf in
  ast