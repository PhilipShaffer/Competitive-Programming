
(* The type of tokens. *)

type token = 
  | WHILE
  | THEN
  | SEMICOLON
  | RPAREN
  | RBRACE
  | PRINT
  | PLUS
  | OR
  | NOT
  | NEQ
  | MULT
  | MOD
  | MINUS
  | LT
  | LPAREN
  | LET
  | LEQ
  | LBRACE
  | INT of (int)
  | IN
  | IF
  | ID of (string)
  | GT
  | GEQ
  | EQ
  | EOF
  | ELSE
  | DO
  | DIV
  | BOOL of (bool)
  | ASSIGN
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.stmt)
