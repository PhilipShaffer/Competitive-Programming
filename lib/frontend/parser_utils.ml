open Parser (* To access token types *)

(* Convert a token to its string representation for debugging/testing *)
let string_of_token = function
  | EOF -> "EOF"
  | INTTYPE -> "INTTYPE"
  | FLOATTYPE -> "FLOATTYPE"
  | STRINGTYPE -> "STRINGTYPE"
  | BOOLTYPE -> "BOOLTYPE"
  | ID s -> Printf.sprintf "ID(%s)" s
  | STRING s -> Printf.sprintf "STRING(%s)" s
  | INT i -> Printf.sprintf "INT(%d)" i
  | FLOAT f -> Printf.sprintf "FLOAT(%f)" f
  | BOOL b -> Printf.sprintf "BOOL(%b)" b
  | LT -> "LT"
  | LEQ -> "LEQ"
  | GT -> "GT"
  | GEQ -> "GEQ"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | PRINT -> "PRINT"
  | WHILE -> "WHILE"
  | DO -> "DO"
  | ASSIGN -> "ASSIGN"
  | RETURN -> "RETURN"
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | MULT -> "MULT"
  | DIV -> "DIV"
  | MOD -> "MOD"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | SEMICOLON -> "SEMICOLON"
  | COLON -> "COLON"
  | COMMA -> "COMMA"
  | ARROW -> "ARROW"
