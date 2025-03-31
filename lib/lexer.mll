(* Lexer for while language *)
(* Uses Ocamllex to compile to a proper lexer *)

{
  (* This is the header section where OCaml code can be included.
     It's typically used for imports and helper functions. *)
  open Parser  (* Import the Parser module to use its tokens *)
}

(* Definitions of regular expressions for different types of tokens *)
let white  = [' ' '\t']+        (* Whitespace characters *)
let newline = '\n'              (* Newline character *)
let digit  = ['0'-'9']          (* Digits *)
let int    = '-'? digit+        (* Integer literals, optionally starting with a minus sign *)
let float  = '-'? digit+ '.' digit+
let letter = ['a'-'z' 'A'-'Z']  (* Letters *)
let id     = letter (letter | digit)* (* Identifiers: starts with a letter, followed by letters or digits *)
let string = '"' ([^'"'] | newline)* '"'

(* The main lexer rule - defines how the lexer should process the input *)
rule read =
  parse
  | white   { read lexbuf }  (* Ignore whitespace *)
  | newline { Lexing.new_line lexbuf; read lexbuf }  (* Ignore newlines *)
  | "true"  { BOOL true }  (* Boolean literal true *)
  | "false" { BOOL false }  (* Boolean literal false *)
  | "if"    { IF }  (* If keyword *)
  | "then"  { THEN }  (* Then keyword *)
  | "else"  { ELSE }  (* Else keyword *)
  | "print" { PRINT }  (* Print keyword *)
  | "while" { WHILE }  (* While keyword *)
  | "do"    { DO }  (* Do keyword *)
  | "let"   { LET }  (* Let keyword *)
  | "in"    { IN }  (* In keyword *)
  | "and"   { AND }  (* Logical AND operator *)
  | "or"    { OR }  (* Logical OR operator *)
  | "not"   { NOT }  (* Logical NOT operator *)
  | id      { ID (Lexing.lexeme lexbuf) }  (* Identifiers *)
  | int     { INT (int_of_string (Lexing.lexeme lexbuf)) }  (* Integer literals *)
  | string  { STRING (Lexing.lexeme lexbuf)}
  | float   { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }  (* Float literals *)
  | "+"     { PLUS }  (* Addition operator *)
  | "-"     { MINUS }  (* Subtraction operator *)
  | "*"     { MULT }  (* Multiplication operator *)
  | "/"     { DIV }  (* Division operator *)
  | "%"     { MOD }  (* Modulo operator *)
  | "<"     { LT }  (* Less than operator *)
  | "<="    { LEQ }  (* Less than or equal operator *)
  | ">"     { GT }  (* Greater than operator *)
  | ">="    { GEQ }  (* Greater than or equal operator *)
  | "="     { EQ }  (* Equality operator *)
  | "!="    { NEQ }  (* Inequality operator *)
  | ":="    { ASSIGN }  (* Assignment operator *)
  | "("     { LPAREN }  (* Left parenthesis *)
  | ")"     { RPAREN }  (* Right parenthesis *)
  | "{"     { LBRACE }  (* Left brace *)
  | "}"     { RBRACE }  (* Right brace *)
  | ";"     { SEMICOLON }  (* Semicolon *)
  | eof     { EOF }  (* End-of-file *)