(* Lexer for while language *)
(* Uses Ocamllex to compile to a proper lexer *)

{
  (* This is the header section where OCaml code can be included.
     It's typically used for imports and helper functions. *)
  open Parser  (* Import the Parser module to use its tokens *)
}

(* Definitions of regular expressions for different types of tokens *)
let white  = [' ' '\t']+              (* Whitespace characters *)
let newline = '\n'                    (* Newline character *)
let digit  = ['0'-'9']                (* Digits *)
let int    = '-'? digit+              (* Integer literals, optionally starting with a minus sign *)
let float  = '-'? digit+ '.' digit+   (* Float literals, optionally starting with a minus sign *)
let letter = ['a'-'z' 'A'-'Z' '_']    (* Letters *)
let id     = letter (letter | digit)* (* Identifiers: starts with a letter, followed by letters or digits *)

(* The main lexer rule - defines how the lexer should process the input *)
rule read =
  parse
  | white   { read lexbuf } (* Ignore whitespace *)
  | newline { Lexing.new_line lexbuf; read lexbuf } (* Ignore newlines *)
  | "//"    { read_single_line_comment lexbuf }     (* Single-line comment *)
  | "/*"    { read_multi_line_comment lexbuf }      (* Multi-line comment *)
  | "(*"    { read_ocaml_style_comment lexbuf }     (* OCaml-style comment *)
  | "true"  { BOOL true }   (* Boolean literal true *)
  | "false" { BOOL false }  (* Boolean literal false *)
  | "if"    { IF }          (* If keyword *)
  | "then"  { THEN }        (* Then keyword *)
  | "else"  { ELSE }        (* Else keyword *)
  | "print" { PRINT }       (* Print keyword *)
  | "while" { WHILE }       (* While keyword *)
  | "do"    { DO }          (* Do keyword *)
  | "and"   { AND }         (* Logical AND operator *)
  | "or"    { OR }          (* Logical OR operator *)
  | "not"   { NOT }         (* Logical NOT operator *)
  | "return"{ RETURN }      (* Return keyword *)
  | "put"   { PUT }         (* Array Put keyword *)
  | "pop"   { POP }         (* Array Pop keyword *)
  | "len"   { LEN }         (* Array length keyword *)
  | "->"    { ARROW }       (* Arrow for function return type *)
  | ","     { COMMA }       (* Comma for parameter/argument separation *)
  | "void"  { VOIDTYPE }    (* Void type keyword *)
  | "int"   { INTTYPE }     (* Integer type *)
  | "float" { FLOATTYPE }   (* Float type *)
  | "string"{ STRINGTYPE }  (* String type *)
  | "bool"  { BOOLTYPE }    (* Boolean type *)
  | id      { ID (Lexing.lexeme lexbuf) }                       (* Identifiers *)
  | int     { INT (int_of_string (Lexing.lexeme lexbuf)) }      (* Integer literals *)
  | '"'     { read_string (Buffer.create 16) lexbuf }           (* Start of a string *)  
  | float   { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }  (* Float literals *)
  | "+"     { PLUS }      (* Addition operator *)
  | "-"     { MINUS }     (* Subtraction operator *)
  | "*"     { MULT }      (* Multiplication operator *)
  | "/"     { DIV }       (* Division operator *)
  | "%"     { MOD }       (* Modulo operator *)
  | "<"     { LT }        (* Less than operator *)
  | "<="    { LEQ }       (* Less than or equal operator *)
  | ">"     { GT }        (* Greater than operator *)
  | ">="    { GEQ }       (* Greater than or equal operator *)
  | "="     { EQ }        (* Equality operator *)
  | "!="    { NEQ }       (* Inequality operator *)
  | ":="    { ASSIGN }    (* Assignment operator *)
  | "("     { LPAREN }    (* Left parenthesis *)
  | ")"     { RPAREN }    (* Right parenthesis *)
  | "{"     { LBRACE }    (* Left brace *)
  | "}"     { RBRACE }    (* Right brace *)
  | "["     { LBRACKET }  (* Left bracket for arrays *)
  | "]"     { RBRACKET }  (* Right bracket for arrays *)
  | ";"     { SEMICOLON } (* Semicolon *)
  | ":"     { COLON }     (* Colon *)
  | eof     { EOF }       (* End-of-file *)
  | _ { raise (Failure ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }  (* Catch-all for unexpected characters *)

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '"'  { Buffer.add_char buf '"'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | [^ '"' '\\']+ { 
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf 
    }
  | eof { raise (Failure "Unterminated string") }
  | _   { raise (Failure ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }

(* Rule for single-line comments: reads until end of line and discards *)
and read_single_line_comment =
  parse
  | newline { Lexing.new_line lexbuf; read lexbuf } (* End of comment, return to main lexer *)
  | eof     { EOF }                                 (* End of file *)
  | _       { read_single_line_comment lexbuf }     (* Ignore anything else *)

(* Rule for multi-line C-style comments: reads until "*/" and discards *)
and read_multi_line_comment =
  parse
  | "*/"    { read lexbuf }                         (* End of comment, return to main lexer *)
  | newline { Lexing.new_line lexbuf; read_multi_line_comment lexbuf }
  | eof     { raise (Failure "Unterminated multi-line comment") }
  | _       { read_multi_line_comment lexbuf }      (* Ignore anything else *)

(* Rule for OCaml-style comments (*...*)  *)
and read_ocaml_style_comment =
  parse
  | "*)"    { read lexbuf }                         (* End of comment, return to main lexer *)
  | newline { Lexing.new_line lexbuf; read_ocaml_style_comment lexbuf }
  | eof     { raise (Failure "Unterminated OCaml-style comment") }
  | _       { read_ocaml_style_comment lexbuf }     (* Ignore anything else *)