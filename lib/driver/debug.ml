open Frontend

let print_tokens s =
  let lexbuf = Lexing.from_string s in
  Printf.printf "Tokens:\n";
  let rec loop () =
    let token = Lexer.read lexbuf in
    match token with
    | Parser.EOF -> Printf.printf "EOF\n"
    | _ -> 
        Printf.printf "%s\n" (
          match token with
          | Parser.INT n -> Printf.sprintf "INT(%d)" n
          | Parser.ID s -> Printf.sprintf "ID(%s)" s
          | Parser.BOOL b -> Printf.sprintf "BOOL(%b)" b
          | Parser.STRING str -> Printf.sprintf "STRING(%s)" str
          | Parser.FLOAT f -> Printf.sprintf "FLOAT(%f)" f
          | Parser.PLUS -> "PLUS"
          | Parser.MINUS -> "MINUS"
          | Parser.MULT -> "MULT"
          | Parser.DIV -> "DIV"
          | Parser.MOD -> "MOD"
          | Parser.LT -> "LT"
          | Parser.LEQ -> "LEQ"
          | Parser.GT -> "GT"
          | Parser.GEQ -> "GEQ"
          | Parser.EQ -> "EQ"
          | Parser.NEQ -> "NEQ"
          | Parser.AND -> "AND"
          | Parser.OR -> "OR"
          | Parser.NOT -> "NOT"
          | Parser.IF -> "IF"
          | Parser.THEN -> "THEN"
          | Parser.ELSE -> "ELSE"
          | Parser.PRINT -> "PRINT"
          | Parser.WHILE -> "WHILE"
          | Parser.DO -> "DO"
          | Parser.LET -> "LET"
          | Parser.IN -> "IN"
          | Parser.COLON -> "COLON"
          | Parser.ASSIGN -> "ASSIGN"
          | Parser.LPAREN -> "LPAREN"
          | Parser.RPAREN -> "RPAREN"
          | Parser.LBRACE -> "LBRACE"
          | Parser.RBRACE -> "RBRACE"
          | Parser.SEMICOLON -> "SEMICOLON"
          | Parser.INTTYPE -> "INTTYPE"
          | Parser.FLOATTYPE -> "FLOATTYPE"
          | Parser.STRINGTYPE -> "STRINGTYPE"
          | Parser.BOOLTYPE -> "BOOLTYPE"
          | Parser.EOF -> "EOF"
        );
        loop ()
  in
  try loop () with _ -> Printf.printf "Error while tokenizing\n"

let read_file filename =
  let channel = open_in filename in
  let content = really_input_string channel (in_channel_length channel) in
  close_in channel;
  content 