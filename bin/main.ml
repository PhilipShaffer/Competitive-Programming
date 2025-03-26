open Competitive_programming

(* Debug function to print all tokens *)
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
          | Parser.PLUS -> "PLUS"
          | Parser.MINUS -> "MINUS"
          | Parser.MULT -> "MULT"
          | Parser.DIV -> "DIV"
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
          | Parser.ASSIGN -> "ASSIGN"
          | Parser.LPAREN -> "LPAREN"
          | Parser.RPAREN -> "RPAREN"
          | Parser.LBRACE -> "LBRACE"
          | Parser.RBRACE -> "RBRACE"
          | Parser.SEMICOLON -> "SEMICOLON"
          | Parser.MOD -> "MOD"
          | Parser.EOF -> "EOF"
        );
        loop ()
  in
  try loop () with _ -> Printf.printf "Error while tokenizing\n"

let parse (s : string) : Ast.stmt =
  let lexbuf = Lexing.from_string s in
  try
    (* Set position information for error reporting *)
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_fname = "input";
      Lexing.pos_lnum = 1;
    };
    let ast = Parser.main Lexer.read lexbuf in
    ast
  with
  | Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      Printf.eprintf "Parse error at line %d character %d\n"
        pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
      exit 1
  | e ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
      exit 1

(* Function to read a file *)
let read_file filename =
  let channel = open_in filename in
  let content = really_input_string channel (in_channel_length channel) in
  close_in channel;
  content

let () =
  (* Parse command line arguments *)
  let interpret_mode = ref true in
  let mips_output = ref None in
  let source_file = ref None in
  
  let spec_list = [
    ("--mips", Arg.String (fun s -> interpret_mode := false; mips_output := Some s),
     "Compile to MIPS and save to the specified file");
  ] in

  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " [--mips output.s] source_file" in
  
  let anon_fun filename =
    match !source_file with
    | None -> source_file := Some filename
    | Some _ -> raise (Arg.Bad "Too many source files provided")
  in
  
  Arg.parse spec_list anon_fun usage_msg;
  
  let filename = match !source_file with
    | None -> failwith "No input file provided"
    | Some f -> f
  in
  
  let program = read_file filename in
  Printf.printf "Reading program from file: %s\n" filename;
  Printf.printf "Program contents:\n%s\n" program;
  
  print_tokens program;
  let ast = parse program in
  print_endline "Parsing successful!";
  
  if !interpret_mode then
    (* Run the interpreter *)
    Interpreter.interpret ast
  else
    (* Compile to MIPS *)
    match !mips_output with
    | None -> failwith "No output file specified for MIPS compilation"
    | Some output_file ->
        Printf.printf "Compiling to MIPS, output file: %s\n" output_file;
        Mips_codegen.compile_to_mips ast output_file;
        Printf.printf "MIPS code generation complete\n"