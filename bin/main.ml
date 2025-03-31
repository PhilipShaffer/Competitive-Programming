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
          | Parser.STRING str -> Printf.sprintf "STRING(%s)" str
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

(* Function to read a file *)
let read_file filename =
  let channel = open_in filename in
  let content = really_input_string channel (in_channel_length channel) in
  close_in channel;
  content

(* Improved structure with modules *)
module Compiler = struct
  type target = LLVM_IR | Assembly | Executable
  
  type compiler_error = 
    | ParseError of string
    | CompileError of string
    | RuntimeError of string

  (* Parse source code into AST *)
  let parse (s : string) : (Ast.stmt, compiler_error) result =
    let lexbuf = Lexing.from_string s in
    try
      (* Set position information for error reporting *)
      let pos = lexbuf.Lexing.lex_curr_p in
      lexbuf.Lexing.lex_curr_p <- { pos with
        Lexing.pos_fname = "input";
        Lexing.pos_lnum = 1;
      };
      let ast = Parser.main Lexer.read lexbuf in
      Ok ast
    with
    | Parser.Error ->
        let pos = Lexing.lexeme_start_p lexbuf in
        Error (ParseError (Printf.sprintf "Parse error at line %d character %d"
          pos.Lexing.pos_lnum
          (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)))
    | e ->
        Error (ParseError (Printf.sprintf "Unexpected error: %s" (Printexc.to_string e)))

  (* Get compiler tool paths from environment or use defaults *)
  let get_tool_path tool default_path =
    try Sys.getenv (String.uppercase_ascii tool ^ "_PATH")
    with Not_found -> default_path
  
  let llc_path = get_tool_path "llc" "/opt/homebrew/opt/llvm@18/bin/llc"
  let clang_path = get_tool_path "clang" "clang"

  (* Error handling for executing shell commands *)
  let safe_execute cmd =
    Printf.printf "Executing: %s\n" cmd;
    match Unix.system cmd with
    | Unix.WEXITED 0 -> Ok ()
    | Unix.WEXITED code -> 
        Error (RuntimeError (Printf.sprintf "Command failed with code %d: %s" code cmd))
    | Unix.WSIGNALED sig_num -> 
        Error (RuntimeError (Printf.sprintf "Command killed by signal %d: %s" sig_num cmd))
    | Unix.WSTOPPED sig_num -> 
        Error (RuntimeError (Printf.sprintf "Command stopped by signal %d: %s" sig_num cmd))

  (* Compile AST to LLVM module *)
  let compile_ast ast =
    try
      let module_val = Codegen.compile ast in
      Ok module_val
    with
    | e -> Error (CompileError (Printf.sprintf "Compilation error: %s" (Printexc.to_string e)))

  (* Compile to different targets *)
  let rec compile_to target output_name ast =
    match target with
    | LLVM_IR -> 
        (match compile_ast ast with
         | Ok module_val -> 
             Printf.printf "LLVM IR generation successful!\n";
             (* Use module_val to create the output file *)
             let output_file = output_name ^ ".ll" in
             let out_str = Llvm.string_of_llmodule module_val in
             let oc = open_out output_file in
             output_string oc out_str;
             close_out oc;
             Ok output_file
         | Error e -> Error e)
    | Assembly ->
        (match compile_to LLVM_IR output_name ast with
         | Ok llvm_file -> 
             (match safe_execute (Printf.sprintf "%s %s -o %s.s" llc_path llvm_file output_name) with
              | Ok () -> Ok (output_name ^ ".s")
              | Error e -> Error e)
         | Error e -> Error e)
    | Executable ->
        (match compile_to LLVM_IR output_name ast with
         | Ok llvm_file -> 
             (match safe_execute (Printf.sprintf "%s %s -o %s" clang_path llvm_file output_name) with
              | Ok () -> Ok output_name
              | Error e -> Error e)
         | Error e -> Error e)

  (* Run a compiled executable *)
  let run executable =
    Printf.printf "Executing: %s\n" executable;
    match Unix.system (Printf.sprintf "./%s" executable) with
    | Unix.WEXITED code -> 
        Printf.printf "Program exited with code: %d\n" code;
        Ok code
    | Unix.WSIGNALED sig_num -> 
        Error (RuntimeError (Printf.sprintf "Program killed by signal %d" sig_num))
    | Unix.WSTOPPED sig_num -> 
        Error (RuntimeError (Printf.sprintf "Program stopped by signal %d" sig_num))

  (* Full compilation pipeline from source to running the program *)
  let compile_and_run ?(show_tokens=false) ?(target=Executable) filename output_name =
    Printf.printf "Reading program from file: %s\n" filename;
    let program = read_file filename in
    Printf.printf "Program contents:\n%s\n" program;
    
    if show_tokens then
      print_tokens program;
    
    match parse program with
    | Error e -> Error e
    | Ok ast -> 
        Printf.printf "Parsing successful!\n";
        match compile_to target output_name ast with
        | Error e -> Error e
        | Ok executable ->
            if target = Executable then
              match run executable with
              | Ok _exit_code -> Ok executable
              | Error e -> Error e
            else
              Ok executable
end

let () =
  (* Command line argument parsing *)
  let filename = ref "" in
  let output_name = ref "program" in
  let show_tokens = ref false in
  let target_type = ref Compiler.Executable in
  
  let set_target s =
    match String.lowercase_ascii s with
    | "ir" | "llvm" | "llvm-ir" -> target_type := Compiler.LLVM_IR
    | "asm" | "assembly" -> target_type := Compiler.Assembly
    | "exe" | "executable" -> target_type := Compiler.Executable
    | _ -> raise (Arg.Bad "Target must be one of: llvm-ir, assembly, executable")
  in
  
  let spec = [
    ("-o", Arg.Set_string output_name, " Output file name (default: program)");
    ("-t", Arg.Bool (fun b -> show_tokens := b), " Show tokens (default: false)");
    ("-target", Arg.String set_target, " Target type: llvm-ir, assembly, executable (default: executable)");
  ] in
  
  let usage = "Usage: compiler [options] file.src" in
  
  Arg.parse spec (fun s -> filename := s) usage;
  
  if !filename = "" then begin
    Printf.eprintf "Error: No input file specified\n";
    Arg.usage spec usage;
    exit 1
  end;
  
  (* Run the compiler with the specified options *)
  match Compiler.compile_and_run ~show_tokens:!show_tokens ~target:!target_type !filename !output_name with
  | Ok output -> Printf.printf "Compilation successful! Output: %s\n" output
  | Error (Compiler.ParseError msg) -> 
      Printf.eprintf "Parse error: %s\n" msg;
      exit 1
  | Error (Compiler.CompileError msg) -> 
      Printf.eprintf "Compilation error: %s\n" msg;
      exit 1
  | Error (Compiler.RuntimeError msg) -> 
      Printf.eprintf "Runtime error: %s\n" msg;
      exit 1