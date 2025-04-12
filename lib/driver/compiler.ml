type target = LLVM_IR | Assembly | Executable

type compiler_error = 
  | ParseError of string
  | CompileError of string
  | RuntimeError of string
  | SemanticError of string

open Frontend
open Backend
open Debug

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

(* Parse source code into AST *)
let parse (s : string) : (Common.Ast.stmt, compiler_error) result =
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

(* Perform semantic analysis on AST *)
let analyze_semantics ast =
  match Semantics.analyze_program ast with
  | Ok annotated_ast -> Ok annotated_ast
  | Error e -> Error (SemanticError (Semantics.error_to_string e))

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
      match analyze_semantics ast with
      | Error e -> Error e
      | Ok annotated_ast ->
          Printf.printf "Semantic analysis successful!\n";
          match compile_to target output_name annotated_ast with
          | Error e -> Error e
          | Ok executable ->
              if target = Executable then
                match run executable with
                | Ok _exit_code -> Ok executable
                | Error e -> Error e
              else
                Ok executable 