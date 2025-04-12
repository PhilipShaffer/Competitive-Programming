open Driver

let () =
  let options = Options.parse () in
  
  match Compiler.compile_and_run 
    ~show_tokens:options.show_tokens 
    ~target:options.target 
    options.filename 
    options.output_name with
  | Ok output -> Printf.printf "Compilation successful! Output: %s\n" output
  | Error (Compiler.ParseError msg) -> 
      Printf.eprintf "Parse error: %s\n" msg;
      exit 1
  | Error (Compiler.SemanticError msg) ->
      Printf.eprintf "Semantic error: %s\n" msg;
      exit 1
  | Error (Compiler.CompileError msg) -> 
      Printf.eprintf "Compilation error: %s\n" msg;
      exit 1
  | Error (Compiler.RuntimeError msg) -> 
      Printf.eprintf "Runtime error: %s\n" msg;
      exit 1