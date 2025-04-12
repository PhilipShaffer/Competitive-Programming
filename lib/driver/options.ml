open Compiler

type t = {
  filename : string;
  output_name : string;
  show_tokens : bool;
  target : target;
}

let usage = "Usage: compiler [options] file.src"

let parse () =
  let filename = ref "" in
  let output_name = ref "program" in
  let show_tokens = ref false in
  let target_type = ref Executable in
  
  let set_target s =
    match String.lowercase_ascii s with
    | "ir" | "llvm" | "llvm-ir" -> target_type := LLVM_IR
    | "asm" | "assembly" -> target_type := Assembly
    | "exe" | "executable" -> target_type := Executable
    | _ -> raise (Arg.Bad "Target must be one of: llvm-ir, assembly, executable")
  in
  
  let spec = [
    ("-o", Arg.Set_string output_name, " Output file name (default: program)");
    ("--show-tokens", Arg.Set show_tokens, " Show tokens (default: false)");
    ("--target", Arg.String set_target, " Target type: llvm-ir, assembly, executable (default: executable)");
  ] in
  
  Arg.parse spec (fun s -> filename := s) usage;
  
  if !filename = "" then begin
    Printf.eprintf "Error: No input file specified\n";
    Arg.usage spec usage;
    exit 1
  end;
  
  {
    filename = !filename;
    output_name = !output_name;
    show_tokens = !show_tokens;
    target = !target_type;
  } 