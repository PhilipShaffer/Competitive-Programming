(** Compiler target types *)
type target = LLVM_IR | Assembly | Executable

(** Compiler error types *)
type compiler_error = 
  | ParseError of string
  | CompileError of string
  | RuntimeError of string
  | SemanticError of string

(** Compile and run a program from a source file *)
val compile_and_run : ?show_tokens:bool -> ?target:target -> string -> string -> (string, compiler_error) result

(** Parse source code into AST *)
val parse : string -> (Common.Ast.stmt, compiler_error) result

(** Perform semantic analysis on AST *)
val analyze_semantics : Common.Ast.stmt -> (Common.Ast.stmt, compiler_error) result

(** Compile AST to LLVM module *)
val compile_ast : Common.Ast.stmt -> (Llvm.llmodule, compiler_error) result

(** Compile to different targets *)
val compile_to : target -> string -> Common.Ast.stmt -> (string, compiler_error) result

(** Run a compiled executable *)
val run : string -> (int, compiler_error) result 