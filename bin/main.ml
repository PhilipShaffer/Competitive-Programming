open Base
open Stdio
open Libraries
module StdHashtbl = Stdlib.Hashtbl
open Parse_error

(* Include Llvm *) 
open Llvm

(* Assume your parser, lexer, and semantic modules are exposed as follows: *)
(* val Parser.main : (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Ast.stmt *)
(* val Lexer.read : Lexing.lexbuf -> Parser.token *)
(* val Semant.analyze_stmt : Semant.symbol_table -> Ast.stmt -> Hir.hir_stmt *)

(* Minimal pretty-printer for HIR - extend as needed *)
let rec pp_hir_stmt (stmt : Hir.hir_stmt) : string =
  match stmt with
  | Hir.HAssign (sym, expr) -> Printf.sprintf "HAssign(%d, %s)" sym (pp_hir_expr expr)
  | Hir.HArrayAssign (arr, idx, value, checked) -> Printf.sprintf "HArrayAssign(%s, %s, %s, %b)" (pp_hir_expr arr) (pp_hir_expr idx) (pp_hir_expr value) checked
  | Hir.HArrayPut (arr, value) -> Printf.sprintf "HPut(%s, %s)" (pp_hir_expr arr) (pp_hir_expr value)
  | Hir.HArrayPop arr -> Printf.sprintf "HPop(%s)" (pp_hir_expr arr)
  | Hir.HDeclare (sym, ty, expr) -> Printf.sprintf "HDeclare(%d, %s, %s)" sym (pp_ty ty) (pp_hir_expr expr)
  | Hir.HIf (cond, t, f) -> Printf.sprintf "HIf(%s, %s, %s)" (pp_hir_expr cond) (pp_hir_stmt t) (pp_hir_stmt f)
  | Hir.HWhile (cond, body) -> Printf.sprintf "HWhile(%s, %s)" (pp_hir_expr cond) (pp_hir_stmt body)
  | Hir.HPrint expr -> Printf.sprintf "HPrint(%s)" (pp_hir_expr expr)
  | Hir.HBlock sl -> Printf.sprintf "HBlock([%s])" (String.concat ~sep:"; " (List.map ~f:pp_hir_stmt sl))
  | Hir.HFunDecl (sym, params, ret, body) ->
      let params_str = String.concat ~sep:", " (List.map ~f:(fun (s, t) -> Printf.sprintf "%d:%s" s (pp_ty t)) params) in
      Printf.sprintf "HFunDecl(%d, [%s], %s, %s)" sym params_str (pp_ty ret) (pp_hir_stmt body)
  | Hir.HReturn expr -> Printf.sprintf "HReturn(%s)" (pp_hir_expr expr)

and pp_hir_expr (expr : Hir.hir_expr) : string =
  match expr with
  | Hir.HVar (sym, ty) -> Printf.sprintf "HVar(%d:%s)" sym (pp_ty ty)
  | Hir.HInt i -> Printf.sprintf "HInt(%d)" i
  | Hir.HString s -> Printf.sprintf "HString(\"%s\")" s
  | Hir.HFloat f -> Printf.sprintf "HFloat(%f)" f
  | Hir.HBool b -> Printf.sprintf "HBool(%b)" b
  | Hir.HBinop (op, e1, e2, ty) -> Printf.sprintf "HBinop(%s, %s, %s, %s)" (pp_bop op) (pp_hir_expr e1) (pp_hir_expr e2) (pp_ty ty)
  | Hir.HUnop (op, e, ty) -> Printf.sprintf "HUnop(%s, %s, %s)" (pp_uop op) (pp_hir_expr e) (pp_ty ty)
  | Hir.HFunCall (sym, args, ty) ->
      let args_str = String.concat ~sep:", " (List.map ~f:pp_hir_expr args) in
      Printf.sprintf "HFunCall(%d, [%s], %s)" sym args_str (pp_ty ty)
  | Hir.HArrayLit (elems, ty) ->
      let elems_str = String.concat ~sep:", " (List.map ~f:pp_hir_expr elems) in
      Printf.sprintf "HArrayLit([%s], %s)" elems_str (pp_ty ty)
  | Hir.HArrayGet (arr, idx, ty, checked) ->
      Printf.sprintf "HArrayGet(%s, %s, %s, %b)" (pp_hir_expr arr) (pp_hir_expr idx) (pp_ty ty) checked
  | Hir.HArrayLen arr ->
      Printf.sprintf "HArrayLen(%s)" (pp_hir_expr arr)
  | Hir.HCastInt (e, ty) -> Printf.sprintf "HCastInt(%s, %s)" (pp_hir_expr e) (pp_ty ty)
  | Hir.HCastFloat (e, ty) -> Printf.sprintf "HCastFloat(%s, %s)" (pp_hir_expr e) (pp_ty ty)
  | Hir.HCastString (e, ty) -> Printf.sprintf "HCastString(%s, %s)" (pp_hir_expr e) (pp_ty ty)

and pp_ty (ty : Ast.value_type) : string =
  match ty with
  | Ast.IntType -> "int"
  | Ast.FloatType -> "float"
  | Ast.StringType -> "string"
  | Ast.BoolType -> "bool"
  | Ast.VoidType -> "void"
  | Ast.ArrayType elem_ty -> Printf.sprintf "%s[]" (pp_ty elem_ty)

and pp_bop (op : Ast.bop) : string =
  match op with
  | Ast.Add -> "+"
  | Ast.Sub -> "-"
  | Ast.Mult -> "*"
  | Ast.Div -> "/"
  | Ast.Mod -> "%"
  | Ast.Lt -> "<"
  | Ast.Leq -> "<="
  | Ast.Gt -> ">"
  | Ast.Geq -> ">="
  | Ast.Eq -> "=="
  | Ast.Neq -> "!="
  | Ast.And -> "and"
  | Ast.Or -> "or"

and pp_uop (op : Ast.uop) : string =
  match op with
  | Ast.Neg -> "-"
  | Ast.Not -> "not"

(* Helper function to show error context *)
let show_error_context source lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  let line_num = pos.Lexing.pos_lnum in
  let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
  let lines = String.split_lines source in
  let error_line = 
    if line_num > 0 && line_num <= List.length lines then
      List.nth_exn lines (line_num - 1)
    else
      "<line not found>"
  in
  let pointer = String.make (col - 1) ' ' ^ "^" in
  Printf.sprintf "\n  %d | %s\n      %s\n" line_num error_line pointer

(* Helper function to show error context with position range *)
let show_error_context_range source start_pos end_pos =
  let line_num = start_pos.Lexing.pos_lnum in
  let start_col = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol + 1 in
  let end_col = end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol + 1 in
  let lines = String.split_lines source in
  let error_line = 
    if line_num > 0 && line_num <= List.length lines then
      List.nth_exn lines (line_num - 1)
    else
      "<line not found>"
  in
  let pointer = 
    String.make (start_col - 1) ' ' ^ 
    String.make (Int.max 1 (end_col - start_col)) '~' in
  Printf.sprintf "\n  %d | %s\n      %s\n" line_num error_line pointer

let () =
  let usage_msg = "Usage: main.exe <source-file>" in
  let filename =
    match Sys.get_argv () with
    | [| _; file |] -> file
    | _ -> prerr_endline usage_msg; Stdlib.exit 1
  in
  let source = In_channel.read_all filename in
  let lexbuf = Lexing.from_string source in
  (* Set filename for better error messages *)
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    let ast = Parser.main Lexer.read lexbuf in
    let hir = Semant.analyze_stmt [StdHashtbl.create 32] ast in
    Stdlib.print_endline "HIR generated successfully!";
    Stdlib.print_endline (pp_hir_stmt hir);

    (* Generate LLVM IR *) 
    Stdlib.print_endline "\nGenerating LLVM IR...";
    let llvm_module = Codegen.codegen_hir hir in

    (* Print LLVM IR to file *) 
    let output_filename = "output.ll" in
    Stdlib.print_endline ("\nWriting LLVM IR to " ^ output_filename ^ "...");
    let () = print_module output_filename llvm_module in
    Stdlib.print_endline "LLVM IR written successfully.";

    (* Re-enable Module verification *) 
    (match Llvm_analysis.verify_module llvm_module with
     | None -> Stdlib.print_endline "\nLLVM module verified successfully."
     | Some err -> Stdlib.print_endline ("\nLLVM module verification failed: " ^ err));

  with
  | ParseError (msg, start_pos, end_pos) ->
      let line = start_pos.Lexing.pos_lnum in
      let col = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol + 1 in
      let context = show_error_context_range source start_pos end_pos in
      prerr_endline (Printf.sprintf "Parse error at line %d, column %d: %s%s" 
                      line col msg context);
      Stdlib.exit 3
  | Semant.Semantic_error msg ->
      prerr_endline ("Semantic error: " ^ msg); Stdlib.exit 2
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      let line = pos.pos_lnum in
      let col = pos.pos_cnum - pos.pos_bol + 1 in
      let token = Lexing.lexeme lexbuf in
      let context = show_error_context source lexbuf in
      prerr_endline (Printf.sprintf "Parse error at line %d, column %d: unexpected token '%s'%s" 
                      line col token context);
      (* Try to provide helpful suggestions based on common errors *)
      (match token with
      | "flot" -> prerr_endline "  Did you mean 'float'?"
      | "in" | "int" when col > 1 -> 
          let prev_chars = String.sub source ~pos:(lexbuf.lex_curr_p.pos_cnum - col - 2) ~len:2 in
          if String.equal prev_chars ": " then
            prerr_endline "  Did you mean 'int' (integer type)?"
      | "=" -> prerr_endline "  Did you mean ':=' for assignment or '=' for equality comparison?"
      | _ -> ());
      Stdlib.exit 3
  | Failure msg when String.is_prefix msg ~prefix:"Codegen not implemented" ->
      prerr_endline ("Codegen Error: " ^ msg); Stdlib.exit 5 (* Specific exit code for codegen errors *)
  | Failure msg ->
      prerr_endline ("Failure: " ^ msg); Stdlib.exit 4
