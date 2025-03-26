(* Symbol table to track variable positions in memory *)
type symbol_table = (string * int) list

(* Generator state *)
type state = {
  mutable symbol_table : symbol_table;
  mutable stack_offset : int;
  mutable label_counter : int;
}

(* Initialize state *)
let init_state () = {
  symbol_table = [];
  stack_offset = 0;
  label_counter = 0;
}

(* Get a fresh label *)
let fresh_label state prefix =
  let label = prefix ^ string_of_int state.label_counter in
  state.label_counter <- state.label_counter + 1;
  label

(* Add a variable to the symbol table *)
let add_var state name =
  state.stack_offset <- state.stack_offset - 4;
  state.symbol_table <- (name, state.stack_offset) :: state.symbol_table;
  state.stack_offset

(* Look up a variable in the symbol table *)
let lookup_var state name =
  try List.assoc name state.symbol_table
  with Not_found -> failwith ("Variable " ^ name ^ " not found")

(* Generate code for expressions *)
let rec gen_expr state expr =
  match expr with
  | Ast.Var x ->
      let offset = lookup_var state x in
      Printf.sprintf "    lw $v0, %d($fp)\n" offset
  
  | Ast.Int n ->
      Printf.sprintf "    li $v0, %d\n" n
  
  | Ast.Bool b ->
      Printf.sprintf "    li $v0, %d\n" (if b then 1 else 0)
  
  | Ast.Binop (op, e1, e2) ->
      let code1 = gen_expr state e1 in
      let code2 = gen_expr state e2 in
      let save_e1 = "    sw $v0, -4($sp)\n    addiu $sp, $sp, -4\n" in
      let restore_e1 = "    lw $t1, 0($sp)\n    addiu $sp, $sp, 4\n" in
      let op_code = match op with
        | Ast.Add -> "    addu $v0, $t1, $v0\n"
        | Ast.Sub -> "    subu $v0, $t1, $v0\n"
        | Ast.Mult -> "    mul $v0, $t1, $v0\n"
        | Ast.Div -> "    div $t1, $v0\n    mflo $v0\n"
        | Ast.Lt -> "    slt $v0, $t1, $v0\n"
        | Ast.Leq -> "    sle $v0, $t1, $v0\n"
        | Ast.Gt -> "    sgt $v0, $t1, $v0\n"
        | Ast.Geq -> "    sge $v0, $t1, $v0\n"
        | Ast.Eq -> "    seq $v0, $t1, $v0\n"
        | Ast.Neq -> "    sne $v0, $t1, $v0\n"
        | Ast.And -> "    and $v0, $t1, $v0\n"
        | Ast.Or -> "    or $v0, $t1, $v0\n"
        | Ast.Mod -> "    div $t1, $v0\n    mfhi $v0\n"
      in
      code1 ^ save_e1 ^ code2 ^ restore_e1 ^ op_code
  
  | Ast.Unop (op, e) ->
      let code = gen_expr state e in
      let op_code = match op with
        | Ast.Neg -> "    neg $v0, $v0\n"
        | Ast.Not -> "    seq $v0, $v0, $zero\n"
      in
      code ^ op_code

(* Generate code for statements *)
let rec gen_stmt state stmt =
  match stmt with
  | Ast.Assign (x, e) ->
      let code = gen_expr state e in
      let offset = 
        try lookup_var state x
        with _ -> add_var state x
      in
      code ^ Printf.sprintf "    sw $v0, %d($fp)\n" offset
  
  | Ast.Let (x, e, s) ->
      let code_e = gen_expr state e in
      let offset = add_var state x in
      let save_var = Printf.sprintf "    sw $v0, %d($fp)\n" offset in
      let code_s = gen_stmt state s in
      code_e ^ save_var ^ code_s
  
  | Ast.If (e, s1, s2) ->
      let code_e = gen_expr state e in
      let label_else = fresh_label state "else_" in
      let label_end = fresh_label state "endif_" in
      let code_s1 = gen_stmt state s1 in
      let code_s2 = gen_stmt state s2 in
      code_e ^
      "    beq $v0, $zero, " ^ label_else ^ "\n" ^
      code_s1 ^
      "    j " ^ label_end ^ "\n" ^
      label_else ^ ":\n" ^
      code_s2 ^
      label_end ^ ":\n"
  
  | Ast.While (e, s) ->
      let label_start = fresh_label state "while_" in
      let label_end = fresh_label state "endwhile_" in
      let code_e = gen_expr state e in
      let code_s = gen_stmt state s in
      label_start ^ ":\n" ^
      code_e ^
      "    beq $v0, $zero, " ^ label_end ^ "\n" ^
      code_s ^
      "    j " ^ label_start ^ "\n" ^
      label_end ^ ":\n"
  
  | Ast.Print e ->
      let code = gen_expr state e in
      code ^
      "    move $a0, $v0\n" ^
      "    li $v0, 1\n" ^
      "    syscall\n" ^
      "    li $a0, '\\n'\n" ^
      "    li $v0, 11\n" ^
      "    syscall\n"
  
  | Ast.Block stmts ->
      List.fold_left (fun acc s -> acc ^ gen_stmt state s) "" stmts

(* Generate the full program *)
let generate_program stmt =
  let state = init_state () in
  let code = gen_stmt state stmt in
  let prologue = 
    ".data\n" ^
    ".text\n" ^
    ".globl main\n" ^
    "main:\n" ^
    "    move $fp, $sp\n" ^
    "    subu $sp, $sp, 128\n"  (* Reserve stack space *)
  in
  let epilogue =
    "    li $v0, 10\n" ^
    "    syscall\n"
  in
  prologue ^ code ^ epilogue

(* Main function to compile to MIPS *)
let compile_to_mips stmt output_file =
  let program = generate_program stmt in
  let oc = open_out output_file in
  output_string oc program;
  close_out oc 