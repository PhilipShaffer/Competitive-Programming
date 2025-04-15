(* lib/backend/tac_gen.ml *)

open Common.Ast
open Common.Tac

(* --- State Management --- *)
let label_counter = ref 0
let temp_counter = ref 0

let new_label () =
  label_counter := !label_counter + 1;
  Printf.sprintf "L%d" !label_counter

let new_temp () =
  temp_counter := !temp_counter + 1;
  Printf.sprintf "_t%d" !temp_counter

(* Context for generating code within a function *)
type gen_context = {
  mutable instructions: instruction list; (* Accumulator for instructions *)
  (* Add more context if needed: symbol table, current block label, etc. *)
}

(* Helper to add an instruction to the context (in reverse order) *)
let emit ctx instr =
  ctx.instructions <- instr :: ctx.instructions

(* --- Expression Translation --- *)
(* Translates an AST expression into a TAC operand (variable or constant)
   and generates instructions to compute it, adding them to the context.
   Returns the operand representing the result. *)
let rec translate_expr (ctx: gen_context) (expr: expr) : operand =
  match expr.expr with
  | Var v -> Var v (* Direct variable usage *)
  | Int i -> Const i (* Integer constant *)
  | String _ | Float _ | Bool _ ->
      (* Placeholder: Need specific handling or error *)
      Printf.eprintf "Warning: TAC generation for String/Float/Bool expressions not fully implemented.\n";
      Const 0 (* Defaulting to 0 for now *)
  | Binop (op, e1, e2) ->
      let op1 = translate_expr ctx e1 in
      let op2 = translate_expr ctx e2 in
      let temp_var = new_temp () in
      emit ctx (BinOp (temp_var, op1, op, op2));
      Var temp_var
  | Unop (op, e) ->
      let sub_operand = translate_expr ctx e in
      let temp_var = new_temp () in
      (match op with
       | Neg -> (* Represent as 0 - operand *)
           emit ctx (BinOp (temp_var, Const 0, Sub, sub_operand))
       | Not -> (* Represent as temp := 1; if operand == 0 goto set_true; temp := 0; set_true: ... *)
           (* This requires more complex control flow - simplified for now *)
           Printf.eprintf "Warning: TAC generation for 'Not' unary operator is simplified.\n";
           (* Simplified: treat 'not x' as 'x == 0' for boolean logic *)
           emit ctx (BinOp (temp_var, sub_operand, Eq, Const 0))
      );
      Var temp_var
  | FunCall (fname, args) ->
      let arg_operands = List.map (translate_expr ctx) args in
      let temp_var = new_temp () in
      (* Assume functions might return void, handle option *)
      emit ctx (Call (Some temp_var, fname, arg_operands));
      Var temp_var

(* --- Statement Translation --- *)
(* Translates an AST statement, adding instructions to the context *)
let rec translate_stmt (ctx: gen_context) (stmt: stmt) : unit =
  match stmt with
  | Assign (var_name, expr) ->
      let src_operand = translate_expr ctx expr in
      emit ctx (Assign (var_name, src_operand))
  | Declare (var_name, _, init_expr) ->
      (* Translate the initialization expression *)
      let src_operand = translate_expr ctx init_expr in
      (* Emit an assignment instruction *)
      emit ctx (Assign (var_name, src_operand))
  | If (cond_expr, then_stmt, else_stmt) ->
      let then_label = new_label () in
      let else_label = new_label () in
      let end_label = new_label () in

      let cond_operand = translate_expr ctx cond_expr in
      emit ctx (CondJump (cond_operand, then_label, else_label));

      (* Else branch first, as it follows the conditional jump *)
      emit ctx (Label else_label);
      translate_stmt ctx else_stmt;
      emit ctx (Jump end_label); (* Jump over the 'then' block *)

      (* Then branch *)
      emit ctx (Label then_label);
      translate_stmt ctx then_stmt;
      (* Fallthrough to end_label is implicit *)

      (* End label *)
      emit ctx (Label end_label)

  | While (cond_expr, body_stmt) ->
      let loop_start_label = new_label () in (* Condition check *)
      let loop_body_label = new_label () in  (* Body execution *)
      let loop_end_label = new_label () in   (* After the loop *)

      emit ctx (Label loop_start_label);
      let cond_operand = translate_expr ctx cond_expr in
      emit ctx (CondJump (cond_operand, loop_body_label, loop_end_label)); (* If true, jump to body, else exit *)

      emit ctx (Label loop_body_label);
      translate_stmt ctx body_stmt;
      emit ctx (Jump loop_start_label); (* Jump back to condition check *)

      emit ctx (Label loop_end_label)

  | Print expr ->
      (* Assuming a built-in function like `_print_int` or similar *)
      let operand = translate_expr ctx expr in
      (* Determine print function based on operand type if types were tracked *)
      let print_func = "_print_int" in (* Defaulting to int *)
      emit ctx (Call (None, print_func, [operand]))
  | Block stmts ->
      (* Process statements in order *)
      List.iter (translate_stmt ctx) stmts
  | Return expr ->
      let ret_operand = translate_expr ctx expr in
      emit ctx (Return (Some ret_operand))
  | FunDef _ ->
      (* Should be handled at the top level *)
      failwith "Nested function definitions encountered during statement translation."


(* --- Basic Block Construction --- *)
(* Naive approach: Turns a list of instructions into a list of basic blocks.
   Splits only occur at labels. Jumps/CondJumps don't end blocks here.
   A proper implementation requires CFG analysis. *)
let create_basic_blocks (instructions: instruction list) : basic_block list =
    let rec build_blocks acc current_block_label current_instrs remaining_instrs =
        match remaining_instrs with
        | [] -> (* End of instructions *)
            if List.length current_instrs > 0 then
                { label = current_block_label; instrs = List.rev current_instrs } :: acc
            else acc (* Avoid empty blocks *)
        | (Label new_label) :: rest ->
            let new_block =
                if List.length current_instrs > 0 then
                    { label = current_block_label; instrs = List.rev current_instrs }
                else (* Handle case where a label immediately follows another or starts the list *)
                    { label = current_block_label; instrs = [] } (* Or maybe skip empty blocks? *)
            in
            (* Start new block, discarding the label instruction itself from the list *)
            build_blocks (new_block :: acc) new_label [] rest
        | instr :: rest ->
            (* Add instruction to current block *)
            build_blocks acc current_block_label (instr :: current_instrs) rest
    in
    match instructions with
    | (Label start_label) :: rest -> List.rev (build_blocks [] start_label [] rest)
    | _ -> (* Should start with a label, add a default one if missing? *)
           Printf.eprintf "Warning: Function TAC doesn't start with a label. Adding default entry.\n";
           let entry_label = new_label () in (* Or use function name *)
           List.rev (build_blocks [] entry_label [] instructions)


(* --- Top-Level Generation --- *)
(* Helper function to translate a list of statements into TAC instructions *)
let translate_statements_to_instructions (stmts: stmt list) : instruction list =
  let ctx = { instructions = [] } in
  (* Add a starting label - necessary for block creation *)
  let entry_label = new_label () in (* Use a generic label or function name based label *)
  emit ctx (Label entry_label);

  List.iter (translate_stmt ctx) stmts;

  (* Ensure block ends with a return if one isn't present *)
   (match ctx.instructions with
    | (Return _) :: _ -> () (* Already ends with return *)
    | _ -> emit ctx (Return None) (* Add void return *)
   );

  List.rev ctx.instructions (* Return instructions in correct order *)

(* --- Top-Level Generation --- *)
let generate_tac (program_ast_list: stmt list) : tac_program =
  (* Reset counters *)
  label_counter := 0;
  temp_counter := 0;

  (* Extract statements from the top-level block if present *)
  let top_level_stmts =
    match program_ast_list with
    | [Block stmts] -> stmts (* Common case: program is a block *)
    | _ -> program_ast_list (* Handle list of top-level statements directly *)
  in

  (* Partition top-level statements into functions and main code *)
  let fun_defs, main_stmts =
    List.partition (function FunDef _ -> true | _ -> false) top_level_stmts
  in

  (* Generate TAC for explicit function definitions *)
  let explicit_functions = List.filter_map (function
    | FunDef { fname; params; return_type=_; body; loc=_ } ->
        let param_names = List.map fst params in
        (* Note: translate_stmt expects a single statement, FunDef body is often a Block *)
        let body_stmts = match body with Block stmts -> stmts | single_stmt -> [single_stmt] in
        let instructions = translate_statements_to_instructions body_stmts in
        let blocks = create_basic_blocks instructions in
        let tac_func = { name = fname; params = param_names; blocks = blocks } in
        Some tac_func
    | _ -> None (* Should not happen due to partition *)
  ) fun_defs in

  (* Generate TAC for the implicit main function if main_stmts is not empty *)
  let main_function_opt =
    if List.length main_stmts > 0 then
      let main_instructions = translate_statements_to_instructions main_stmts in
      let main_blocks = create_basic_blocks main_instructions in
      let main_tac_func = { name = "_main"; params = []; blocks = main_blocks } in
      Some main_tac_func
    else
      None
  in

  (* Combine main (if exists) and other functions *)
  let all_functions =
    match main_function_opt with
    | Some main_func -> main_func :: explicit_functions
    | None -> explicit_functions
  in

  { functions = all_functions }