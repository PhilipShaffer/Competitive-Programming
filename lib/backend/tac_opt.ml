(* lib/backend/tac_opt.ml *)

open Common.Tac
(* Removed 'open Common.Ast' to avoid name collisions with Tac constructors *)

(* Helper modules for string maps and sets *)
module SMap = Map.Make(String)
module SSet = Set.Make(String)
module LabelMap = Map.Make(String) (* Map where keys are block labels *)

(* --- Helper Functions for Variable Use/Def Analysis --- *)

(* Get the set of variables used by an operand *)
let vars_used_in_operand (op: operand) : SSet.t =
  match op with
  | Var v -> SSet.singleton v
  | Const _ -> SSet.empty

(* Get the set of variables used by an instruction *)
let vars_used (instr: instruction) : SSet.t =
  match instr with
  | Assign (_, src) -> vars_used_in_operand src
  | BinOp (_, op1, _, op2) -> SSet.union (vars_used_in_operand op1) (vars_used_in_operand op2)
  | CondJump (cond, _, _) -> vars_used_in_operand cond
  | Call (_, _, args) -> List.fold_left SSet.union SSet.empty (List.map vars_used_in_operand args)
  | Return op_opt -> Option.fold ~none:SSet.empty ~some:vars_used_in_operand op_opt
  | Label _ | Jump _ -> SSet.empty

(* Get the variable defined by an instruction, if any *)
let var_defined (instr: instruction) : string option =
  match instr with
  | Assign (dest, _) -> Some dest
  | BinOp (dest, _, _, _) -> Some dest
  | Call (Some dest, _, _) -> Some dest
  | Call (None, _, _) -> None
  | Label _ | Jump _ | CondJump _ | Return _ -> None

(* Check if an instruction has side effects (conservative check) *)
let has_side_effects (instr: instruction) : bool =
  match instr with
  | Call _ -> true (* Function calls might have side effects *)
  (* Consider BinOp with Div/Mod as potentially having side effects (division by zero) *)
  | BinOp (_, _, Div, _) | BinOp (_, _, Mod, _) -> true
  | _ -> false
(* --- Constant Folding --- *)

(* Evaluate a binary operation on two constants *)
(* Explicitly qualify bop from Common.Ast *)
let eval_binop (op: Common.Ast.bop) (v1: int) (v2: int) : operand option =
  match op with
  | Common.Ast.Add  -> Some (Const (v1 + v2))
  | Common.Ast.Sub  -> Some (Const (v1 - v2))
  | Common.Ast.Mult -> Some (Const (v1 * v2))
  | Common.Ast.Div  -> if v2 != 0 then Some (Const (v1 / v2)) else None
  | Common.Ast.Mod  -> if v2 != 0 then Some (Const (v1 mod v2)) else None
  | Common.Ast.Lt   -> Some (Const (if v1 < v2 then 1 else 0))
  | Common.Ast.Leq  -> Some (Const (if v1 <= v2 then 1 else 0))
  | Common.Ast.Gt   -> Some (Const (if v1 > v2 then 1 else 0))
  | Common.Ast.Geq  -> Some (Const (if v1 >= v2 then 1 else 0))
  | Common.Ast.Eq   -> Some (Const (if v1 = v2 then 1 else 0))
  | Common.Ast.Neq  -> Some (Const (if v1 != v2 then 1 else 0))
  | Common.Ast.And  -> Some (Const (if v1 != 0 && v2 != 0 then 1 else 0))
  | Common.Ast.Or   -> Some (Const (if v1 != 0 || v2 != 0 then 1 else 0))

(* Apply constant folding to a single instruction *)
let fold_instruction (instr: instruction) : instruction =
  match instr with
  | BinOp (dest, Const v1, op, Const v2) ->
      (match eval_binop op v1 v2 with
       | Some (result_operand : operand) -> Assign (dest, result_operand)
       | None -> instr
      )
  | _ -> instr

(* Apply constant folding to a list of instructions *)
let constant_folding_pass (instrs: instruction list) : instruction list =
  List.map fold_instruction instrs

(* --- Copy Propagation --- *)

(* Substitute an operand based on the current copy map *)
let substitute_operand (copy_map: operand SMap.t) (op: operand) : operand =
  match op with
  | Var v -> SMap.find_opt v copy_map |> Option.value ~default:op
  | Const _ -> op

(* Remove mappings that involve the defined variable 'dest' *)
let update_copy_map_on_def (copy_map: operand SMap.t) (dest: string) : operand SMap.t =
  let map_without_dest = SMap.remove dest copy_map in
  SMap.filter (fun _ (src_op: operand) -> src_op <> Var dest) map_without_dest

(* Apply copy propagation within a list of instructions (intra-block),
   using reaching constants information for initialization. *)
let copy_propagation_pass (instrs: instruction list) (const_in: int SMap.t) : instruction list =
  (* Initialize the map with known constants at block entry, converting int to Const operand *)
  let initial_map : operand SMap.t = SMap.map (fun c -> Const c) const_in in

  let rec propagate (acc_instrs: instruction list) (current_map: operand SMap.t) (remaining_instrs: instruction list) : instruction list =
    match remaining_instrs with
    | [] -> List.rev acc_instrs
    | (instr : instruction) :: rest ->
        let (new_instr : instruction), (next_map : operand SMap.t) =
          match instr with
          | Assign (dest, src) ->
              let (subst_src : operand) = substitute_operand current_map src in
              let updated_map = update_copy_map_on_def current_map dest in
              let final_map =
                match subst_src with
                | Var v when v = dest -> updated_map
                | (_ : operand) -> SMap.add dest subst_src updated_map
              in
              (Assign (dest, subst_src), final_map)
          | BinOp (dest, op1, bop, op2) ->
              let (subst_op1 : operand) = substitute_operand current_map op1 in
              let (subst_op2 : operand) = substitute_operand current_map op2 in
              let final_map = update_copy_map_on_def current_map dest in
              (BinOp (dest, subst_op1, bop, subst_op2), final_map)
          | CondJump (cond, l1, l2) ->
              let (subst_cond : operand) = substitute_operand current_map cond in
              (CondJump (subst_cond, l1, l2), current_map)
          | Call (dest_opt, fname, args) ->
              let (subst_args : operand list) = List.map (substitute_operand current_map) args in
              let final_map =
                match dest_opt with
                | Some dest -> update_copy_map_on_def current_map dest
                | None -> current_map
              in
              (Call (dest_opt, fname, subst_args), final_map)
          | Return op_opt ->
              let (subst_op_opt : operand option) = Option.map (substitute_operand current_map) op_opt in
              (Return subst_op_opt, current_map)
          | Label _ | Jump _ -> (instr, current_map)
        in
        propagate (new_instr :: acc_instrs) next_map rest
  in
  propagate [] initial_map instrs (* Start with the initial map derived from const_in *)

(* --- Dead Branch Elimination --- *)

(* Apply dead branch elimination to a list of instructions *)
let dead_branch_elimination_pass (instrs: instruction list) : instruction list =
  let rec eliminate (acc_instrs: instruction list) (remaining_instrs: instruction list) : instruction list =
    match remaining_instrs with
    | [] -> List.rev acc_instrs
    | (CondJump (Const c, then_lbl, else_lbl)) :: rest ->
        (* If condition is constant, replace with unconditional jump *)
        let new_instr =
          if c != 0 then (* True condition *)
            Jump then_lbl
          else (* False condition *)
            Jump else_lbl
        in
        eliminate (new_instr :: acc_instrs) rest
    | instr :: rest ->
        eliminate (instr :: acc_instrs) rest
  in
  eliminate [] instrs

(* --- Function Inlining --- *)

(* Unique suffix generation *)
let inline_counter = ref 0
let generate_unique_suffix () =
  inline_counter := !inline_counter + 1;
  "_inline" ^ string_of_int !inline_counter

(* Renaming logic *)
let rename_operand (suffix: string) (params: SSet.t) (operand: operand) : operand =
  match operand with
  | Var v ->
      (* Rename parameters and local temporaries (_t) *)
      if SSet.mem v params || String.starts_with ~prefix:"_t" v then
        Var (v ^ suffix)
      else
        operand (* Global variables or caller variables are not renamed *)
  | Const _ -> operand

let rename_instruction (suffix: string) (params: SSet.t) (instr: instruction) : instruction =
  let rename_op = rename_operand suffix params in
  let rename_var v = if SSet.mem v params || String.starts_with ~prefix:"_t" v then v ^ suffix else v in
  match instr with
  | Assign (dest, src) -> Assign (rename_var dest, rename_op src)
  | BinOp (dest, op1, bop, op2) -> BinOp (rename_var dest, rename_op op1, bop, rename_op op2)
  | Label l -> Label (l ^ suffix) (* Rename labels as well *)
  | Jump l -> Jump (l ^ suffix)
  | CondJump (cond, then_lbl, else_lbl) -> CondJump (rename_op cond, then_lbl ^ suffix, else_lbl ^ suffix)
  | Call (dest_opt, fname, args) ->
      let renamed_dest_opt = Option.map rename_var dest_opt in
      (* Don't rename the function name (fname) *)
      Call (renamed_dest_opt, fname, List.map rename_op args)
  | Return op_opt -> Return (Option.map rename_op op_opt)

(* Substitution logic *)
let substitute_params (param_arg_map: operand SMap.t) (instr: instruction) : instruction =
  (* Substitute operands within the instruction *)
  let subst_op (op: operand) : operand = (* Added type annotation *)
    match op with
    | Var v -> SMap.find_opt v param_arg_map |> Option.value ~default:op
    | Const _ -> op
  in
  match instr with
  | Assign (dest, src) -> Assign (dest, subst_op src) (* Don't substitute dest *)
  | BinOp (dest, op1, bop, op2) -> BinOp (dest, subst_op op1, bop, subst_op op2) (* Don't substitute dest *)
  | CondJump (cond, then_lbl, else_lbl) -> CondJump (subst_op cond, then_lbl, else_lbl)
  | Call (dest_opt, fname, args) -> Call (dest_opt, fname, List.map subst_op args) (* Don't substitute dest_opt *)
  | Return op_opt -> Return (Option.map subst_op op_opt)
  | Label _ | Jump _ -> instr (* No operands to substitute *)

(* Simple inlining heuristics *)
let should_inline (caller_name: string) (callee: tac_function) : bool =
  (* Add explicit type annotation for 'block' in fold_left *)
  let instruction_count = List.fold_left (fun acc (block : basic_block) -> acc + List.length block.instrs) 0 callee.blocks in
  let is_recursive = caller_name = callee.name in
  let max_instructions = 15 (* Configurable heuristic *) in
  (not is_recursive) && (instruction_count <= max_instructions)

(* Main function inlining pass *)
let inline_functions (prog: tac_program) : tac_program =
  let func_map = List.fold_left (fun map f -> SMap.add f.name f map) SMap.empty prog.functions in

  let inline_in_function (func: tac_function) : tac_function =
    let new_label_gen () : label = generate_unique_suffix () in (* Returns string label *)

    (* Removed 'rec' keyword as it's not recursive *)
    let inline_in_block (block: basic_block) : basic_block list =
      (* Process instructions, accumulating instructions before a potential call *)
      let rec process_instrs (instrs_before_call: instruction list) (remaining_instrs: instruction list) : basic_block list = (* Explicit return type *)
        match remaining_instrs with
        | [] -> (* No call found in this block or reached end *)
            (* Return the original block (or modified if prior inlining happened) *)
            let result : basic_block list = [{ block with instrs = List.rev instrs_before_call }] in (* Explicit type *)
            result
        | (Call (dest_opt, fname, args)) :: instrs_after_call ->
            (match SMap.find_opt fname func_map with
             | Some callee when should_inline func.name callee ->
                 Printf.printf "INFO: Inlining call to '%s' in function '%s' (block %s)\n" fname func.name block.label;
                 (* --- Inlining Steps --- *)
                 let suffix = generate_unique_suffix () in
                 let callee_params_set = SSet.of_list callee.params in

                 (* 1. Rename callee body (labels, params, temps) *)
                 (* Corrected: Construct record directly *)
                 let renamed_callee_blocks : basic_block list = List.map (fun blk ->
                   { label = blk.label ^ suffix;
                     instrs = List.map (rename_instruction suffix callee_params_set) blk.instrs
                   }) callee.blocks in

                 (* 2. Create Param -> Arg Map (using RENAMED param names) *)
                 let param_arg_map =
                   try List.fold_left2 (fun map param arg -> SMap.add (param ^ suffix) arg map) SMap.empty callee.params args
                   with Invalid_argument _ -> failwith ("Arity mismatch in call to " ^ fname ^ " in " ^ func.name)
                 in

                 (* 3. Substitute Params in Renamed Body *)
                 (* Corrected: Construct record directly *)
                 let substituted_blocks : basic_block list = List.map (fun blk ->
                     { label = blk.label; (* Keep renamed label *)
                       instrs = List.map (substitute_params param_arg_map) blk.instrs }
                   ) (renamed_callee_blocks : basic_block list) in

                 (* 4. Handle Returns & Create Continuation *)
                 let continuation_label = new_label_gen () in
                 (* Corrected: Construct record directly *)
                 let inlined_blocks_processed : basic_block list = List.map (fun blk ->
                     let map_func = (fun (instr: instruction) : instruction list ->
                       match instr with
                       | Return (Some ret_op) ->
                           (match dest_opt with
                            | Some dest -> [Assign (dest, ret_op); Jump continuation_label]
                            | None      -> [Jump continuation_label]
                           )
                       | Return None -> [Jump continuation_label]
                       | other_instr -> [other_instr]
                     ) in
                     let new_instrs : instruction list = List.concat_map map_func blk.instrs in
                     { label = blk.label; instrs = new_instrs } (* Construct directly *)
                   ) (substituted_blocks : basic_block list) in

                 (* 5. Stitch blocks together *)
                 let entry_label_of_inlined = match renamed_callee_blocks with
                   | [] -> continuation_label
                   | first_inlined_block :: _ -> first_inlined_block.label
                 in

                 let call_site_block : basic_block = {
                   label = block.label;
                   instrs = List.rev instrs_before_call @ [Jump entry_label_of_inlined]
                 } in

                 let continuation_block_opt : basic_block option =
                   if instrs_after_call <> [] then
                     Some { label = continuation_label; instrs = instrs_after_call }
                   else
                     None
                 in

                 let result_blocks : basic_block list =
                   call_site_block :: inlined_blocks_processed @ (Option.to_list continuation_block_opt)
                 in
                 result_blocks

             | _ ->
                 process_instrs (Call(dest_opt, fname, args) :: instrs_before_call) instrs_after_call
            )
        | instr :: rest ->
            process_instrs (instr :: instrs_before_call) rest
      in
      process_instrs [] block.instrs
    in
    { func with blocks = List.concat_map inline_in_block func.blocks }
  in
  { functions = List.map inline_in_function prog.functions }


(* --- Data Structures for Liveness Analysis --- *)
(* Moved CFG types here *)
type cfg_node = {
  _label: label; (* Prefixed as it's mainly used as a map key *)
  instrs: instruction list;
  predecessors: label list; (* Made immutable *)
  successors: label list;   (* Made immutable *)
  mutable use_set: SSet.t; (* Variables used before definition in this block *)
  mutable def_set: SSet.t; (* Variables defined in this block *)
  mutable const_in: int SMap.t; (* Constants known at block entry *)
  mutable const_out: int SMap.t; (* Constants known at block exit *)
}

(* Control Flow Graph: Map from label to cfg_node *)
type cfg = cfg_node LabelMap.t

(* Maps from block labels to sets of live variables *)
type live_map = SSet.t LabelMap.t

(* --- Control Flow Graph Construction --- *)
(* Moved build_cfg here *)
let build_cfg (blocks: basic_block list) : cfg =
  (* 1. Create initial nodes map (without links yet) *)
  let initial_nodes =
    List.fold_left (fun map block ->
      let node = {
        _label = block.label;
        instrs = block.instrs;
        predecessors = []; (* Initialize empty *)
        successors = [];   (* Initialize empty *)
        use_set = SSet.empty;
        def_set = SSet.empty;
        const_in = SMap.empty; (* Initialize empty *)
        const_out = SMap.empty; (* Initialize empty *)
      } in
      LabelMap.add block.label node map
    ) LabelMap.empty blocks
  in

  (* 2. Calculate successor lists for all nodes *)
  let successor_map =
    List.fold_left (fun map block ->
      let next_block_opt =
        let current_index_opt = List.find_index (fun b -> b.label = block.label) blocks in
        match current_index_opt with
        | Some i -> List.nth_opt blocks (i + 1)
        | None -> None
      in
      let successors =
        match List.rev block.instrs with
        | (Jump lbl) :: _ -> [lbl]
        | (CondJump (_, then_lbl, else_lbl)) :: _ -> [then_lbl; else_lbl]
        | (Return _) :: _ -> []
        | _ -> (match next_block_opt with Some nb -> [nb.label] | None -> [])
      in
      LabelMap.add block.label successors map
    ) LabelMap.empty blocks
  in

  (* 3. Calculate predecessor lists based on successor map *)
  let predecessor_map = ref LabelMap.empty in
  LabelMap.iter (fun label successors ->
    List.iter (fun succ_label ->
      let current_preds = LabelMap.find_opt succ_label !predecessor_map |> Option.value ~default:[] in
      predecessor_map := LabelMap.add succ_label (label :: current_preds) !predecessor_map
    ) successors
  ) successor_map;

  (* 4. Create final CFG map with links *)
  LabelMap.mapi (fun label node ->
    let successors = LabelMap.find_opt label successor_map |> Option.value ~default:[] in
    let predecessors = LabelMap.find_opt label !predecessor_map |> Option.value ~default:[] in
    { node with successors = successors; predecessors = predecessors }
  ) initial_nodes

(* --- Unreachable Code Elimination --- *)
(* Moved find_reachable_blocks and unreachable_code_elimination_pass here *)
let find_reachable_blocks (cfg: cfg) (start_label: label) : SSet.t =
  let visited = ref SSet.empty in
  let rec visit label =
    if not (SSet.mem label !visited) then begin
      visited := SSet.add label !visited;
      match LabelMap.find_opt label cfg with
      | Some node -> List.iter visit node.successors
      | None -> () (* Should not happen if CFG is well-formed *)
    end
  in
  visit start_label;
  !visited

let unreachable_code_elimination_pass (func: tac_function) : tac_function =
  (* Build CFG for the function *)
  let cfg = build_cfg func.blocks in

  (* Find all reachable blocks from the entry point (first block) *)
  let entry_label = match func.blocks with
    | [] -> "" (* Should not happen for valid functions *)
    | first_block :: _ -> first_block.label
  in
  let reachable_labels = find_reachable_blocks cfg entry_label in

  (* Filter blocks to keep only reachable ones *)
  let reachable_blocks = List.filter (fun block ->
    SSet.mem block.label reachable_labels
  ) func.blocks in

  { func with blocks = reachable_blocks }

(* --- Liveness Analysis --- *)
(* Moved compute_block_use_def and analyze_liveness here *)
let compute_block_use_def (instrs: instruction list) : (SSet.t * SSet.t) =
  let defined_in_block = ref SSet.empty in
  let used_in_block = ref SSet.empty in

  List.iter (fun instr ->
    (* Find variables used in this instruction *)
    let current_used = vars_used instr in
    (* Add used variables to block's use set ONLY if not already defined in the block *)
    SSet.iter (fun v ->
      if not (SSet.mem v !defined_in_block) then
        used_in_block := SSet.add v !used_in_block
    ) current_used;

    (* Find variable defined by this instruction *)
    match var_defined instr with
    | Some v -> defined_in_block := SSet.add v !defined_in_block
    | None -> ()
  ) instrs;

  (!used_in_block, !defined_in_block)

let analyze_liveness (cfg: cfg) : (live_map * live_map) =
  let live_in_map = ref (LabelMap.map (fun _ -> SSet.empty) cfg) in
  let live_out_map = ref (LabelMap.map (fun _ -> SSet.empty) cfg) in
  let worklist = ref (LabelMap.fold (fun label _ acc -> label :: acc) cfg []) in (* Initial worklist with all labels *)

  (* Helper to get live_in set, defaulting to empty *)
  let get_live_in label =
    LabelMap.find_opt label !live_in_map |> Option.value ~default:SSet.empty
  in

  while !worklist <> [] do
    let label = List.hd !worklist in
    worklist := List.tl !worklist;

    match LabelMap.find_opt label cfg with
    | None -> (* Should not happen if CFG is well-formed *)
        Printf.eprintf "Warning: Liveness analysis encountered label '%s' not in CFG.\n" label
    | Some node_b ->
        (* 1. Calculate new live_out[B] = Union(live_in[S] for S in Successors(B)) *)
        let new_live_out =
          List.fold_left (fun acc succ_label ->
            SSet.union acc (get_live_in succ_label)
          ) SSet.empty node_b.successors
        in
        live_out_map := LabelMap.add label new_live_out !live_out_map; (* Update live_out *)

        (* 2. Calculate new live_in[B] = use[B] U (live_out[B] - def[B]) *)
        let use_b = node_b.use_set in
        let def_b = node_b.def_set in
        let live_out_minus_def = SSet.diff new_live_out def_b in
        let new_live_in = SSet.union use_b live_out_minus_def in

        (* 3. Check if live_in[B] changed *)
        let old_live_in = get_live_in label in
        if not (SSet.equal new_live_in old_live_in) then begin
          live_in_map := LabelMap.add label new_live_in !live_in_map; (* Update live_in *)
          (* Add predecessors to worklist *)
          List.iter (fun pred_label ->
            if not (List.mem pred_label !worklist) then (* Avoid duplicates for efficiency *)
              worklist := pred_label :: !worklist
          ) node_b.predecessors
        end
  done;

  (!live_in_map, !live_out_map)

(* --- Dead Code Elimination (Using Liveness Information) --- *)
(* Moved dead_code_elimination_pass here *)
let dead_code_elimination_pass (instrs: instruction list) (live_out: SSet.t) : instruction list =
  let rec eliminate (remaining_instrs: instruction list) (live_vars: SSet.t) (acc_instrs: instruction list) : instruction list =
    match remaining_instrs with
    | [] -> acc_instrs (* Return accumulated instructions in original order *)
    | instr :: rest ->
        let defined = var_defined instr in
        let used = vars_used instr in
        let is_live =
          match defined with
          | Some v -> SSet.mem v live_vars
          | None -> true (* Instructions without definition are always considered live in this context *)
        in
        let is_dead = (not is_live) && (defined != None) && (not (has_side_effects instr)) in

        (* Update live set for the next (previous) instruction *)
        let next_live_vars =
          let live_after_def = match defined with
            | Some v -> SSet.remove v live_vars
            | None -> live_vars
          in
          SSet.union used live_after_def
        in

        (* Keep the instruction if it's not dead *)
        let new_acc_instrs = if is_dead then acc_instrs else instr :: acc_instrs in
        eliminate rest next_live_vars new_acc_instrs
  in
  (* Start the backward pass with the known live_out set for this block *)
  eliminate (List.rev instrs) live_out []


(* --- Reaching Constants Analysis --- *)

(* Merge function for constant maps: Intersect bindings *)
let merge_const_maps (map1: int SMap.t) (map2: int SMap.t) : int SMap.t =
  SMap.merge (fun _ val1_opt val2_opt ->
    match val1_opt, val2_opt with
    | Some v1, Some v2 when v1 = v2 -> Some v1 (* Keep if values match *)
    | _ -> None (* Discard if variable not in both or values differ *)
  ) map1 map2

(* Transfer function for a block: Update constant map based on instructions *)
let transfer_const_block (instrs: instruction list) (const_in: int SMap.t) : int SMap.t =
  List.fold_left (fun current_map instr ->
    match instr with (* Reverted: Use constructors directly due to 'open Common.Tac' *)
    | Assign (dest, Const c) -> SMap.add dest c current_map (* Define/redefine constant *)
    | Assign (dest, Var v) ->
        (match SMap.find_opt v current_map with
         | Some c -> SMap.add dest c current_map (* Propagate constant copy *)
         | None -> SMap.remove dest current_map (* Kill destination if source not constant *)
        )
    | BinOp (dest, Const c1, op, Const c2) ->
        (match eval_binop op c1 c2 with
         | Some (Const c_res) -> SMap.add dest c_res current_map (* Fold constant binop *)
         | _ -> SMap.remove dest current_map (* Kill destination if binop not foldable or division by zero *)
        )
    | BinOp (dest, Var v1, op, Var v2) ->
        (match SMap.find_opt v1 current_map, SMap.find_opt v2 current_map with
         | Some c1, Some c2 ->
             (match eval_binop op c1 c2 with
              | Some (Const c_res) -> SMap.add dest c_res current_map
              | _ -> SMap.remove dest current_map
             )
         | _ -> SMap.remove dest current_map (* Kill destination if operands not constant *)
        )
    | BinOp (dest, Var v, op, Const c) | BinOp (dest, Const c, op, Var v) ->
         (match SMap.find_opt v current_map with
          | Some c_v ->
              (* Determine order for non-commutative ops *)
              let c1, c2 = if match instr with BinOp(_, Const _, _, Var _) -> true | _ -> false then c, c_v else c_v, c in
              (match eval_binop op c1 c2 with
               | Some (Const c_res) -> SMap.add dest c_res current_map
               | _ -> SMap.remove dest current_map
              )
          | _ -> SMap.remove dest current_map (* Kill destination if var operand not constant *)
         )
    (* Any other instruction that defines a variable kills its constant status *)
    (* Removed redundant Assign(dest,_) and BinOp(dest,_,_,_) cases as they are covered by specific patterns above *)
    | Call (Some dest, _, _) -> SMap.remove dest current_map (* Call with dest kills dest *)
    (* Instructions without definitions or non-constant assignments don't affect the map in terms of adding constants,
       but might kill existing ones if they redefine a variable (handled above) *)
    | Label _ | Jump _ | CondJump _ | Return _ | Call (None, _, _) -> current_map
  ) const_in instrs

(* Analyze reaching constants using a forward dataflow analysis *)
let analyze_reaching_constants (cfg: cfg) : unit =
  let worklist = ref (LabelMap.fold (fun label _ acc -> label :: acc) cfg []) in (* Initial worklist with all labels *)
  let entry_label_opt = LabelMap.min_binding_opt cfg |> Option.map fst in (* Assuming first block is entry *)

  (* Initialize const_in and const_out for all nodes *)
   LabelMap.iter (fun _ node ->
       node.const_in <- SMap.empty;
       node.const_out <- SMap.empty;
   ) cfg;

  (* Set initial state for entry block if it exists *)
  (match entry_label_opt with
   | Some entry_label ->
       (match LabelMap.find_opt entry_label cfg with
        | Some entry_node ->
            entry_node.const_in <- SMap.empty; (* Entry IN is empty *)
            entry_node.const_out <- transfer_const_block entry_node.instrs entry_node.const_in; (* Compute initial OUT *)
            (* Add successors of entry to worklist initially *)
            worklist := entry_node.successors;
        | None -> worklist := [] (* Should not happen *)
       )
   | None -> worklist := [] (* No blocks in function *)
  );


  while !worklist <> [] do
    let label = List.hd !worklist in
    worklist := List.tl !worklist;

    match LabelMap.find_opt label cfg with
    | None -> Printf.eprintf "Warning: Reaching constants analysis encountered label '%s' not in CFG.\n" label
    | Some node ->
        (* 1. Calculate new const_in[B] = Merge(const_out[P] for P in Predecessors(B)) *)
        let merged_in =
          if Some label = entry_label_opt then
            SMap.empty (* Entry block's IN is always empty *)
          else
            match node.predecessors with
            | [] -> SMap.empty (* Should not happen for non-entry blocks *)
            | first_pred :: rest_preds ->
                (* Start with the OUT set of the first predecessor *)
                let initial_map =
                  match LabelMap.find_opt first_pred cfg with
                  | Some pred_node -> pred_node.const_out
                  | None -> SMap.empty (* Should not happen *)
                in
                (* Intersect (merge) with the OUT sets of the remaining predecessors *)
                List.fold_left (fun acc_map pred_label ->
                  match LabelMap.find_opt pred_label cfg with
                  | Some pred_node -> merge_const_maps acc_map pred_node.const_out
                  | None -> acc_map
                ) initial_map rest_preds
        in
        let old_const_in = node.const_in in

        (* 2. Check if const_in changed *)
        if not (SMap.equal (=) merged_in old_const_in) then begin
            node.const_in <- merged_in; (* Update const_in *)

            (* 3. Calculate new const_out[B] using transfer function *)
            let new_const_out = transfer_const_block node.instrs node.const_in in

            (* 4. Check if const_out changed *)
            if not (SMap.equal (=) new_const_out node.const_out) then begin
              node.const_out <- new_const_out; (* Update const_out *)

              (* 5. Add successors to worklist *)
              List.iter (fun succ_label ->
                if not (List.mem succ_label !worklist) then
                  worklist := succ_label :: !worklist
              ) node.successors
            end
        end
  done


(* Add these helper functions before optimize_tac *)

(* Build a map from function name to the set of functions it calls directly *)
let build_call_graph (functions: tac_function list) : SSet.t SMap.t =
  List.fold_left (fun graph func ->
    let callees = ref SSet.empty in
    List.iter (fun (block : basic_block) -> (* Explicit type annotation *)
      List.iter (fun instr ->
        match instr with
        | Call (_, fname, _) -> callees := SSet.add fname !callees
        | _ -> ()
      ) block.instrs
    ) func.blocks;
    SMap.add func.name !callees graph
  ) SMap.empty functions

(* Find all reachable functions starting from a set of entry points *)
let find_reachable_functions (call_graph: SSet.t SMap.t) (entry_points: SSet.t) : SSet.t =
  let reachable = ref entry_points in
  let worklist = ref (SSet.elements entry_points) in
  while !worklist <> [] do
    let caller = List.hd !worklist in
    worklist := List.tl !worklist;
    match SMap.find_opt caller call_graph with
    | Some callees ->
        SSet.iter (fun callee ->
          if not (SSet.mem callee !reachable) then begin
            reachable := SSet.add callee !reachable;
            worklist := callee :: !worklist
          end
        ) callees
    | None -> () (* Function might not call anything or might not be in the graph (e.g., external) *)
  done;
  !reachable

(* --- Function and Program Optimization --- *)

(* Inner optimization function (used multiple times) *)
(* Removed the erroneous 'let optimize_tac...' wrapper from here *)
let optimize_function_pass (func: tac_function) : tac_function =
    (* 1. Build CFG *)
    let cfg = build_cfg func.blocks in

    (* 2. Compute Use/Def for all blocks (populate cfg nodes) *)
    LabelMap.iter (fun _ node ->
      let use_def = compute_block_use_def node.instrs in
      node.use_set <- fst use_def;
      node.def_set <- snd use_def;
    ) cfg;

    (* 3. Analyze Liveness *)
    let _live_in_map, live_out_map = analyze_liveness cfg in

    (* 4. Analyze Reaching Constants *)
    analyze_reaching_constants cfg; (* Run the analysis *)

    (* 5. Optimize each block using liveness and constant info *)
    let optimize_block (block: basic_block) : basic_block =
      let block_label = block.label in
      let live_out = LabelMap.find_opt block_label live_out_map |> Option.value ~default:SSet.empty in

      (* Iterate optimization passes until a fixed point is reached *)
      let rec run_passes current_instrs block_const_in = (* Added block_const_in parameter *)
        let folded = constant_folding_pass current_instrs in
        (* Pass block_const_in to copy_propagation_pass *)
        let propagated = copy_propagation_pass folded block_const_in in (* Use block_const_in *)
        let dbe_optimized = dead_branch_elimination_pass propagated in
        let eliminated = dead_code_elimination_pass dbe_optimized live_out in

        if eliminated = current_instrs then
          current_instrs (* Fixed point reached *)
        else
          run_passes eliminated block_const_in (* Use block_const_in in recursive call *)
      in

      (* Retrieve const_in for the block *)
      let const_in =
         match LabelMap.find_opt block_label cfg with
         | Some node -> node.const_in
         | None -> SMap.empty (* Should not happen *)
      in
      let final_instrs = run_passes block.instrs const_in in (* Pass const_in for initial call *)

      { block with instrs = final_instrs }
    in

    (* Apply optimization to all blocks *)
    let optimized_func = { func with blocks = List.map optimize_block func.blocks } in

    (* 5. Remove unreachable blocks *)
  unreachable_code_elimination_pass optimized_func

(* Note: optimize_function_pass ends here *)

(* Optimize each function in the program - Main entry point *)
let optimize_tac (prog: tac_program) : tac_program =

  (* --- Main Optimization Pipeline --- *)

  (* 1. Initial Optimization Pass (before inlining) *)
  let optimized_once_prog = { functions = List.map optimize_function_pass prog.functions } in

  (* 2. Function Inlining *)
  let inlined_prog = inline_functions optimized_once_prog in

  (* 3. Second Optimization Pass (after inlining) *)
  let optimized_twice_prog = { functions = List.map optimize_function_pass inlined_prog.functions } in

  (* 4. Remove Unused Functions *)
  let call_graph = build_call_graph optimized_twice_prog.functions in
  let entry_points = SSet.singleton "_main" in (* Assuming _main is the entry point *)
  let reachable_function_names = find_reachable_functions call_graph entry_points in
  let final_functions = List.filter (fun f -> SSet.mem f.name reachable_function_names) optimized_twice_prog.functions in

  { functions = final_functions }