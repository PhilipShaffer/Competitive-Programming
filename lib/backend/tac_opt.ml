(* lib/backend/tac_opt.ml *)

open Common.Tac
open Common.Ast (* Need Ast.bop *)

(* Helper modules for string maps and sets *)
module SMap = Map.Make(String)
module SSet = Set.Make(String)
module LabelMap = Map.Make(String) (* Map where keys are block labels *)

(* --- Data Structures for Liveness Analysis --- *)

(* Represents a node in the Control Flow Graph *)
type cfg_node = {
  _label: label; (* Prefixed as it's mainly used as a map key *)
  instrs: instruction list;
  predecessors: label list; (* Made immutable *)
  successors: label list;   (* Made immutable *)
  mutable use_set: SSet.t; (* Variables used before definition in this block *)
  mutable def_set: SSet.t; (* Variables defined in this block *)
}

(* Control Flow Graph: Map from label to cfg_node *)
type cfg = cfg_node LabelMap.t

(* Maps from block labels to sets of live variables *)
type live_map = SSet.t LabelMap.t

(* Map from block label to its (use, def) sets *)
(* Removed unused type alias block_use_def_map *)

(* --- Control Flow Graph Construction --- *)

(* Builds the Control Flow Graph for a function *)
let build_cfg (blocks: basic_block list) : cfg =
  (* 1. Create initial nodes map (without links yet) *)
  let initial_nodes =
    List.fold_left (fun map block ->
      let node = {
        _label = block.label;
        instrs = block.instrs;
        predecessors = []; (* Initialize empty *)
        successors = [];   (* Initialize empty *)
        use_set = SSet.empty; (* Will be computed later *)
        def_set = SSet.empty; (* Will be computed later *)
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
let eval_binop (op: Common.Ast.bop) (v1: int) (v2: int) : operand option =
  match op with
  | Add  -> Some (Const (v1 + v2))
  | Sub  -> Some (Const (v1 - v2))
  | Mult -> Some (Const (v1 * v2))
  | Div  -> if v2 != 0 then Some (Const (v1 / v2)) else None
  | Mod  -> if v2 != 0 then Some (Const (v1 mod v2)) else None
  | Lt   -> Some (Const (if v1 < v2 then 1 else 0))
  | Leq  -> Some (Const (if v1 <= v2 then 1 else 0))
  | Gt   -> Some (Const (if v1 > v2 then 1 else 0))
  | Geq  -> Some (Const (if v1 >= v2 then 1 else 0))
  | Eq   -> Some (Const (if v1 = v2 then 1 else 0))
  | Neq  -> Some (Const (if v1 != v2 then 1 else 0))
  | And  -> Some (Const (if v1 != 0 && v2 != 0 then 1 else 0))
  | Or   -> Some (Const (if v1 != 0 || v2 != 0 then 1 else 0))

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

(* Apply copy propagation within a list of instructions (intra-block) *)
let copy_propagation_pass (instrs: instruction list) : instruction list =
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
  propagate [] SMap.empty instrs

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

(* --- Unreachable Code Elimination --- *)

(* Find all reachable blocks from a given starting label *)
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

(* Remove unreachable blocks from a function *)
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

(* --- Liveness Analysis & CFG Construction (Placeholders) --- *)

(* Computes Use/Def sets for a single block *)
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

(* Performs iterative liveness analysis using a worklist algorithm *)
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

(* Apply dead code elimination within a list of instructions (intra-block) *)
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
  (* Start the backward pass. Initially, no variables are known to be live *out* of the block.
     This is a simplification; a full analysis would need live-out information. *)
  (* Start the backward pass with the known live_out set for this block *)
  eliminate (List.rev instrs) live_out []


(* --- Function and Program Optimization --- *)
(* Optimize each function in the program *)
let optimize_tac (prog: tac_program) : tac_program =
  let optimize_function func =
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

    (* 4. Optimize each block using liveness info *)
    let optimize_block (block: basic_block) : basic_block =
      let block_label = block.label in
      let live_out = LabelMap.find_opt block_label live_out_map |> Option.value ~default:SSet.empty in

      (* Iterate optimization passes until a fixed point is reached *)
      let rec run_passes current_instrs =
        let folded = constant_folding_pass current_instrs in
        let propagated = copy_propagation_pass folded in
        let dbe_optimized = dead_branch_elimination_pass propagated in
        let eliminated = dead_code_elimination_pass dbe_optimized live_out in

        if eliminated = current_instrs then
          current_instrs (* Fixed point reached *)
        else
          run_passes eliminated (* Rerun passes *)
      in

      let final_instrs = run_passes block.instrs in
      { block with instrs = final_instrs }
    in

    (* Apply optimization to all blocks *)
    let optimized_func = { func with blocks = List.map optimize_block func.blocks } in
    
    (* 5. Remove unreachable blocks *)
    unreachable_code_elimination_pass optimized_func
  in

  { functions = List.map optimize_function prog.functions }