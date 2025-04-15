# TAC Optimization Enhancements Plan

## Goal

Enhance the TAC optimization phase to further improve code efficiency by implementing Dead Branch Elimination, Function Inlining, and related cleanup passes, building upon the existing liveness-based optimizations.

## Current State

The optimizer (`lib/backend/tac_opt.ml`) currently performs the following optimizations within each basic block, iterated until a fixed point is reached:

1.  **Constant Folding:** Evaluates constant expressions at compile time.
2.  **Copy Propagation:** Replaces variable uses with their known constant or variable equivalents.
3.  **Dead Code Elimination (Liveness-based):** Removes assignments to variables whose values are not used later, using inter-block liveness analysis to ensure correctness across blocks.

This process relies on a Control Flow Graph (CFG) built for each function and liveness information (`live_in`/`live_out` sets) computed via dataflow analysis.

## Proposed Enhancements

### 1. Dead Branch Elimination (Constant Conditions)

*   **Goal:** Simplify control flow by removing branches that are provably never taken based on constant conditions.
*   **Example:** Transform `if 1 goto L_true else goto L_false` into `Jump L_true`.
*   **Implementation Strategy:**
    *   Add a new pass, `dead_branch_elimination_pass`, that operates on a list of instructions.
    *   This pass looks for `CondJump (Const c, then_lbl, else_lbl)` instructions.
    *   If `c` represents true (e.g., `c != 0`), replace the `CondJump` with `Jump then_lbl`.
    *   If `c` represents false (e.g., `c == 0`), replace the `CondJump` with `Jump else_lbl`.
    *   Integrate this pass into the fixed-point iteration loop (`run_passes`) within `optimize_block` in `tac_opt.ml`. A good placement is after Constant Folding and Copy Propagation, as they might create the constant conditions.
    ```ocaml
    (* Inside optimize_block / run_passes loop *)
    let folded = constant_folding_pass current_instrs in
    let propagated = copy_propagation_pass folded in
    let dbe_optimized = dead_branch_elimination_pass propagated in (* New Pass *)
    let eliminated = dead_code_elimination_pass dbe_optimized live_out in
    (* ... check for fixed point ... *)
    ```
*   **Complexity:** Relatively low. Involves pattern matching on instructions and replacing one instruction with another.

### 2. Unreachable Code Elimination (Block Level)

*   **Goal:** Remove entire basic blocks that cannot be reached from the function's entry point. This is crucial after Dead Branch Elimination removes paths leading to certain blocks.
*   **Implementation Strategy:**
    *   Perform this optimization *after* the per-block optimizations within `optimize_function`.
    *   Requires the Control Flow Graph (CFG) already built by `build_cfg`.
    *   Perform a graph traversal (e.g., Depth-First Search or Breadth-First Search) starting from the function's entry block (the first block in `func.blocks`).
    *   Collect the set of all reachable block labels.
    *   Filter the function's `blocks` list, keeping only those whose labels are in the reachable set.
    *   **Important:** After removing blocks, the CFG (specifically predecessor/successor lists in remaining nodes) might become inconsistent if not updated. It might be simpler to rebuild the CFG *after* this pass if subsequent passes rely on it, or carefully update the links. For now, assume this is one of the final passes for the function.
*   **Complexity:** Medium. Requires CFG traversal and list/map manipulation.
*   **Mermaid Diagram:**
    ```mermaid
    graph TD
        A[Start optimize_function] --> B{Build CFG};
        B --> C{Run Per-Block Optimizations (Fixed Point)};
        C --> D{Perform Reachability Analysis (DFS/BFS from Entry)};
        D --> E{Identify Reachable Blocks};
        E --> F{Filter func.blocks};
        F --> G[Return Optimized Function];
    ```

### 3. Function Inlining

*   **Goal:** Replace calls to certain functions with their actual code bodies to eliminate call overhead and enable further optimizations (like propagation across the call boundary).
*   **Implementation Strategy:**
    *   **Define Heuristics:** Decide which functions are candidates for inlining. Simple criteria:
        *   Function size (e.g., fewer than X instructions/blocks).
        *   Non-recursive functions (to avoid infinite inlining).
        *   (Optional) Functions called only once.
        *   (Optional) Functions marked with an `inline` keyword in the source.
    *   **Create Inlining Pass:** This pass needs access to the definitions of *all* functions in the program (`tac_program`). It should likely run as a separate transformation on the `tac_program` *before* the main `optimize_tac` loop that calls `optimize_function`.
    *   **Process (within the Inlining Pass):**
        1.  Build a map of function names to their `tac_function` definitions.
        2.  Iterate through each function and its blocks/instructions.
        3.  When a `Call (dest_opt, fname, args)` is found:
        4.  Check if `fname` meets the inlining criteria using the function map and heuristics. Avoid self-recursion.
        5.  If yes:
            *   Retrieve the `tac_function` definition for `fname`.
            *   **Rename Locals/Params:** Generate unique names for all parameters and temporary variables (`_tX`) defined within the target function's body. This prevents name collisions with the calling context. A simple approach is to append a unique suffix based on the call site.
            *   **Substitute Parameters:** Replace uses of the function's original parameters with the corresponding `args` from the call site within the renamed function body.
            *   **Substitute Return:** Replace `Return (Some operand)` instructions with `Assign (dest, operand)` (using the call's `dest` from `dest_opt` if present). Handle `Return None` by simply removing the instruction. Ensure jumps go to the instruction *after* the original call site. This might require splitting the block containing the call.
            *   Replace the original `Call` instruction with the sequence of modified instructions from the inlined function body. Block splitting might be necessary if the call wasn't the last instruction.
    *   **Complexity:** High. Requires careful handling of variable renaming (alpha conversion), block splitting, access to global program structure, and robust heuristics to avoid code bloat.
*   **Placement:** Best run as a separate pass over the entire `tac_program` before `optimize_tac` iterates through `optimize_function`. May need multiple passes or integration with a call graph analysis for recursive cases.

### 4. Post-Inlining Optimizations

*   **Goal:** Clean up code after inlining.
*   **Implementation:** The existing fixed-point iteration within `optimize_block` (Constant Folding, Copy Propagation, DCE, Dead Branch Elimination) will automatically handle many opportunities created by inlining. No new passes are strictly required, but their iterative application is key.

## Suggested Order of Implementation

1.  **Implement Dead Branch Elimination:** Add `dead_branch_elimination_pass` to the `optimize_block` fixed-point loop.
2.  **Implement Unreachable Code Elimination:** Add this as a final step in `optimize_function` after the block optimization loop.
3.  **(Later) Implement Function Inlining:** Create a new pass that transforms the `tac_program` before `optimize_tac` is called.

This order tackles lower-complexity, high-impact optimizations first.