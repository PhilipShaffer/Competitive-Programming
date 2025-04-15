# TAC Optimization: Inter-block Liveness Analysis Plan

## Goal

Implement inter-block liveness analysis to accurately compute which variables are live (needed later) at the exit of each basic block in the Three-Address Code (TAC) representation. Use this information to make the Dead Code Elimination (DCE) pass safe and effective, preventing the erroneous removal of variables needed in subsequent blocks.

## Core Concepts

*   **Control Flow Graph (CFG):** A representation of a function where nodes are basic blocks and directed edges represent possible control flow transfers (jumps, conditional branches, fallthroughs).
*   **Liveness:** A variable `v` is *live* at a program point `p` if there exists a path from `p` to a use of `v` along which `v` is not redefined.
*   **Dataflow Analysis:** An iterative algorithm that propagates information (in this case, liveness sets) through the CFG until a stable state (fixed point) is reached.
*   **`live_in[B]`:** The set of variables live at the entry of block `B`.
*   **`live_out[B]`:** The set of variables live at the exit of block `B`.
*   **`used[B]`:** The set of variables used (read) in block `B` *before* being defined (written) within `B`.
*   **`def[B]`:** The set of variables defined (written) in block `B`.

## Implementation Plan

The implementation will primarily involve modifications and additions to `lib/backend/tac_opt.ml`.

1.  **Data Structures:**
    *   Define types for CFG nodes (holding block label, instructions, predecessor labels, successor labels) and the CFG itself (e.g., a map from labels to nodes).
    *   Use maps from block labels to variable sets (`LabelMap.t -> SSet.t`) for `live_in` and `live_out` data.

2.  **CFG Construction:**
    *   Implement `build_cfg(blocks: basic_block list) : cfg`.
    *   Create a label-to-block map for lookups.
    *   Iterate through blocks to determine successors based on the last instruction (`Jump`, `CondJump`, `Return`, fallthrough).
    *   Calculate predecessors based on successor relationships.
    *   Return the completed CFG.

3.  **Compute Block `used` and `def` Sets:**
    *   Implement `compute_block_use_def(block: basic_block) : (SSet.t * SSet.t)`.
    *   Iterate through instructions *in order*:
        *   Track variables defined so far within the block.
        *   Add variables used by an instruction to `used[B]` only if they haven't been defined earlier *in the same block*.
        *   Add variables defined by an instruction to `def[B]`.
    *   Precompute and store these sets for all blocks.

4.  **Liveness Analysis (Iterative Dataflow):**
    *   Implement `analyze_liveness(cfg: cfg, block_use_def_map) : (live_in_map, live_out_map)`.
    *   Initialize `live_in` and `live_out` maps with empty sets.
    *   Use a worklist algorithm, initially containing all block labels.
    *   While the worklist is not empty:
        *   Dequeue block `B`.
        *   Calculate `new_live_out = Union(live_in[S])` for all successors `S` of `B`.
        *   Calculate `new_live_in = used[B] U (new_live_out - def[B])`.
        *   If `new_live_in` changed for `B`, update `live_in[B]` and enqueue all predecessors of `B`.
        *   Update `live_out[B]`.
    *   Return the converged `live_in` and `live_out` maps.

5.  **Integrate with Dead Code Elimination (DCE):**
    *   Modify `optimize_function` to orchestrate CFG building, use/def computation, and liveness analysis.
    *   Pass the computed `live_out_map` down through `optimize_block`.
    *   Modify `dead_code_elimination_pass` to accept the `live_out` set for the specific block being processed.
    *   Initialize the backward scan in DCE using this `live_out` set instead of an empty set.

## Process Flow Diagram

```mermaid
graph TD
    subgraph Per Function Optimization
        InputFunc[Function Blocks] --> BuildCFG[1. Build CFG];
        BuildCFG --> ComputeUseDef[2. Compute Use/Def per Block];
        ComputeUseDef --> AnalyzeLiveness[3. Analyze Liveness (Dataflow)];
        AnalyzeLiveness --> LiveOutMap[Live_Out Sets per Block];
        InputFunc --> OptimizeBlocks[4. Optimize Blocks];
        LiveOutMap --> OptimizeBlocks;
    end

    subgraph Per Block Optimization
        OptimizeBlocks --> InputBlock[Block B];
        InputBlock --> RunPasses[Run Optimization Passes];
        LiveOutMap -- Get Live_Out(B) --> RunPasses;
        RunPasses --> ConstantFold[Constant Folding];
        ConstantFold --> CopyProp[Copy Propagation];
        CopyProp --> DCE[Dead Code Elimination];
        DCE --> OutputBlock[Optimized Block B];
    end

    subgraph Dead Code Elimination (Modified)
        DCE --> InputInstr[Instructions of B];
        LiveOutMap -- Get Live_Out(B) --> InitDCE[Initialize Backward Scan];
        InitDCE -- live_vars = Live_Out(B) --> BackwardScan[Scan Instructions Backwards];
        BackwardScan --> CheckLiveness[Check if instr defines dead var];
        CheckLiveness -- Keep --> BackwardScan;
        CheckLiveness -- Remove --> BackwardScan;
        BackwardScan --> OutputInstr[Optimized Instructions];
    end

    InputFunc --> OptimizeBlocks;
    OptimizeBlocks --> RunPasses;
    RunPasses --> DCE;
    DCE --> OutputInstr;
```

## Next Steps

Proceed with implementing these changes, likely starting with the data structures and CFG construction, followed by the liveness analysis algorithm, and finally integrating the results into the DCE pass.