(** LLVM Code Generation for Piglet HIR *)

open Llvm

(** [codegen_hir hir]
    Translates the given HIR statement (typically the root of the program)
    into an LLVM module.
    @param hir The root HIR statement.
    @return The generated LLVM module.
*)
val codegen_hir : Hir.hir_stmt -> llmodule 