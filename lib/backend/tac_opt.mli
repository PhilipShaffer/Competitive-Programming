(* lib/backend/tac_opt.mli *)

open Common.Tac

(** Optimizes a TAC program using various passes. *)
val optimize_tac : tac_program -> tac_program