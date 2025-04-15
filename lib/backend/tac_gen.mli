(* lib/backend/tac_gen.mli *)

open Common.Ast
open Common.Tac

(** Translates an AST program (list of statements/definitions) into a TAC program. *)
val generate_tac : stmt list -> tac_program