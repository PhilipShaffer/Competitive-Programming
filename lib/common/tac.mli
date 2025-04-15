(* lib/common/tac.mli *)

(** Three-Address Code Intermediate Representation *)

  type operand =
    | Var of string       (** Variable identifier *)
    | Const of int      (** Integer constant *)
    (* Add other constant types like Float, Bool, String if needed *)

  type label = string     (** Label identifier *)

  type instruction =
    | Assign of string * operand  (** x := y *)
    | BinOp of string * operand * Ast.bop * operand (** x := y op z *)
    (* Ast.bop assumes you have binary operators defined in your AST module *)
    | Label of label        (** L: *)
    | Jump of label         (** goto L *)
    | CondJump of operand * label * label (** if x goto L1 else goto L2 *)
    | Call of string option * string * operand list (** [x :=] f(arg1, arg2, ...) *)
    | Return of operand option (** return [x] *)
    (* Add memory operations if needed:
    | Load of string * operand    (* x := M[y] *)
    | Store of operand * operand  (* M[x] := y *)
    *)

  type basic_block = {
    label: label;          (** Entry label of the block *)
    instrs: instruction list; (** Instructions in the block *)
  }

  type tac_function = {
    name: string;           (** Function name *)
    params: string list;    (** Parameter names *)
    blocks: basic_block list; (** Basic blocks *)
  }

  type tac_program = {
    functions: tac_function list; (** List of functions in the program *)
    (* Add global variables if needed *)
  }

  (** Pretty printing functions (optional but helpful) *)
  val string_of_operand : operand -> string
  val string_of_instruction : instruction -> string
  val string_of_basic_block : basic_block -> string
  val string_of_tac_function : tac_function -> string
  val string_of_tac_program : tac_program -> string