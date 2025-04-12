open Common.Ast

(* Position information *)
type position = {
  line: int;
  column: int;
  code: string;
}

(* Enhanced type information *)
type type_info = {
  expr_type: value_type;
  pos: position option;
}

(* Error types *)
type semantic_error =
  | UndeclaredVariable of string
  | TypeMismatch of {
      expected: value_type;
      got: value_type;
      context: string;
      pos: position option;
      value: string;
    }
  | InvalidOperation of string
  | DuplicateDeclaration of string

(* State type *)
type sem_state

(* Core operations *)
val create_state : unit -> sem_state
val add_error : sem_state -> semantic_error -> sem_state
val analyze_expr : sem_state -> expr -> (value_type * expr, semantic_error) result
val analyze_program : stmt -> (stmt, semantic_error) result

(* Expression utilities *)
val get_expr_value : string -> raw_expr

(* Error handling *)
val error_to_string : semantic_error -> string 