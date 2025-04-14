(* Interface for the unified error handling module *)

open Ast (* Need Ast.location *)

(** Information captured for a compiler error *)
type error_info = {
  message: string;       (** The main error message *)
  loc: location;         (** The source code location of the error *)
  context: string option; (** Optional context (e.g., expected token, type mismatch details) *)
}

(** The exception raised for all compiler errors *)
exception CompilerError of error_info

(** Create an error_info record. *)
val make_error : message:string -> loc:location -> ?context:string option -> unit -> error_info

(** Create error info and raise the CompilerError exception. *)
val raise_error : message:string -> loc:location -> ?context:string option -> unit -> 'a

(** Format error information into a user-friendly string. *)
val format_error : error_info -> string

(** Helper to extract Ast.location from a lexer buffer. *)
val loc_of_lexbuf : Lexing.lexbuf -> location