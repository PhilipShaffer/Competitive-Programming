(* Implementation of the unified error handling module *)

open Ast (* Need Ast.location *)

(* Information captured for a compiler error *)
(* The record type definition is implicit from the .mli *)
type error_info = {
  message: string;
  loc: location;
  context: string option;
}

(* The exception raised for all compiler errors *)
exception CompilerError of error_info

(* Helper to extract Ast.location from a lexer buffer. *)
let loc_of_lexbuf lexbuf : location =
  let start_pos = Lexing.lexeme_start_p lexbuf in
  let end_pos = Lexing.lexeme_end_p lexbuf in
  {
    start_line = start_pos.pos_lnum;
    start_col = start_pos.pos_cnum - start_pos.pos_bol;
    end_line = end_pos.pos_lnum;
    end_col = end_pos.pos_cnum - end_pos.pos_bol;
  }

(* Create an error_info record. *)
let make_error ~(message: string) ~(loc: location) ?(context: string option = None) () : error_info =
  { message; loc; context }

(* Create error info and raise the CompilerError exception. *)
let raise_error ~(message: string) ~(loc: location) ?(context: string option = None) () : 'a =
  let error_info = make_error ~message ~loc ~context () in
  raise (CompilerError error_info)

(* Format error information into a user-friendly string. *)
let format_error (info: error_info) : string =
  let loc_str = Printf.sprintf "line %d, column %d" info.loc.start_line info.loc.start_col in
  match info.context with
  | None -> Printf.sprintf "Error at %s: %s" loc_str info.message
  | Some ctx -> Printf.sprintf "Error at %s: %s (%s)" loc_str info.message ctx