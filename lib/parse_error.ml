(* Module for parser error handling *)

exception ParseError of string * Lexing.position * Lexing.position

let raise_parse_error msg start_pos end_pos =
  raise (ParseError (msg, start_pos, end_pos))