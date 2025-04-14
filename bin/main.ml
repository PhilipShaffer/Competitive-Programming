open Base
open Core
open Core_unix
open Frontend
open Common.Type_checker

let parse_string str =
  let lexbuf = Lexing.from_string str in
  try
    let ast = Frontend.Parser.main Frontend.Lexer.read lexbuf in
    Ok (type_check_program ast)
  with
  | Failure msg -> Or_error.error_string msg
  | Parser.Error -> Or_error.error_string (sprintf "Syntax error at position %d" (Lexing.lexeme_start lexbuf))

let read_input () =
  let rec read_lines acc =
    match In_channel.input_line In_channel.stdin with
    | None -> List.rev acc
    | Some line -> read_lines (line :: acc)
  in
  String.concat ~sep:"\n" (read_lines [])

let run () =
  Command.basic
    ~summary:"CompeteX Compiler"
    ~readme:(fun () -> "A compiler for the CompeteX programming language")
    (let%map_open.Command () = return () in
     fun () ->
       printf "Enter a program (press Ctrl+D when done):\n%!";
       let input = read_input () in
       match parse_string input with
       | Ok ast ->
           printf "\nParsed and Type-Checked AST:\n%!";
           printf "%s\n%!" (Common.Ast.show_stmt ast)
       | Error e ->
           eprintf "Error: %s\n%!" (Error.to_string_hum e);
           exit 1)

let () = Command_unix.run run
