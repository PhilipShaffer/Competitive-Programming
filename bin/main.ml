open Base
open Frontend
open Common.Type_checker

let parse_string str =
  let lexbuf = Lexing.from_string str in
  try
    let ast = Frontend.Parser.main Frontend.Lexer.read lexbuf in
    Ok (type_check_program ast)
  with
  | Failure msg -> Core.Or_error.error_string msg
  | Parser.Error -> Core.Or_error.error_string (Core.sprintf "Syntax error at position %d" (Lexing.lexeme_start lexbuf))

let read_input () =
  let rec read_lines acc =
    match Core.In_channel.input_line Core.In_channel.stdin with
    | None -> Core.List.rev acc
    | Some line -> read_lines (line :: acc)
  in
  Core.String.concat ~sep:"\n" (read_lines [])

let run () =
  Core.Command.basic
    ~summary:"CompeteX Compiler"
    ~readme:(fun () -> "A compiler for the CompeteX programming language")
    (let%map_open.Core.Command () = Core.Command.Param.return () in
     fun () ->
       Core.printf "Enter a program (press Ctrl+D when done):\n%!";
       let input = read_input () in
       match parse_string input with
       | Ok ast ->
           Core.printf "\nParsed and Type-Checked AST:\n%!";
           Core.printf "%s\n%!" (Common.Ast.show_stmt ast)
       | Error e ->
           Core.eprintf "Error: %s\n%!" (Base.Error.to_string_hum e);
           Core.exit 1)

let () = Command_unix.run (run ())
