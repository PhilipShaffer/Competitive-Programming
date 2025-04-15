open Base
open Frontend
open Common.Type_checker
open Common.Tac
open Backend.Tac_gen
open Backend.Tac_opt

let parse_string str =
  let lexbuf = Lexing.from_string str in
  try
    let ast = Frontend.Parser.main Frontend.Lexer.read lexbuf in
    Ok (type_check_program ast)
  with
  | Failure msg -> Core.Or_error.error_string msg
  | Parser.Error -> Core.Or_error.error_string (Core.sprintf "Syntax error at position %d" (Lexing.lexeme_start lexbuf))

(* Helper function to read entire file content *)
let read_file_content filename =
  try Ok (Core.In_channel.read_all filename)
  with Sys_error msg -> Core.Or_error.error_string (Printf.sprintf "Error reading file '%s': %s" filename msg)

let run () =
  Core.Command.basic
    ~summary:"CompeteX Compiler"
    ~readme:(fun () -> "A compiler for the CompeteX programming language")
    (let%map_open.Core.Command filename = anon ("filename" %: string) in
     fun () ->
       match read_file_content filename with
       | Error e -> (* Handle file reading error *)
           Core.eprintf "Error: %s\n%!" (Base.Error.to_string_hum e);
           Core.exit 1
       | Ok input -> (* Proceed with compilation if file read successfully *)
           match parse_string input with
           | Ok ast ->
               Core.printf "Parsing and type checking successful for '%s'.\n%!" filename;
               (* Generate TAC *)
               Core.printf "Generating TAC...\n%!";
               let initial_tac = generate_tac [ast] in (* Wrap ast in a list *)
               (* Optimize TAC *)
               Core.printf "Optimizing TAC...\n%!";
               let optimized_tac = optimize_tac initial_tac in
               let tac_output_filename = "program.tac" in
               (* Write Optimized TAC to file *)
               (try
                  Core.Out_channel.write_all tac_output_filename ~data:(string_of_tac_program optimized_tac);
                  Core.printf "Optimized TAC successfully written to '%s'.\n%!" tac_output_filename
                with Sys_error msg ->
                  Core.eprintf "Error writing TAC file '%s': %s\n%!" tac_output_filename msg;
                  Core.exit 1);
           | Error e ->
               Core.eprintf "Compilation Error in '%s': %s\n%!" filename (Base.Error.to_string_hum e);
               Core.exit 1)

let () = Command_unix.run (run ())
