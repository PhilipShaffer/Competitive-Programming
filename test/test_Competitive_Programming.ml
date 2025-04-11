open OUnit2
open Competitive_programming

let lexbuf = Lexing.from_string "fisse"
(* Define a simple test function *)
let test1 _ = 
  let token = Lexer.read lexbuf in
  match token with
  | Parser.ID s -> Printf.printf "ID(%s)\n" s
  | _ -> Printf.printf "Unexpected token\n"

(* Define the test suite *)
let suite =
  "suite" >::: [
    "test1" >:: test1;  (* Use ">::" to bind test names and functions *)
  ]

(* Run the test suite *)
let () =
  run_test_tt_main suite

