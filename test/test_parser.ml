open Alcotest
open Frontend (* Access Lexer, Parser, Parser_utils *)
open Common (* Need Ast for parser tests *)
open Ast (* Open Ast module directly *)

(* Helper function to parse a string into an AST statement *)
let parse_string s =
  let lexbuf = Lexing.from_string s in
  Parser.main Lexer.read lexbuf

(* Custom equality functions to ignore location information during tests *)
let rec equal_stmt_ignore_loc s1 s2 =
  match (s1, s2) with
  | Assign (id1, e1), Assign (id2, e2) -> id1 = id2 && equal_expr_ignore_loc e1 e2
  | Declare (id1, t1, e1), Declare (id2, t2, e2) -> id1 = id2 && equal_value_type t1 t2 && equal_expr_ignore_loc e1 e2
  | If (e1, t1, el1), If (e2, t2, el2) -> equal_expr_ignore_loc e1 e2 && equal_stmt_ignore_loc t1 t2 && equal_stmt_ignore_loc el1 el2
  | While (e1, b1), While (e2, b2) -> equal_expr_ignore_loc e1 e2 && equal_stmt_ignore_loc b1 b2
  | Print e1, Print e2 -> equal_expr_ignore_loc e1 e2
  | Block l1, Block l2 -> List.length l1 = List.length l2 && List.for_all2 equal_stmt_ignore_loc l1 l2
  | Return e1, Return e2 -> equal_expr_ignore_loc e1 e2
  | FunDef f1, FunDef f2 ->
      f1.fname = f2.fname &&
      List.length f1.params = List.length f2.params &&
      List.for_all2 (fun (n1, t1) (n2, t2) -> n1 = n2 && equal_value_type t1 t2) f1.params f2.params &&
      equal_value_type f1.return_type f2.return_type &&
      equal_stmt_ignore_loc f1.body f2.body (* Ignore loc in FunDef too *)
  | _, _ -> false

and equal_expr_ignore_loc e1 e2 =
  (* Ignore type_info and loc for now, focus on raw structure *)
  equal_raw_expr_ignore_loc e1.expr e2.expr (* && e1.type_info = e2.type_info *)

and equal_raw_expr_ignore_loc r1 r2 =
  match (r1, r2) with
  | Var id1, Var id2 -> id1 = id2
  | Int i1, Int i2 -> i1 = i2
  | String s1, String s2 -> s1 = s2
  | Float f1, Float f2 -> Float.equal f1 f2
  | Bool b1, Bool b2 -> b1 = b2
  | Binop (op1, l1, r1), Binop (op2, l2, r2) ->
      equal_bop op1 op2 && equal_expr_ignore_loc l1 l2 && equal_expr_ignore_loc r1 r2
  | Unop (op1, e1), Unop (op2, e2) ->
      equal_uop op1 op2 && equal_expr_ignore_loc e1 e2
  | FunCall (f1, args1), FunCall (f2, args2) ->
      f1 = f2 && List.length args1 = List.length args2 && List.for_all2 equal_expr_ignore_loc args1 args2
  | _, _ -> false

(* Use the custom equality function for statement testables *)
let stmt_testable = testable pp_stmt equal_stmt_ignore_loc

(* --- Parser Tests --- *)

let test_parser_simple_assign () =
  let input = "x := 1;" in
  (* Manually construct the expected AST node.
     NOTE: Locations are tricky to get right in manual construction.
           We might ignore locations in comparisons for simplicity initially. *)
  let expected = Block [Assign ("x", {expr=Int 1; type_info=None; loc={start_line=1; start_col=5; end_line=1; end_col=6}})] (* Simplified loc *)
  in
  check stmt_testable "Simple assign" expected (parse_string input)

let test_parser_if_else () =
  let input = "if x > 0 then { y := 1; } else { y := -1; }" in
  (* Placeholder locations - focus on structure *)
  let dummy_loc = {start_line=0; start_col=0; end_line=0; end_col=0} in
  (* Helper to create an expr record *)
  let make_expr raw_e = {expr=raw_e; type_info=None; loc=dummy_loc} in
  let expected = Block [
      If (
        (* Condition: x > 0 *)
        make_expr (Binop (Gt, make_expr (Var "x"), make_expr (Int 0))),
        (* Then branch: { y := 1; } *)
        Block [ Assign ("y", make_expr (Int 1)) ],
        (* Else branch: { y := -1; } *)
        Block [ Assign ("y", make_expr (Int (-1))) ]
      )
  ]
  in
  check stmt_testable "If/then/else" expected (parse_string input)

(* --- Test Suite --- *)

let parser_suite =
  [
    ("Parser", [
      test_case "Simple assignment" `Quick test_parser_simple_assign;
      test_case "If/then/else" `Quick test_parser_if_else;
      (* Add more parser tests here *)
    ]);
  ]

(* Run the tests *)
let () =
  Alcotest.run "Parser Tests" parser_suite