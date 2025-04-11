open Base
open Ast
open Types
open Env

(* Semantic error type *)
type semantic_error =
  | UndeclaredVariable of string
  | TypeMismatch of value_type * value_type
  | InvalidOperation of string
  | DuplicateDeclaration of string

(* Convert semantic error to string *)
let error_to_string = function
  | UndeclaredVariable name -> Printf.sprintf "Undeclared variable: %s" name
  | TypeMismatch (t1, t2) -> 
      Printf.sprintf "Type mismatch: expected %s, got %s" 
        (type_to_string t1) (type_to_string t2)
  | InvalidOperation msg -> Printf.sprintf "Invalid operation: %s" msg
  | DuplicateDeclaration name -> Printf.sprintf "Duplicate declaration: %s" name

(* Semantic analysis state *)
type sem_state = {
  env: environment;
  errors: semantic_error list;
}

(* Create initial semantic state *)
let create_state () = {
  env = create_env ();
  errors = [];
}

(* Add an error to the state *)
let add_error state error = {
  state with errors = error :: state.errors
}

(* Check if a variable is declared *)
let check_variable_declared state name =
  match lookup state.env name with
  | Some _ -> Ok ()
  | None -> Error (UndeclaredVariable name)

(* Analyze an expression and return its type and annotated expression *)
let rec analyze_expr state = function
  | Var name ->
      (match lookup state.env name with
       | Some entry -> Ok (entry.typ, Var name)
       | None -> Error (UndeclaredVariable name))
  
  | Int n -> Ok (IntType, Int n)
  | Float f -> Ok (FloatType, Float f)
  | String s -> Ok (StringType, String s)
  | Bool b -> Ok (BoolType, Bool b)
  
  | Binop (op, e1, e2) ->
      (match analyze_expr state e1 with
       | Error e -> Error e
       | Ok (t1, e1') ->
           match analyze_expr state e2 with
           | Error e -> Error e
           | Ok (t2, e2') ->
               if is_valid_binop op t1 t2 then
                 match binop_result_type op t1 t2 with
                 | Some t -> Ok (t, Binop (op, e1', e2'))
                 | None -> Error (InvalidOperation "Invalid binary operation")
               else
                 Error (TypeMismatch (t1, t2)))
  
  | Unop (op, e) ->
      (match analyze_expr state e with
       | Error e -> Error e
       | Ok (t, e') ->
           if is_valid_unop op t then
             match unop_result_type op t with
             | Some t' -> Ok (t', Unop (op, e'))
             | None -> Error (InvalidOperation "Invalid unary operation")
           else
             Error (TypeMismatch (t, t)))

(* Analyze a statement and return updated state and annotated statement *)
let rec analyze_stmt state = function
  | Assign (name, expr) ->
      (match check_variable_declared state name with
       | Error e -> Error e
       | Ok () ->
           match analyze_expr state expr with
           | Error e -> Error e
           | Ok (expr_type, expr') ->
               let var_type = get_var_type state.env name in
               if eq expr_type var_type then
                 Ok (state, Assign (name, expr'))
               else
                 Error (TypeMismatch (var_type, expr_type)))
  
  | Declare (name, typ, expr) ->
      (match lookup state.env name with
       | Some _ -> Error (DuplicateDeclaration name)
       | None ->
           match analyze_expr state expr with
           | Error e -> Error e
           | Ok (expr_type, expr') ->
               if eq expr_type typ then
                 let _ = add_var state.env name typ in
                 Ok (state, Declare (name, typ, expr'))
               else
                 Error (TypeMismatch (typ, expr_type)))
  
  | Let (name, expr, stmt) ->
      (match analyze_expr state expr with
       | Error e -> Error e
       | Ok (expr_type, expr') ->
           let new_env = create_env () in
           let _ = add_var new_env name expr_type in
           let new_state = { state with env = new_env } in
           match analyze_stmt new_state stmt with
           | Error e -> Error e
           | Ok (state', stmt') -> Ok (state', Let (name, expr', stmt')))
  
  | If (cond, then_stmt, else_stmt) ->
      (match analyze_expr state cond with
       | Error e -> Error e
       | Ok (cond_type, cond') ->
           if eq cond_type BoolType then
             match analyze_stmt state then_stmt with
             | Error e -> Error e
             | Ok (state1, then_stmt') ->
                 match analyze_stmt state else_stmt with
                 | Error e -> Error e
                 | Ok (_, else_stmt') -> Ok (state1, If (cond', then_stmt', else_stmt'))
           else
             Error (TypeMismatch (BoolType, cond_type)))
  
  | While (cond, stmt) ->
      (match analyze_expr state cond with
       | Error e -> Error e
       | Ok (cond_type, cond') ->
           if eq cond_type BoolType then
             match analyze_stmt state stmt with
             | Error e -> Error e
             | Ok (state', stmt') -> Ok (state', While (cond', stmt'))
           else
             Error (TypeMismatch (BoolType, cond_type)))
  
  | Print expr ->
      (match analyze_expr state expr with
       | Error e -> Error e
       | Ok (_, expr') -> Ok (state, Print expr'))
  
  | Block stmts ->
      let rec analyze_block state = function
        | [] -> Ok (state, [])
        | stmt :: rest ->
            match analyze_stmt state stmt with
            | Error e -> Error e
            | Ok (state', stmt') ->
                match analyze_block state' rest with
                | Error e -> Error e
                | Ok (state'', stmts') -> Ok (state'', stmt' :: stmts')
      in
      match analyze_block state stmts with
      | Error e -> Error e
      | Ok (state', stmts') -> Ok (state', Block stmts')

(* Main semantic analysis function *)
let analyze_program stmt =
  let state = create_state () in
  match analyze_stmt state stmt with
  | Error e -> Error e
  | Ok (_, annotated_stmt) -> Ok annotated_stmt 