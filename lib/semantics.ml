open Base
open Ast
open Types
open Env

(* Enhanced position information *)
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

(* Semantic error type *)
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

(* Helper to generate type conversion suggestions *)
let suggest_conversion expected got value_str =
  match (expected, got) with
  | (IntType, FloatType) -> (
      try
        let f = Float.of_string value_str in
        if Float.is_integer f then
          Printf.sprintf "\nSuggestion: Did you mean to use integer %d instead of float %g?" 
            (Float.to_int f) f
        else
          Printf.sprintf "\nSuggestion: You can use Float.to_int to convert %g to an integer" f
      with _ -> ""
    )
  | (FloatType, IntType) -> (
      try
        let n = Int.of_string value_str in
        Printf.sprintf "\nSuggestion: Did you mean to use float %d.0 instead of integer %d?" n n
      with _ -> ""
    )
  | _ -> ""

(* Helper to suggest fixes for binary operations *)
let suggest_binop_fix op t1 t2 =
  match (op, t1, t2) with
  | (Add, IntType, FloatType) | (Sub, IntType, FloatType) | 
    (Mult, IntType, FloatType) | (Div, IntType, FloatType) ->
      "\nSuggestion: Convert both operands to float using Float.of_int, or convert the float to int using Float.to_int"
  | (Add, FloatType, IntType) | (Sub, FloatType, IntType) | 
    (Mult, FloatType, IntType) | (Div, FloatType, IntType) ->
      "\nSuggestion: Convert both operands to float using Float.of_int"
  | _ -> ""

(* Convert semantic error to string *)
let error_to_string = function
  | UndeclaredVariable name -> 
      Printf.sprintf "Undeclared variable: '%s'" name
  | TypeMismatch { expected; got; context; pos=_; value } -> 
      let suggestion = suggest_conversion expected got value in
      Printf.sprintf "Type mismatch: %s\n  Expected: %s\n  Got: %s%s" 
        context
        (type_to_string expected)
        (type_to_string got)
        suggestion
  | InvalidOperation msg -> 
      Printf.sprintf "Invalid operation: %s" msg
  | DuplicateDeclaration name -> 
      Printf.sprintf "Duplicate declaration of variable '%s'" name

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

(* Helper function to create a type-annotated expression *)
let mk_typed_expr raw_expr typ = {
  expr = raw_expr;
  type_info = Some { expr_type = typ; pos = None }
}

(* Helper to extract value from expression for suggestions *)
let get_expr_value code =
  try
    if String.contains code '.' then
      Float (Float.of_string code)
    else
      Int (Int.of_string code)
  with _ -> 
    String code  (* Default case if not a number *)

(* Check if a variable is declared *)
let check_variable_declared state name =
  match lookup state.env name with
  | Some _ -> Ok ()
  | None -> Error (UndeclaredVariable name)

(* Convert binary operator to string *)
let binop_to_string = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Lt -> "<"
  | Leq -> "<="
  | Gt -> ">"
  | Geq -> ">="
  | Eq -> "=="
  | Neq -> "!="
  | And -> "&&"
  | Or -> "||"

(* Convert unary operator to string *)
let unop_to_string = function
  | Neg -> "-"
  | Not -> "!"

(* Helper to get string representation of an expression *)
let expr_to_string = function
  | Int n -> Int.to_string n
  | Float f -> Float.to_string f
  | String s -> s
  | Bool b -> Bool.to_string b
  | Var name -> name
  | Binop (op, _, _) -> binop_to_string op
  | Unop (op, _) -> unop_to_string op

(* Analyze an expression and return its type and annotated expression *)
let rec analyze_expr state expr =
  match expr.expr with
  | Var name ->
      (match lookup state.env name with
       | Some entry -> Ok (entry.typ, mk_typed_expr (Var name) entry.typ)
       | None -> Error (UndeclaredVariable name))
  
  | Int n -> Ok (IntType, mk_typed_expr (Int n) IntType)
  | Float f -> Ok (FloatType, mk_typed_expr (Float f) FloatType)
  | String s -> Ok (StringType, mk_typed_expr (String s) StringType)
  | Bool b -> Ok (BoolType, mk_typed_expr (Bool b) BoolType)
  
  | Binop (op, e1, e2) ->
      (match analyze_expr state e1 with
       | Error err -> Error err
       | Ok (t1, e1') ->
           match analyze_expr state e2 with
           | Error err -> Error err
           | Ok (t2, e2') ->
               if is_valid_binop op t1 t2 then
                 match binop_result_type op t1 t2 with
                 | Some t -> Ok (t, mk_typed_expr (Binop (op, e1', e2')) t)
                 | None -> Error (InvalidOperation "Invalid binary operation")
               else
                 let context = Printf.sprintf "Cannot apply operator '%s' to these types" (binop_to_string op) in
                 let suggestion = suggest_binop_fix op t1 t2 in
                 Error (TypeMismatch { 
                   expected = t1;
                   got = t2;
                   context = context ^ suggestion;
                   pos = None;
                   value = expr_to_string e2.expr
                 }))
  
  | Unop (op, e) ->
      (match analyze_expr state e with
       | Error err -> Error err
       | Ok (t, e') ->
           if is_valid_unop op t then
             match unop_result_type op t with
             | Some t' -> Ok (t', mk_typed_expr (Unop (op, e')) t')
             | None -> Error (InvalidOperation "Invalid unary operation")
           else
             Error (TypeMismatch { 
               expected = t;
               got = t;
               context = Printf.sprintf "Cannot apply unary operator '%s' to this type" (unop_to_string op);
               pos = None;
               value = expr_to_string e.expr
             }))

(* Analyze a statement and return updated state and annotated statement *)
let rec analyze_stmt state stmt =
  match stmt with
  | Assign (name, expr) -> (
      match check_variable_declared state name with
      | Error e -> Error e
      | Ok () -> (
          match analyze_expr state expr with
          | Error e -> Error e
          | Ok (expr_type, expr') ->
              let var_type = get_var_type state.env name in
              if eq expr_type var_type then
                Ok (state, Assign (name, expr'))
              else
                Error (TypeMismatch { 
                  expected = var_type;
                  got = expr_type;
                  context = Printf.sprintf "Cannot assign value to variable '%s'" name;
                  pos = None;
                  value = expr_to_string expr.expr
                })
        )
    )

  | Declare (name, typ, init_expr) -> (
      match lookup state.env name with
      | Some _ -> Error (DuplicateDeclaration name)
      | None -> (
          match analyze_expr state init_expr with
          | Error e -> Error e
          | Ok (expr_type, expr') ->
              if eq expr_type typ then
                let _ = add_var state.env name typ in
                Ok (state, Declare (name, typ, expr'))
              else
                Error (TypeMismatch { 
                  expected = typ;
                  got = expr_type;
                  context = Printf.sprintf "Cannot initialize variable '%s' with wrong type" name;
                  pos = None;
                  value = expr_to_string init_expr.expr
                })
        )
    )

  | Let (name, expr, body) -> (
      match analyze_expr state expr with
      | Error e -> Error e
      | Ok (expr_type, expr') ->
          let new_env = create_env () in
          let _ = add_var new_env name expr_type in
          let new_state = { state with env = new_env } in
          match analyze_stmt new_state body with
          | Error e -> Error e
          | Ok (state', body') -> Ok (state', Let (name, expr', body'))
    )

  | If (cond, then_stmt, else_stmt) -> (
      match analyze_expr state cond with
      | Error e -> Error e
      | Ok (cond_type, cond') ->
          if eq cond_type BoolType then
            match analyze_stmt state then_stmt with
            | Error e -> Error e
            | Ok (state1, then_stmt') -> (
                match analyze_stmt state else_stmt with
                | Error e -> Error e
                | Ok (_, else_stmt') -> Ok (state1, If (cond', then_stmt', else_stmt'))
              )
          else
            Error (TypeMismatch { 
              expected = BoolType;
              got = cond_type;
              context = "Condition in if statement must be a boolean";
              pos = None;
              value = expr_to_string cond.expr
            })
    )

  | While (cond, body) -> (
      match analyze_expr state cond with
      | Error e -> Error e
      | Ok (cond_type, cond') ->
          if eq cond_type BoolType then
            match analyze_stmt state body with
            | Error e -> Error e
            | Ok (state', body') -> Ok (state', While (cond', body'))
          else
            Error (TypeMismatch { 
              expected = BoolType;
              got = cond_type;
              context = "Condition in while statement must be a boolean";
              pos = None;
              value = expr_to_string cond.expr
            })
    )

  | Print expr -> (
      match analyze_expr state expr with
      | Error e -> Error e
      | Ok (_, expr') -> Ok (state, Print expr')
    )

  | Block stmts -> (
      let rec analyze_block state = function
        | [] -> Ok (state, [])
        | stmt :: rest -> (
            match analyze_stmt state stmt with
            | Error e -> Error e
            | Ok (state', stmt') -> (
                match analyze_block state' rest with
                | Error e -> Error e
                | Ok (state'', stmts') -> Ok (state'', stmt' :: stmts')
              )
          )
      in
      match analyze_block state stmts with
      | Error e -> Error e
      | Ok (state', stmts') -> Ok (state', Block stmts')
    )

(* Main semantic analysis function *)
let analyze_program stmt =
  let state = create_state () in
  match analyze_stmt state stmt with
  | Error e -> Error e
  | Ok (_, annotated_stmt) -> Ok annotated_stmt 