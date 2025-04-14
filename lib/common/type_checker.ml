open Ast
open Error (* Use the new error module (already within Common library) *)

(* Type environment for variables *)
type type_env = (string * value_type) list

(* Type environment for functions: maps name to (param_types, return_type) *)
type fun_env = (string * (value_type list * value_type)) list

(* Helper function to get the type of a variable from the environment *)
let rec lookup_var_type env x =
  match env with
  | [] -> None
  | (y, t) :: rest -> if x = y then Some t else lookup_var_type rest x

(* Helper function to get the signature of a function from the environment *)
let rec lookup_fun_sig fun_env fname =
  match fun_env with
  | [] -> None
  | (name, sig') :: rest -> if fname = name then Some sig' else lookup_fun_sig rest fname

(* Create a type_info with location *)
let mk_type_info t loc = { expr_type = t; pos = Some loc }

(* Old error reporting helper removed *)

(* Infer the type of an expression *)
let rec infer_type env fun_env e =
  match e.expr with
  | Var x ->
      (match lookup_var_type env x with
       | Some t -> { e with type_info = Some (mk_type_info t e.loc) }
       | None -> raise_error ~message:(Printf.sprintf "Unbound variable: %s" x) ~loc:e.loc ())
  | Int _ -> { e with type_info = Some (mk_type_info IntType e.loc) }
  | Float _ -> { e with type_info = Some (mk_type_info FloatType e.loc) }
  | String _ -> { e with type_info = Some (mk_type_info StringType e.loc) }
  | Bool _ -> { e with type_info = Some (mk_type_info BoolType e.loc) }
  | Binop (op, e1, e2) ->
      let e1' = infer_type env fun_env e1 in
      let e2' = infer_type env fun_env e2 in
      let t1 = (Option.get e1'.type_info).expr_type in
      let t2 = (Option.get e2'.type_info).expr_type in
      let result_type = (* Determine result type OR raise error *)
        match op with
        | Add | Sub | Mult | Div | Mod ->
            if equal_value_type t1 IntType && equal_value_type t2 IntType then IntType
            else if (equal_value_type t1 IntType || equal_value_type t1 FloatType) &&
                    (equal_value_type t2 IntType || equal_value_type t2 FloatType) then FloatType
            else raise_error ~message:(Printf.sprintf "Type mismatch: Operands for arithmetic op must be Int/Int or compatible Int/Float, got %s and %s" (show_value_type t1) (show_value_type t2)) ~loc:e.loc ()
        | Lt | Leq | Gt | Geq ->
            if (equal_value_type t1 IntType || equal_value_type t1 FloatType) &&
               (equal_value_type t2 IntType || equal_value_type t2 FloatType) then BoolType
            else raise_error ~message:(Printf.sprintf "Type mismatch: Operands for comparison must be Int or Float, got %s and %s" (show_value_type t1) (show_value_type t2)) ~loc:e.loc ()
        | Eq | Neq ->
             (* Note: Requires types to be exactly the same. Add check for Int/Float comparison if needed *)
            if equal_value_type t1 t2 then BoolType
            else raise_error ~message:(Printf.sprintf "Type mismatch: Operands for equality must be the same type, got %s and %s" (show_value_type t1) (show_value_type t2)) ~loc:e.loc ()
        | And | Or ->
            if equal_value_type t1 BoolType && equal_value_type t2 BoolType then BoolType
            else raise_error ~message:(Printf.sprintf "Type mismatch: Operands for logical op must be Bool, got %s and %s" (show_value_type t1) (show_value_type t2)) ~loc:e.loc ()
      in
      { expr = Binop (op, e1', e2'); type_info = Some (mk_type_info result_type e.loc); loc = e.loc }
  | Unop (op, exp) ->
      let exp' = infer_type env fun_env exp in
      let t = (Option.get exp'.type_info).expr_type in
      let result_type =
        match op with
        | Neg ->
            if equal_value_type t IntType || equal_value_type t FloatType then t
            else raise_error ~message:(Printf.sprintf "Type mismatch: Operand for negation must be Int or Float, got %s" (show_value_type t)) ~loc:e.loc ()
        | Not ->
            if equal_value_type t BoolType then BoolType
            else raise_error ~message:(Printf.sprintf "Type mismatch: Operand for logical not must be Bool, got %s" (show_value_type t)) ~loc:e.loc ()
      in
      { expr = Unop (op, exp'); type_info = Some (mk_type_info result_type e.loc); loc = e.loc }
  | FunCall (fname, args) ->
      (* Perform the checks within the match, returning the final typed expression record *)
      let typed_call_expr =
        match lookup_fun_sig fun_env fname with
        | None -> raise_error ~message:(Printf.sprintf "Undefined function: %s" fname) ~loc:e.loc ()
        | Some (param_types, return_type) ->
            let typed_args = List.map (infer_type env fun_env) args in
            if List.length typed_args <> List.length param_types then
              raise_error ~message:(Printf.sprintf "Function %s expects %d arguments, but got %d"
                            fname (List.length param_types) (List.length typed_args)) ~loc:e.loc ()
            else begin (* Use begin/end for clarity with side-effect + return *)
              List.iter2 (fun typed_arg expected_type ->
                let arg_type = (Option.get typed_arg.type_info).expr_type in
                if not (equal_value_type arg_type expected_type) then
                  raise_error ~message:(Printf.sprintf "Type mismatch in argument for function %s. Expected %s but got %s"
                                fname (show_value_type expected_type) (show_value_type arg_type)) ~loc:typed_arg.loc ()
              ) typed_args param_types;
              (* If checks pass, construct the result record *)
              { expr = FunCall (fname, typed_args); type_info = Some (mk_type_info return_type e.loc); loc = e.loc }
            end (* End of begin/end block *)
      in
      typed_call_expr (* Return the result of the match *)

(* Type check a statement and return the updated environments and typed statement *)
(* current_fun_return_type is Some t when inside a function expecting return type t *)
let rec type_check_stmt env fun_env current_fun_return_type stmt =
  match stmt with
  | Assign (x, e) ->
      let e' = infer_type env fun_env e in
      let t_expr = (Option.get e'.type_info).expr_type in
      (match lookup_var_type env x with
       | Some t_var when equal_value_type t_expr t_var -> (env, fun_env, Assign (x, e'))
       | Some t_var -> raise_error ~message:(Printf.sprintf "Type mismatch in assignment to %s. Expected %s but got %s"
                                     x (show_value_type t_var) (show_value_type t_expr)) ~loc:e.loc ()
       | None -> raise_error ~message:(Printf.sprintf "Unbound variable in assignment: %s" x) ~loc:e.loc ())
  | Declare (x, t, e) ->
      (match lookup_var_type env x with
       | Some _ -> raise_error ~message:(Printf.sprintf "Variable %s already declared in this scope" x) ~loc:e.loc ()
       | None ->
           let e' = infer_type env fun_env e in
           let t' = (Option.get e'.type_info).expr_type in
           if equal_value_type t t' then
             let env' = (x, t) :: env in
             (env', fun_env, Declare (x, t, e'))
           else raise_error ~message:(Printf.sprintf "Type mismatch in declaration of %s. Declared %s but assigned %s"
                              x (show_value_type t) (show_value_type t')) ~loc:e.loc ())
  | If (e, s1, s2) ->
      let e' = infer_type env fun_env e in
      let t = (Option.get e'.type_info).expr_type in
      if not (equal_value_type t BoolType) then
        raise_error ~message:"Condition in if statement must be boolean" ~loc:e.loc ()
      else
        (* Check branches, passing down the expected return type *)
        let (_, _, s1') = type_check_stmt env fun_env current_fun_return_type s1 in
        let (_, _, s2') = type_check_stmt env fun_env current_fun_return_type s2 in
        (env, fun_env, If (e', s1', s2'))
  | While (e, s) ->
      let e' = infer_type env fun_env e in
      let t = (Option.get e'.type_info).expr_type in
      if not (equal_value_type t BoolType) then
        raise_error ~message:"Condition in while loop must be boolean" ~loc:e.loc ()
      else
        (* Check body, passing down the expected return type *)
        let (_, _, s') = type_check_stmt env fun_env current_fun_return_type s in
        (env, fun_env, While (e', s'))
  | Print e ->
      let e' = infer_type env fun_env e in
      (env, fun_env, Print e')
  | Block sl ->
      (* Fold through statements, updating environments sequentially *)
      let (env_final, fun_env_final, sl_rev_typed) =
        List.fold_left
          (fun (current_env, current_fun_env, typed_stmts) s ->
            (* Pass down the expected return type *)
            let (next_env, next_fun_env, s') = type_check_stmt current_env current_fun_env current_fun_return_type s in
            (next_env, next_fun_env, s' :: typed_stmts))
          (env, fun_env, []) sl
      in
      (env_final, fun_env_final, Block (List.rev sl_rev_typed))
  | Return e ->
      (match current_fun_return_type with
       | None -> raise_error ~message:"Return statement used outside of a function" ~loc:e.loc ()
       | Some expected_type ->
           let e' = infer_type env fun_env e in
           let actual_type = (Option.get e'.type_info).expr_type in
           if not (equal_value_type actual_type expected_type) then
             raise_error ~message:(Printf.sprintf "Return type mismatch. Expected %s but got %s"
                           (show_value_type expected_type) (show_value_type actual_type)) ~loc:e.loc ()
           else
             (env, fun_env, Return e')
      )
  | FunDef { fname; params; return_type; body; loc } ->
      (match lookup_var_type env fname with
       | Some _ -> raise_error ~message:(Printf.sprintf "Name collision: %s is already defined as a variable" fname) ~loc ()
       | None ->
           (match lookup_fun_sig fun_env fname with
            | Some _ -> raise_error ~message:(Printf.sprintf "Function %s already defined" fname) ~loc ()
            | None ->
                (* Create environment for the function body *)
                let body_env = List.fold_left (fun acc (pname, ptype) -> (pname, ptype) :: acc) env params in
                (* Type check the body, passing the expected return type *)
                let (_, _, typed_body) = type_check_stmt body_env fun_env (Some return_type) body in
                (* Add function signature to the function environment *)
                let param_types = List.map snd params in
                let fun_env' = (fname, (param_types, return_type)) :: fun_env in
                (env, fun_env', FunDef { fname; params; return_type; body = typed_body; loc })
           )
      )

(* Type check a program (a single statement, often a block) *)
let type_check_program stmt =
  (* Start with empty environments and no expected return type *)
  let (_, _, stmt') = type_check_stmt [] [] None stmt in
  stmt'