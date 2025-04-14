open Ast

(* Type environment - maps variable names to their types *)
type type_env = (string * value_type) list

(* Helper function to get the type of a variable from the environment *)
let rec lookup_type env x =
  match env with
  | [] -> None
  | (y, t) :: rest -> if x = y then Some t else lookup_type rest x

(* Create a type_info with location *)
let mk_type_info t loc = { expr_type = t; pos = Some loc }

(* Infer the type of an expression *)
let rec infer_type env e =
  match e.expr with
  | Var x -> 
      (match lookup_type env x with
       | Some t -> { e with type_info = Some (mk_type_info t e.loc) }
       | None -> raise (Failure (Printf.sprintf "Unbound variable: %s at line %d, column %d" 
                                x e.loc.start_line e.loc.start_col)))
  | Int _ -> { e with type_info = Some (mk_type_info IntType e.loc) }
  | Float _ -> { e with type_info = Some (mk_type_info FloatType e.loc) }
  | String _ -> { e with type_info = Some (mk_type_info StringType e.loc) }
  | Bool _ -> { e with type_info = Some (mk_type_info BoolType e.loc) }
  | Binop (op, e1, e2) ->
      let e1' = infer_type env e1 in
      let e2' = infer_type env e2 in
      let t1 = (Option.get e1'.type_info).expr_type in
      let t2 = (Option.get e2'.type_info).expr_type in
      let result_type = match op with
        | Add | Sub | Mult | Div | Mod ->
            if equal_value_type t1 IntType && equal_value_type t2 IntType then IntType
            else if (equal_value_type t1 IntType || equal_value_type t1 FloatType) && 
                    (equal_value_type t2 IntType || equal_value_type t2 FloatType) then FloatType
            else raise (Failure (Printf.sprintf "Type mismatch in arithmetic operation at line %d, column %d" 
                               e.loc.start_line e.loc.start_col))
        | Lt | Leq | Gt | Geq ->
            if (equal_value_type t1 IntType || equal_value_type t1 FloatType) && 
               (equal_value_type t2 IntType || equal_value_type t2 FloatType) then BoolType
            else raise (Failure (Printf.sprintf "Type mismatch in comparison at line %d, column %d" 
                               e.loc.start_line e.loc.start_col))
        | Eq | Neq ->
            if equal_value_type t1 t2 then BoolType
            else raise (Failure (Printf.sprintf "Type mismatch in equality comparison at line %d, column %d" 
                               e.loc.start_line e.loc.start_col))
        | And | Or ->
            if equal_value_type t1 BoolType && equal_value_type t2 BoolType then BoolType
            else raise (Failure (Printf.sprintf "Type mismatch in logical operation at line %d, column %d" 
                               e.loc.start_line e.loc.start_col))
      in
      { expr = Binop (op, e1', e2'); type_info = Some (mk_type_info result_type e.loc); loc = e.loc }
  | Unop (op, e) ->
      let e' = infer_type env e in
      let t = (Option.get e'.type_info).expr_type in
      let result_type = match op with
        | Neg -> if equal_value_type t IntType || equal_value_type t FloatType then t 
                else raise (Failure (Printf.sprintf "Type mismatch in negation at line %d, column %d" 
                                   e.loc.start_line e.loc.start_col))
        | Not -> if equal_value_type t BoolType then BoolType 
                else raise (Failure (Printf.sprintf "Type mismatch in logical not at line %d, column %d" 
                                   e.loc.start_line e.loc.start_col))
      in
      { expr = Unop (op, e'); type_info = Some (mk_type_info result_type e.loc); loc = e.loc }

(* Type check a statement and return the updated environment *)
let rec type_check_stmt env stmt =
  match stmt with
  | Assign (x, e) ->
      let e' = infer_type env e in
      let t = (Option.get e'.type_info).expr_type in
      (match lookup_type env x with
       | Some t' when equal_value_type t t' -> (env, Assign (x, e'))
       | Some _ -> raise (Failure (Printf.sprintf "Type mismatch in assignment to %s at line %d, column %d" 
                                 x e.loc.start_line e.loc.start_col))
       | None -> raise (Failure (Printf.sprintf "Unbound variable in assignment: %s at line %d, column %d" 
                               x e.loc.start_line e.loc.start_col)))
  | Declare (x, t, e) ->
      let e' = infer_type env e in
      let t' = (Option.get e'.type_info).expr_type in
      if equal_value_type t t' then ((x, t) :: env, Declare (x, t, e'))
      else raise (Failure (Printf.sprintf "Type mismatch in declaration of %s at line %d, column %d" 
                         x e.loc.start_line e.loc.start_col))
  | Let (x, e, s) ->
      let e' = infer_type env e in
      let t = (Option.get e'.type_info).expr_type in
      let (env', s') = type_check_stmt ((x, t) :: env) s in
      (List.tl env', Let (x, e', s'))
  | If (e, s1, s2) ->
      let e' = infer_type env e in
      let t = (Option.get e'.type_info).expr_type in
      if not (equal_value_type t BoolType) then 
        raise (Failure (Printf.sprintf "Condition in if statement must be boolean at line %d, column %d" 
                      e.loc.start_line e.loc.start_col))
      else
        let (_, s1') = type_check_stmt env s1 in
        let (_, s2') = type_check_stmt env s2 in
        (env, If (e', s1', s2'))
  | While (e, s) ->
      let e' = infer_type env e in
      let t = (Option.get e'.type_info).expr_type in
      if not (equal_value_type t BoolType) then 
        raise (Failure (Printf.sprintf "Condition in while loop must be boolean at line %d, column %d" 
                      e.loc.start_line e.loc.start_col))
      else
        let (_, s') = type_check_stmt env s in
        (env, While (e', s'))
  | Print e ->
      let e' = infer_type env e in
      (env, Print e')
  | Block sl ->
      let (env', sl') = List.fold_left
        (fun (env, sl) s ->
          let (env', s') = type_check_stmt env s in
          (env', s' :: sl))
        (env, []) sl
      in
      (env', Block (List.rev sl'))

(* Type check a program *)
let type_check_program stmt =
  let (_, stmt') = type_check_stmt [] stmt in
  stmt' 