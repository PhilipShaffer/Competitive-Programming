
type value = 
  | Int of int
  | String of string (* String type *)

type env = (string * value) list (* Environment: list of variable bindings *)


let rec lookup (x : string) (env : env) : value =
  match env with
  | [] -> failwith ("Variable " ^ x ^ " not found")
  | (y, v) :: rest -> if x = y then v else lookup x rest

let rec eval_expr (e : Ast.expr) (env : env) : value =
  match e with
  | Ast.Var x -> lookup x env
  | Ast.Int n -> Int n
  | Ast.Bool b -> if b then Int 1 else Int 0
  | Ast.String str -> String str
  | Ast.Binop (op, e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in
    (match op with
      | Ast.Add -> (match v1, v2 with
            | Int i1, Int i2 -> Int (i1 + i2)  (* Addition works only for Int values *)
            | _ -> failwith "Invalid types for addition")
      | Ast.Sub -> (match v1, v2 with
            | Int i1, Int i2 -> Int (i1 - i2)  (* Subtraction works only for Int values *)
            | _ -> failwith "Invalid types for subtraction")
      | Ast.Mult -> (match v1, v2 with
            | Int i1, Int i2 -> Int (i1 * i2)  (* Multiplication works only for Int values *)
            | _ -> failwith "Invalid types for multiplication")
      | Ast.Div -> (match v1, v2 with
            | Int i1, Int i2 -> Int (i1 / i2)  (* Division works only for Int values *)
            | _ -> failwith "Invalid types for division")
      | Ast.Lt -> (match v1, v2 with
            | Int i1, Int i2 -> Int (if i1 < i2 then 1 else 0)  (* Less-than comparison works for Int *)
            | _ -> failwith "Invalid types for less-than comparison")
      | Ast.Leq -> (match v1, v2 with
            | Int i1, Int i2 -> Int (if i1 <= i2 then 1 else 0)  (* Less-than-or-equal comparison for Int *)
            | _ -> failwith "Invalid types for less-than-or-equal comparison")
      | Ast.Gt -> (match v1, v2 with
            | Int i1, Int i2 -> Int (if i1 > i2 then 1 else 0)  (* Greater-than comparison for Int *)
            | _ -> failwith "Invalid types for greater-than comparison")
      | Ast.Geq -> (match v1, v2 with
            | Int i1, Int i2 -> Int (if i1 >= i2 then 1 else 0)  (* Greater-than-or-equal comparison for Int *)
            | _ -> failwith "Invalid types for greater-than-or-equal comparison")
      | Ast.Eq -> (match v1, v2 with
            | Int i1, Int i2 -> Int (if i1 = i2 then 1 else 0)  (* Equality comparison works for Int *)
            | String s1, String s2 -> Int (if s1 = s2 then 1 else 0)  (* Equality comparison works for String *)
            | _ -> failwith "Invalid types for equality comparison")
      | Ast.Neq -> (match v1, v2 with
            | Int i1, Int i2 -> Int (if i1 <> i2 then 1 else 0)  (* Inequality comparison for Int *)
            | String s1, String s2 -> Int (if s1 <> s2 then 1 else 0)  (* Inequality comparison for String *)
            | _ -> failwith "Invalid types for inequality comparison")
      | Ast.And -> (match v1, v2 with
            | Int i1, Int i2 -> Int (if i1 <> 0 && i2 <> 0 then 1 else 0)  (* Logical AND for Int values *)
            | _ -> failwith "Invalid types for logical AND")
      | Ast.Or -> (match v1, v2 with
            | Int i1, Int i2 -> Int (if i1 <> 0 || i2 <> 0 then 1 else 0)  (* Logical OR for Int values *)
            | _ -> failwith "Invalid types for logical OR")
      | Ast.Mod -> (match v1, v2 with
            | Int i1, Int i2 -> Int (i1 mod i2)  (* Modulo operation for Int values *)
            | _ -> failwith "Invalid types for modulo"))
  | Ast.Unop (op, e) ->
      let v = eval_expr e env in
      (match op with
          | Ast.Neg -> (match v with
              | Int i -> Int (-i)  (* Negation works for Int only *)
              | _ -> failwith "Invalid type for negation")
          | Ast.Not -> (match v with
              | Int i -> Int (if i = 0 then 1 else 0)  (* Logical NOT for Int only *)
              | _ -> failwith "Invalid type for logical NOT"))

let rec eval_stmt (s : Ast.stmt) (env : env) : env =
  match s with
  | Ast.Assign (x, e) ->
      let v = eval_expr e env in
      (x, v) :: env
  | Ast.Let (x, e, s) ->
      let v = eval_expr e env in
      let new_env = (x, v) :: env in
      eval_stmt s new_env
  | Ast.If (e, s1, s2) ->
      let v = eval_expr e env in
      if v <> Int 0 then eval_stmt s1 env else eval_stmt s2 env
  | Ast.While (e, s) ->
      let rec loop env =
        let v = eval_expr e env in
        if v <> Int 0 then
          let new_env = eval_stmt s env in
          loop new_env
        else env
      in
      loop env
  | Ast.Print e ->
      let v = eval_expr e env in
      (match v with
        | Int n -> Printf.printf "%d\n" n       (* Print Int values *)
        | String s -> Printf.printf "%s\n" s);  (* Print String values *)
        env
  | Ast.Block sl ->
      List.fold_left (fun env s -> eval_stmt s env) env sl

let interpret (s : Ast.stmt) : unit =
  let _ = eval_stmt s [] in
  ()