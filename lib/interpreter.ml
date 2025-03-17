type env = (string * int) list (* Environment: list of variable bindings *)

let rec lookup (x : string) (env : env) : int =
  match env with
  | [] -> failwith ("Variable " ^ x ^ " not found")
  | (y, v) :: rest -> if x = y then v else lookup x rest

let rec eval_expr (e : Ast.expr) (env : env) : int =
  match e with
  | Ast.Var x -> lookup x env
  | Ast.Int n -> n
  | Ast.Bool b -> if b then 1 else 0
  | Ast.Binop (op, e1, e2) ->
      let v1 = eval_expr e1 env in
      let v2 = eval_expr e2 env in
      (match op with
       | Ast.Add -> v1 + v2
       | Ast.Sub -> v1 - v2
       | Ast.Mult -> v1 * v2
       | Ast.Div -> v1 / v2
       | Ast.Lt -> if v1 < v2 then 1 else 0
       | Ast.Leq -> if v1 <= v2 then 1 else 0
       | Ast.Gt -> if v1 > v2 then 1 else 0
       | Ast.Geq -> if v1 >= v2 then 1 else 0
       | Ast.Eq -> if v1 = v2 then 1 else 0
       | Ast.Neq -> if v1 <> v2 then 1 else 0
       | Ast.And -> if v1 <> 0 && v2 <> 0 then 1 else 0
       | Ast.Or -> if v1 <> 0 || v2 <> 0 then 1 else 0
       | Ast.Mod -> v1 mod v2)
  | Ast.Unop (op, e) ->
      let v = eval_expr e env in
      (match op with
       | Ast.Neg -> -v
       | Ast.Not -> if v = 0 then 1 else 0)

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
      if v <> 0 then eval_stmt s1 env else eval_stmt s2 env
  | Ast.While (e, s) ->
      let rec loop env =
        let v = eval_expr e env in
        if v <> 0 then
          let new_env = eval_stmt s env in
          loop new_env
        else env
      in
      loop env
  | Ast.Print e ->
      let v = eval_expr e env in
      Printf.printf "%d\n" v;
      env
  | Ast.Block sl ->
      List.fold_left (fun env s -> eval_stmt s env) env sl

let interpret (s : Ast.stmt) : unit =
  let _ = eval_stmt s [] in
  ()