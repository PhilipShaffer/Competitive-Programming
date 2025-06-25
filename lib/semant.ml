(* Semantic analysis for the strict While language *)

open Ast
open Hir

exception Semantic_error of string

(* Symbol table entry - removed array size tracking as it becomes stale *)
type symbol_info =
  | SymVar of value_type
  | SymFun of (value_type list) * value_type

(* Symbol table with scope stack *)
type symbol_table = (string, symbol_info) Hashtbl.t list

(* Generate fresh unique symbol IDs with scope isolation *)
type id_context = {
  mutable counter: int;
  name_to_id: (string, int) Hashtbl.t;
}

let create_id_context () = {
  counter = 0;
  name_to_id = Hashtbl.create 256;
}

let fresh_symbol ctx =
  let id = ctx.counter in
  ctx.counter <- ctx.counter + 1;
  id

let sym_of_name ctx name =
  try Hashtbl.find ctx.name_to_id name 
  with Not_found -> 
    let sid = fresh_symbol ctx in 
    Hashtbl.add ctx.name_to_id name sid; 
    sid

(* Array size tracking removed - arrays manage their own size at runtime *)

(* Lookup symbol in the stack of scopes *)
let rec lookup_symbol tbls name =
  match tbls with
  | [] -> None
  | tbl :: rest -> (try Some (Hashtbl.find tbl name) with Not_found -> lookup_symbol rest name)

(* Check if a symbol exists in the current scope (top of stack) *)
let lookup_in_current_scope tbl name =
  match tbl with
  | [] -> None
  | current :: _ -> (try Some (Hashtbl.find current name) with Not_found -> None)

(* Enter a new scope *)
let push_scope tbls = (Hashtbl.create 16) :: tbls

(* Exit a scope *)
let pop_scope tbls = match tbls with | _ :: rest -> rest | [] -> []

(* Add a symbol to the current scope, fail if it already exists in this scope *)
let add_symbol tbls name info =
  match tbls with
  | tbl :: _ ->
      if Hashtbl.mem tbl name then
        raise (Semantic_error ("Symbol already defined in this scope: " ^ name))
      else
        Hashtbl.add tbl name info
  | [] -> failwith "No scope to add symbol"

(* Main entry for semantic analysis: from AST to HIR *)
let rec analyze_stmt (ctx : id_context) (tbls : symbol_table) (stmt : Ast.stmt) : Hir.hir_stmt =
  match stmt with
  | Assign (x, e) ->
      let sym = match lookup_symbol tbls x with Some (SymVar _) -> sym_of_name ctx x | _ -> raise (Semantic_error ("Undeclared variable: " ^ x)) in
      let he, t = analyze_expr ctx tbls e in
      (* Strict typing: must match exactly *)
      (match lookup_symbol tbls x with 
       | Some (SymVar t') when t = t' -> () 
       | _ -> raise (Semantic_error ("Type mismatch in assignment to: " ^ x)));
      HAssign (sym, he)
  | ArrayAssign (arr, idx, value) ->
      let harr, arr_type = analyze_expr ctx tbls arr in
      let hidx, idx_type = analyze_expr ctx tbls idx in
      let hval, val_type = analyze_expr ctx tbls value in
      if idx_type <> IntType then raise (Semantic_error "Array index must be an integer");
      
      (* Reuse the same bounds checking logic for array assignments *)
      let check_constant_array _name arr_len idx_val =
        if idx_val < 0 then 
          raise (Semantic_error ("[Semant] Array index out of bounds: negative index " ^ string_of_int idx_val))
        else if idx_val >= arr_len then
          raise (Semantic_error ("[Semant] Array index out of bounds: index " ^ string_of_int idx_val ^ 
                                " exceeds array length " ^ string_of_int arr_len))
      in
      
      (* Check bounds when possible and track if we've statically verified bounds *)
      let bounds_checked = 
        (* First handle the negative index case separately to avoid control flow warnings *)
        match idx with
        | Int i when i < 0 ->
            (* We can always detect negative indices at compile time *)
            raise (Semantic_error ("[Semant] Array index out of bounds: negative index " ^ string_of_int i))
        | _ ->
            (* Now handle the regular cases *)
            match arr, idx with
            | ArrayLit elems, Int i -> 
                let arr_len = List.length elems in
                check_constant_array "array literal" arr_len i;
                true (* Bounds checked at compile time *)
            | Var _arr_name, Int _i ->
                (* For named arrays, we can't statically determine size without tracking
                 but we can still check for negative indices *)
              false
            | _, _ -> 
                false (* Need runtime bounds checking *)
      in

      (* Type checking *)
      (match arr_type with
      | ArrayType elem_type when elem_type = val_type -> HArrayAssign (harr, hidx, hval, bounds_checked)
      | ArrayType _ -> raise (Semantic_error "Type mismatch in array assignment")
      | _ -> raise (Semantic_error "Cannot index non-array type"))
  | ArrayPut (arr, value) ->
      let harr, arr_type = analyze_expr ctx tbls arr in
      let hval, val_type = analyze_expr ctx tbls value in
      (match arr_type with
      | ArrayType elem_type when elem_type = val_type -> 
          HArrayPut (harr, hval)
      | ArrayType _ -> raise (Semantic_error "Type mismatch in put statement")
      | _ -> raise (Semantic_error "Cannot put to non-array type"))
  | ArrayPop arr ->
      let harr, arr_type = analyze_expr ctx tbls arr in
      (match arr_type with
      | ArrayType _ -> HArrayPop harr
      | _ -> raise (Semantic_error "Cannot pop from non-array type"))
  | Declare (x, t, e) ->
      let sym = fresh_symbol ctx in
      Hashtbl.replace ctx.name_to_id x sym;
      let he, et = analyze_expr ctx tbls e in
      if t <> et then raise (Semantic_error ("Type mismatch in declaration of: " ^ x));
      
      add_symbol tbls x (SymVar t);
      HDeclare (sym, t, he)
  (* Let binding removed as it's no longer needed *)
  | If (cond, st, sf) ->
      let hc, t = analyze_expr ctx tbls cond in
      if t <> BoolType then raise (Semantic_error "Condition must be bool");
      let hst = analyze_stmt ctx (push_scope tbls) st in
      let hsf = analyze_stmt ctx (push_scope tbls) sf in
      HIf (hc, hst, hsf)
  | While (cond, body) ->
      let hc, t = analyze_expr ctx tbls cond in
      if t <> BoolType then raise (Semantic_error "While condition must be bool");
      let hbody = analyze_stmt ctx (push_scope tbls) body in
      HWhile (hc, hbody)
  | Print e ->
      let he, _ = analyze_expr ctx tbls e in
      HPrint he
  | Block sl ->
      let tbls' = push_scope tbls in
      (* First pass: add all function signatures to the symbol table *)
      List.iter (function
        | FunDecl (name, params, ret_type, _) ->
            if lookup_in_current_scope tbls' name <> None then
              raise (Semantic_error ("Function already defined in this scope: " ^ name))
            else
              add_symbol tbls' name (SymFun (List.map snd params, ret_type))
        | _ -> ()) sl;
      (* Second pass: analyze all statements *)
      let hir_stmts = List.map (analyze_stmt ctx tbls') sl in
      HBlock hir_stmts
  | FunDecl (name, params, ret_type, body) ->
      let sym = fresh_symbol ctx in
      Hashtbl.replace ctx.name_to_id name sym;
      (* Add function to current scope if not already added by Block's first pass *)
      (match lookup_in_current_scope tbls name with
       | None -> add_symbol tbls name (SymFun (List.map snd params, ret_type))
       | Some _ -> ());
      let param_syms = List.map (fun (n, t) ->
        let sid = fresh_symbol ctx in
        Hashtbl.replace ctx.name_to_id n sid;
        (sid, t)
      ) params in
      let tbls' = push_scope tbls in
      (* Add parameters to function scope, not function itself *)
      List.iter2 (fun (n, t) (_sid, _) -> add_symbol tbls' n (SymVar t)) params param_syms;
      let hbody = analyze_stmt ctx tbls' body in
      HFunDecl (sym, param_syms, ret_type, hbody)
  | Return e ->
      let he, _ = analyze_expr ctx tbls e in
      HReturn he

and analyze_expr (ctx : id_context) (tbls : symbol_table) (expr : Ast.expr) : Hir.hir_expr * value_type =
  match expr with
  | Var x ->
      (match lookup_symbol tbls x with
      | Some (SymVar t) -> (HVar (sym_of_name ctx x, t), t)
      | _ -> raise (Semantic_error ("Undeclared variable: " ^ x)))
  | Int i -> (HInt i, IntType)
  | String s -> (HString s, StringType)
  | Float f -> (HFloat f, FloatType)
  | Bool b -> (HBool b, BoolType)
  | ArrayLit elems ->
      if List.length elems = 0 then raise (Semantic_error "Empty array literals are not allowed");
      let helems, types = List.split (List.map (analyze_expr ctx tbls) elems) in
      let elem_type = List.hd types in
      if not (List.for_all (fun t -> t = elem_type) types) then
        raise (Semantic_error "All array elements must have the same type");
      (HArrayLit (helems, elem_type), ArrayType elem_type)
  | ArrayGet (arr, idx) ->
      let harr, arr_type = analyze_expr ctx tbls arr in
      let hidx, idx_type = analyze_expr ctx tbls idx in
      if idx_type <> IntType then raise (Semantic_error "Array index must be an integer");
            
      (* Enhanced static bounds check for compile-time determinable cases *)
      let check_constant_array _name arr_len idx_val =
        if idx_val < 0 then 
          raise (Semantic_error ("[Semant] Array index out of bounds: negative index " ^ string_of_int idx_val))
        else if idx_val >= arr_len then
          raise (Semantic_error ("[Semant] Array index out of bounds: index " ^ string_of_int idx_val ^ 
                                 " exceeds array length " ^ string_of_int arr_len))
      in

      (* Check bounds based on various cases and track if we've statically verified bounds *)
      let bounds_checked = 
        (* First handle the negative index case separately to avoid control flow warnings *)
        match idx with
        | Int i when i < 0 ->
            (* We can always detect negative indices at compile time *)
            raise (Semantic_error ("[Semant] Array index out of bounds: negative index " ^ string_of_int i))
        | _ ->
            (* Now handle the regular cases *)
            match arr, idx with
            | ArrayLit elems, Int i -> 
                (* For array literals with constant index, we can check at compile time *)
                let arr_len = List.length elems in
                check_constant_array "array literal" arr_len i;
                true (* Bounds checked at compile time *)
                
            | Var _arr_name, Int _i ->
                (* For named arrays, we can't statically determine size without tracking
                   but we can still check for negative indices *)
                false
                
            | _, _ -> 
                (* For variable indices or non-literal arrays, we'll rely on runtime checks *)
                false
      in
      
      (match arr_type with
      | ArrayType elem_type -> (HArrayGet (harr, hidx, elem_type, bounds_checked), elem_type)
      | _ -> raise (Semantic_error "Cannot index non-array type"))
  | ArrayLen arr ->
      let harr, arr_type = analyze_expr ctx tbls arr in
      (match arr_type with
      | ArrayType _ -> (HArrayLen harr, IntType)
      | _ -> raise (Semantic_error "Cannot get length of non-array type"))
  | Binop (op, e1, e2) ->
      let h1, t1 = analyze_expr ctx tbls e1 in
      let h2, t2 = analyze_expr ctx tbls e2 in
      if t1 <> t2 then raise (Semantic_error "Operands must have the same type");
      (match op, t1 with
      | (Add | Sub | Mult | Div | Mod), IntType -> (HBinop (op, h1, h2, IntType), IntType)
      | (Add | Sub | Mult | Div | Mod), FloatType -> (HBinop (op, h1, h2, FloatType), FloatType)
      | (Lt | Leq | Gt | Geq), IntType | (Lt | Leq | Gt | Geq), FloatType -> (HBinop (op, h1, h2, BoolType), BoolType)
      | (Eq | Neq), t when t = IntType || t = FloatType || t = BoolType || t = StringType -> (HBinop (op, h1, h2, BoolType), BoolType)
      | (And | Or), BoolType -> (HBinop (op, h1, h2, BoolType), BoolType)
      | _ -> raise (Semantic_error "Invalid operands for operator"))
  | Unop (op, e) ->
      let he, t = analyze_expr ctx tbls e in
      (match op, t with
      | Neg, IntType -> (HUnop (Neg, he, IntType), IntType)
      | Neg, FloatType -> (HUnop (Neg, he, FloatType), FloatType)
      | Not, BoolType -> (HUnop (Not, he, BoolType), BoolType)
      | _ -> raise (Semantic_error "Invalid operand for unary operator"))
  | FunCall (fname, args) ->
      (match lookup_symbol tbls fname with
      | Some (SymFun (param_types, ret_type)) ->
          if List.length args <> List.length param_types then
            raise (Semantic_error ("Wrong number of arguments for function: " ^ fname));
          let hargs, arg_types = List.split (List.map (analyze_expr ctx tbls) args) in
          if not (List.for_all2 (fun t1 t2 -> t1 = t2) arg_types param_types) then
            raise (Semantic_error ("Type mismatch in function call: " ^ fname));
          (HFunCall (sym_of_name ctx fname, hargs, ret_type), ret_type)
      | _ -> raise (Semantic_error ("Undeclared function: " ^ fname)))
  | CastInt e ->
      let he, t = analyze_expr ctx tbls e in
      (match t with
      | FloatType -> (HCastInt (he, FloatType), IntType)
      | StringType -> raise (Semantic_error "Cannot cast string to int")
      | IntType -> raise (Semantic_error "Redundant cast to int")
      | _ -> raise (Semantic_error "Invalid type for int cast"))
  | CastFloat e ->
      let he, t = analyze_expr ctx tbls e in
      (match t with
      | IntType -> (HCastFloat (he, IntType), FloatType)
      | StringType -> raise (Semantic_error "Cannot cast string to float")
      | FloatType -> raise (Semantic_error "Redundant cast to float")
      | _ -> raise (Semantic_error "Invalid type for float cast"))
  | CastString e ->
      let he, t = analyze_expr ctx tbls e in
      (match t with
      | IntType -> (HCastString (he, IntType), StringType)
      | FloatType -> (HCastString (he, FloatType), StringType)
      | StringType -> raise (Semantic_error "Redundant cast to string")
      | _ -> raise (Semantic_error "Invalid type for string cast"))

(* Top-level semantic analysis entry point *)
let analyze_program (stmt : Ast.stmt) : Hir.hir_stmt =
  let ctx = create_id_context () in
  let initial_scope = Hashtbl.create 16 in
  let tbls = [initial_scope] in
  analyze_stmt ctx tbls stmt