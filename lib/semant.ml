(* Semantic analysis for the strict While language *)

open Ast
open Hir

exception Semantic_error of string

(* Symbol table entry with optional array size *)
type symbol_info =
  | SymVar of value_type * int option (* Type and optional array size for arrays *)
  | SymFun of (value_type list) * value_type

(* Symbol table with scope stack *)
type symbol_table = (string, symbol_info) Hashtbl.t list

(* Generate fresh unique symbol IDs *)
let symbol_counter = ref 0
let fresh_symbol () =
  let id = !symbol_counter in
  incr symbol_counter; id

(* Helper: get symbol ID for a name (assumes unique for now) *)
let sym_table_ids = Hashtbl.create 256
let sym_of_name name =
  try Hashtbl.find sym_table_ids name with Not_found -> let sid = fresh_symbol () in Hashtbl.add sym_table_ids name sid; sid

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
let rec analyze_stmt (tbls : symbol_table) (stmt : Ast.stmt) : Hir.hir_stmt =
  match stmt with
  | Assign (x, e) ->
      let sym = match lookup_symbol tbls x with Some (SymVar (_, _)) -> sym_of_name x | _ -> raise (Semantic_error ("Undeclared variable: " ^ x)) in
      let he, t = analyze_expr tbls e in
      (* Strict typing: must match exactly *)
      (match lookup_symbol tbls x with 
       | Some (SymVar (t', _)) when t = t' -> () 
       | _ -> raise (Semantic_error ("Type mismatch in assignment to: " ^ x)));
      HAssign (sym, he)
  | ArrayAssign (arr, idx, value) ->
      let harr, arr_type = analyze_expr tbls arr in
      let hidx, idx_type = analyze_expr tbls idx in
      let hval, val_type = analyze_expr tbls value in
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
            | Var arr_name, Int i ->
                (match lookup_symbol tbls arr_name with
                 | Some (SymVar (ArrayType _, Some size)) ->
                     check_constant_array arr_name size i;
                     true (* Bounds checked at compile time *)
                 | _ -> false)
            | _, _ -> 
                false (* Need runtime bounds checking *)
      in

      (* Type checking *)
      (match arr_type with
      | ArrayType elem_type when elem_type = val_type -> HArrayAssign (harr, hidx, hval, bounds_checked)
      | ArrayType _ -> raise (Semantic_error "Type mismatch in array assignment")
      | _ -> raise (Semantic_error "Cannot index non-array type"))
  | ArrayPut (arr, value) ->
      let harr, arr_type = analyze_expr tbls arr in
      let hval, val_type = analyze_expr tbls value in
      (match arr_type, arr with
      | ArrayType elem_type, Var arr_name when elem_type = val_type -> 
          (* Update the array size in the symbol table when using put *)
          update_array_size tbls arr_name 1;
          HArrayPut (harr, hval)
      | ArrayType elem_type, _ when elem_type = val_type -> 
          (* For non-variable arrays (e.g., array literals), we can't update the symbol table *)
          HArrayPut (harr, hval)
      | ArrayType _, _ -> raise (Semantic_error "Type mismatch in put statement")
      | _ -> raise (Semantic_error "Cannot put to non-array type"))
  | ArrayPop arr ->
      let harr, arr_type = analyze_expr tbls arr in
      (match arr_type, arr with
      | ArrayType _, Var arr_name ->
          (* Update the array size in the symbol table when using pop *)
          update_array_size tbls arr_name (-1);
          HArrayPop harr
      | ArrayType _, _ -> 
          (* For non-variable arrays, we can't update the symbol table *)
          HArrayPop harr
      | _ -> raise (Semantic_error "Cannot pop from non-array type"))
  | Declare (x, t, e) ->
      let sym = fresh_symbol () in
      Hashtbl.replace sym_table_ids x sym;
      let he, et = analyze_expr tbls e in
      if t <> et then raise (Semantic_error ("Type mismatch in declaration of: " ^ x));
      
      (* Track array size for arrays initialized with literals *)
      let array_size = 
        match t, e with
        | ArrayType _, ArrayLit elems -> Some (List.length elems)
        | _ -> None 
      in
      
      add_symbol tbls x (SymVar (t, array_size));
      HDeclare (sym, t, he)
  (* Let binding removed as it's no longer needed *)
  | If (cond, st, sf) ->
      let hc, t = analyze_expr tbls cond in
      if t <> BoolType then raise (Semantic_error "Condition must be bool");
      let hst = analyze_stmt (push_scope tbls) st in
      let hsf = analyze_stmt (push_scope tbls) sf in
      HIf (hc, hst, hsf)
  | While (cond, body) ->
      let hc, t = analyze_expr tbls cond in
      if t <> BoolType then raise (Semantic_error "While condition must be bool");
      let hbody = analyze_stmt (push_scope tbls) body in
      HWhile (hc, hbody)
  | Print e ->
      let he, _ = analyze_expr tbls e in
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
      let hir_stmts = List.map (analyze_stmt tbls') sl in
      HBlock hir_stmts
  | FunDecl (name, params, ret_type, body) ->
      let sym = fresh_symbol () in
      Hashtbl.replace sym_table_ids name sym;
      let param_syms = List.map (fun (n, t) ->
        let sid = fresh_symbol () in
        Hashtbl.replace sym_table_ids n sid;
        (sid, t)
      ) params in
      let tbls' = push_scope tbls in
      List.iter2 (fun (n, t) (_sid, _) -> add_symbol tbls' n (SymVar (t, None))) params param_syms;
      add_symbol tbls' name (SymFun (List.map snd params, ret_type));
      let hbody = analyze_stmt tbls' body in
      HFunDecl (sym, param_syms, ret_type, hbody)
  | Return e ->
      let he, _ = analyze_expr tbls e in
      HReturn he

and analyze_expr (tbls : symbol_table) (expr : Ast.expr) : Hir.hir_expr * value_type =
  match expr with
  | Var x ->
      (match lookup_symbol tbls x with
      | Some (SymVar (t, _)) -> (HVar (sym_of_name x, t), t)
      | _ -> raise (Semantic_error ("Undeclared variable: " ^ x)))
  | Int i -> (HInt i, IntType)
  | String s -> (HString s, StringType)
  | Float f -> (HFloat f, FloatType)
  | Bool b -> (HBool b, BoolType)
  | ArrayLit elems ->
      if List.length elems = 0 then raise (Semantic_error "Empty array literals are not allowed");
      let helems, types = List.split (List.map (analyze_expr tbls) elems) in
      let elem_type = List.hd types in
      if not (List.for_all (fun t -> t = elem_type) types) then
        raise (Semantic_error "All array elements must have the same type");
      (HArrayLit (helems, elem_type), ArrayType elem_type)
  | ArrayGet (arr, idx) ->
      let harr, arr_type = analyze_expr tbls arr in
      let hidx, idx_type = analyze_expr tbls idx in
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
                
            | Var arr_name, Int i ->
                (* For named arrays with constant index, use our tracked array size *)
                (match lookup_symbol tbls arr_name with
                | Some (SymVar (ArrayType _, Some size)) ->
                    (* We have the array size stored in the symbol table *)
                    check_constant_array arr_name size i;
                    true (* Bounds checked at compile time *)
                | _ -> false)
                
            | _, _ -> 
                (* For variable indices or non-literal arrays, we'll rely on runtime checks *)
                false
      in
      
      (match arr_type with
      | ArrayType elem_type -> (HArrayGet (harr, hidx, elem_type, bounds_checked), elem_type)
      | _ -> raise (Semantic_error "Cannot index non-array type"))
  | ArrayLen arr ->
      let harr, arr_type = analyze_expr tbls arr in
      (match arr_type with
      | ArrayType _ -> (HArrayLen harr, IntType)
      | _ -> raise (Semantic_error "Cannot get length of non-array type"))
  | Binop (op, e1, e2) ->
      let h1, t1 = analyze_expr tbls e1 in
      let h2, t2 = analyze_expr tbls e2 in
      if t1 <> t2 then raise (Semantic_error "Operands must have the same type");
      (match op, t1 with
      | (Add | Sub | Mult | Div | Mod), IntType -> (HBinop (op, h1, h2, IntType), IntType)
      | (Add | Sub | Mult | Div | Mod), FloatType -> (HBinop (op, h1, h2, FloatType), FloatType)
      | (Lt | Leq | Gt | Geq), IntType | (Lt | Leq | Gt | Geq), FloatType -> (HBinop (op, h1, h2, BoolType), BoolType)
      | (Eq | Neq), t when t = IntType || t = FloatType || t = BoolType || t = StringType -> (HBinop (op, h1, h2, BoolType), BoolType)
      | (And | Or), BoolType -> (HBinop (op, h1, h2, BoolType), BoolType)
      | _ -> raise (Semantic_error "Invalid operands for operator"))
  | Unop (op, e) ->
      let he, t = analyze_expr tbls e in
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
          let hargs, arg_types = List.split (List.map (analyze_expr tbls) args) in
          if not (List.for_all2 (fun t1 t2 -> t1 = t2) arg_types param_types) then
            raise (Semantic_error ("Type mismatch in function call: " ^ fname));
          (HFunCall (sym_of_name fname, hargs, ret_type), ret_type)
      | _ -> raise (Semantic_error ("Undeclared function: " ^ fname)))
