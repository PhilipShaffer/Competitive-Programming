(** Pretty print an AST statement *)
val print_stmt : Common.Ast.stmt -> unit

(** Convert an AST statement to a string *)
val string_of_stmt : Common.Ast.stmt -> string 