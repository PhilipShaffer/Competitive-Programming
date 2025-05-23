(* Uses Menhir to compile to a proper parser *)

%{
  (* This is the header section where OCaml code can be included.
     It's typically used for imports and helper functions. *)
  open Ast  (* Import the AST module to use its types *)
  open Parse_error  (* Import parse error handling *)
%}

%token INTTYPE FLOATTYPE STRINGTYPE BOOLTYPE

%token <string> ID      (* Identifiers, carrying a string value *)
%token <string> STRING
%token <int> INT       (* Integer literals, carrying an int value *)
%token <float> FLOAT
%token <bool> BOOL      (* Boolean literals, carrying a bool value *)

(* Tokens for comparison operators *)
%token LT LEQ GT GEQ EQ NEQ

(* Tokens for keywords *)
%token IF ELSE PRINT WHILE ASSIGN

(* Tokens for logical operators *)
%token AND OR NOT

(* Tokens for arithmetic operators *)
%token PLUS MINUS MULT DIV MOD

(* Tokens for delimiters *)
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON COLON COMMA
%token ARROW
%token RETURN
%token VOIDTYPE
%token EOF

(* Array-related tokens *)
%token LBRACKET RBRACKET
%token PUT POP LEN

(* Precedence and associativity declarations - lower lines have higher precedence *)
(* These declarations help resolve ambiguities in the grammar *)
(* For example, in "a + b * c", * has higher precedence than +, so it binds tighter *)
(* NOTE: One shift/reduce conflict may remain due to grammar structure. See Menhir warnings for details. *)
(* Removed unused precedence for IN and ELSE as they were never useful *)
%left OR                (* 'or' is left-associative: a or b or c = (a or b) or c *)
%left AND               (* 'and' is left-associative and has higher precedence than 'or' *)
%nonassoc LT LEQ GT GEQ EQ NEQ  (* Comparison operators are non-associative *)
%left PLUS MINUS        (* Arithmetic + and - are left-associative *)
%left MULT DIV MOD      (* Arithmetic *, /, % have higher precedence than + and - *)
%right NOT              (* 'not' is right-associative: not not a = not (not a) *)
%nonassoc UMINUS        (* Unary negation has highest precedence *)

(* Start symbol declaration - specifies the entry point of the grammar *)
(* The type annotation specifies what type the start symbol produces *)
%start <Ast.stmt> main

(* Grammar rules section - defines the grammar of the language *)
%%

(* The main rule - entry point of the parser *)
main:
  | sl = stmt_list; EOF { Block sl }  (* Parse a statement followed by EOF, return the statement *)
  ;

(* Expression rules - define how expressions are parsed *)
expr:
  | x = ID; LPAREN; args = arg_list; RPAREN { FunCall(x, args) }  (* Function call: f(a, b, ...) *)
  | ID; LPAREN; error { 
      let start_pos = $startpos($3) in
      let end_pos = $endpos($3) in
      raise_parse_error "Invalid function call syntax. Expected arguments followed by ')'" start_pos end_pos
    }
  | x = ID                       { Var x }                 (* Variable reference *)
  | i = INT                      { Int i }                 (* Integer literal *)
  | b = BOOL                     { Bool b }                (* Boolean literal *)
  | str = STRING                 { String str }            (* String literal *)
  | f = FLOAT                    { Float f }               (* Float literal *)
  | e1 = expr; PLUS;  e2 = expr  { Binop (Add, e1, e2) }   (* Addition: e1 + e2 *)
  | e1 = expr; MINUS; e2 = expr  { Binop (Sub, e1, e2) }   (* Subtraction: e1 - e2 *)
  | e1 = expr; MULT;  e2 = expr  { Binop (Mult, e1, e2) }  (* Multiplication: e1 * e2 *)
  | e1 = expr; DIV;   e2 = expr  { Binop (Div, e1, e2) }   (* Division: e1 / e2 *)
  | e1 = expr; LT;    e2 = expr  { Binop (Lt, e1, e2) }    (* Less than: e1 < e2 *)
  | e1 = expr; LEQ;   e2 = expr  { Binop (Leq, e1, e2) }   (* Less than or equal: e1 <= e2 *)
  | e1 = expr; GT;    e2 = expr  { Binop (Gt, e1, e2) }    (* Greater than: e1 > e2 *)
  | e1 = expr; GEQ;   e2 = expr  { Binop (Geq, e1, e2) }   (* Greater than or equal: e1 >= e2 *)
  | e1 = expr; EQ;    e2 = expr  { Binop (Eq, e1, e2) }    (* Equality: e1 == e2 *)
  | expr; EQ; EQ; error {
      let start_pos = $startpos($3) in
      let end_pos = $endpos($3) in
      raise_parse_error "Invalid comparison. Did you mean '=' instead of '=='?" start_pos end_pos
    }
  | e1 = expr; NEQ;   e2 = expr  { Binop (Neq, e1, e2) }   (* Inequality: e1 != e2 *)
  | e1 = expr; AND;   e2 = expr  { Binop (And, e1, e2) }   (* Logical AND: e1 and e2 *)
  | e1 = expr; OR;    e2 = expr  { Binop (Or, e1, e2) }    (* Logical OR: e1 or e2 *)
  | e1 = expr; MOD;   e2 = expr  { Binop (Mod, e1, e2) }   (* Modulo: e1 % e2 *)
  | NOT; e = expr                { Unop (Not, e) }         (* Logical NOT: not e *)
  | MINUS; e = expr %prec UMINUS { Unop (Neg, e) }         (* Unary negation: -e *)
  | LPAREN; e = expr; RPAREN     { e }                     (* Parenthesized expression: (e) *)
  | LPAREN; error {
      let start_pos = $startpos($2) in
      let end_pos = $endpos($2) in
      raise_parse_error "Invalid parenthesized expression. Expected an expression inside ( )" start_pos end_pos
    }
  | LPAREN; expr; error {
      let start_pos = $startpos($3) in
      let end_pos = $endpos($3) in
      raise_parse_error "Unclosed parenthesis. Expected ')' after expression" start_pos end_pos
    }
  | LBRACKET; elems = separated_list(COMMA, expr); RBRACKET { ArrayLit elems }     (* Array literal *)
  | LBRACKET; error {
      let start_pos = $startpos($2) in
      let end_pos = $endpos($2) in
      raise_parse_error "Invalid array literal. Expected expressions separated by commas inside [ ]" start_pos end_pos
    }
  | arr = expr; LBRACKET; idx = expr; RBRACKET { ArrayGet(arr, idx) }              (* Array access *)
  | expr; LBRACKET; error {
      let start_pos = $startpos($3) in
      let end_pos = $endpos($3) in
      raise_parse_error "Invalid array access. Expected an index expression inside [ ]" start_pos end_pos
    }
  | LEN; LPAREN; arr = expr; RPAREN       { ArrayLen arr }    (* Array length *)
  | LEN; LPAREN; error {
      let start_pos = $startpos($3) in
      let end_pos = $endpos($3) in
      raise_parse_error "Invalid len syntax. Expected: len(array)" start_pos end_pos
    }
  | INTTYPE; LPAREN; e = expr; RPAREN     { CastInt e }         (* Type cast to int: int(e) *)
  | FLOATTYPE; LPAREN; e = expr; RPAREN   { CastFloat e }       (* Type cast to float: float(e) *)
  | STRINGTYPE; LPAREN; e = expr; RPAREN  { CastString e }      (* Type cast to string: string(e) *)
  | INTTYPE; LPAREN; error {
      let start_pos = $startpos($3) in
      let end_pos = $endpos($3) in
      raise_parse_error "Invalid type cast. Expected an expression inside int( )" start_pos end_pos
    }
  | FLOATTYPE; LPAREN; error {
      let start_pos = $startpos($3) in
      let end_pos = $endpos($3) in
      raise_parse_error "Invalid type cast. Expected an expression inside float( )" start_pos end_pos
    }
  | STRINGTYPE; LPAREN; error {
      let start_pos = $startpos($3) in
      let end_pos = $endpos($3) in
      raise_parse_error "Invalid type cast. Expected an expression inside string( )" start_pos end_pos
    }
  ;

(* Statement rules - define how statements are parsed *)
stmt:
  | id = ID; LPAREN; params = param_list; RPAREN; ARROW; ret_type = type_expr; ASSIGN; LBRACE; body = stmt_list; RBRACE { FunDecl(id, params, ret_type, Block body) }  (* Function declaration *)
  | ID; LPAREN; param_list; RPAREN; error {
      let start_pos = $startpos($5) in
      let end_pos = $endpos($5) in
      raise_parse_error "Invalid function declaration. Expected '->' followed by return type after parameters" start_pos end_pos
    }
  | RETURN; e = expr { Return e }  (* Return statement *)
  | RETURN; error {
      let start_pos = $startpos($2) in
      let end_pos = $endpos($2) in
      raise_parse_error "Invalid return statement. Expected an expression after 'return'" start_pos end_pos
    }
  | arr = expr; LBRACKET; idx = expr; RBRACKET; ASSIGN; value = expr { ArrayAssign(arr, idx, value) }  (* Array assignment: arr[idx] := value *)
  | expr; LBRACKET; expr; RBRACKET; error {
      let start_pos = $startpos($5) in
      let end_pos = $endpos($5) in
      raise_parse_error "Invalid array assignment. Expected ':=' after array access" start_pos end_pos
    }
  | PUT; LPAREN; arr = expr; COMMA; value = expr; RPAREN  { ArrayPut(arr, value) }   (* Array put: put(arr, value) *)
  | PUT; LPAREN; error {
      let start_pos = $startpos($3) in
      let end_pos = $endpos($3) in
      raise_parse_error "Invalid put syntax. Expected: put(array, value)" start_pos end_pos
    }
  | POP; LPAREN; arr = expr; RPAREN                       { ArrayPop arr }           (* Array pop: pop(arr) *)
  | POP; LPAREN; error {
      let start_pos = $startpos($3) in
      let end_pos = $endpos($3) in
      raise_parse_error "Invalid pop syntax. Expected: pop(array)" start_pos end_pos
    }
  | x = ID;           ASSIGN; e = expr                    { Assign (x, e) }          (* Assignment: x := e *)
  | ID;           EQ; error { 
      let start_pos = $startpos($2) in
      let end_pos = $endpos($2) in
      raise_parse_error "Invalid assignment. Did you mean ':=' instead of '='?" start_pos end_pos
    }
  | x = ID;   COLON;   t = type_expr;    ASSIGN; e = expr { Declare (x, t, e) }      (* Typed variable declaration: x: t := e *)
  | ID;   COLON;   error { 
      let start_pos = $startpos($3) in
      let end_pos = $endpos($3) in
      raise_parse_error "Invalid type in variable declaration. Expected a valid type after ':'" start_pos end_pos
    }
  | IF; e = expr; LBRACE; s1 = stmt_list; RBRACE; ELSE; LBRACE; s2 = stmt_list; RBRACE { If (e, Block s1, Block s2) }  (* Conditional: if e { ... } else { ... } *)
  | IF; e = expr; LBRACE; s = stmt_list; RBRACE     { If (e, Block s, Block []) }                (* Conditional: if e { ... } *)
  | IF; error {
      let start_pos = $startpos($2) in
      let end_pos = $endpos($2) in
      raise_parse_error "Invalid if statement. Expected: if <condition> { ... }" start_pos end_pos
    }
  | IF; expr; error {
      let start_pos = $startpos($3) in
      let end_pos = $endpos($3) in
      raise_parse_error "Invalid if statement. Expected '{' after condition" start_pos end_pos
    }
  | WHILE;  e = expr; s = stmt                    { While (e, s) }           (* Loop: while e s *)
  | WHILE; error {
      let start_pos = $startpos($2) in
      let end_pos = $endpos($2) in
      raise_parse_error "Invalid while loop. Expected: while <condition> <statement>" start_pos end_pos
    }
  | WHILE; expr; error {
      let start_pos = $startpos($3) in
      let end_pos = $endpos($3) in
      raise_parse_error "Invalid while loop. Expected statement or '{' after condition" start_pos end_pos
    }
  | PRINT;  e = expr                                      { Print e }                (* Print statement: print e *)
  | PRINT; error {
      let start_pos = $startpos($2) in
      let end_pos = $endpos($2) in
      raise_parse_error "Invalid print statement. Expected an expression after 'print'" start_pos end_pos
    }
  | LBRACE; sl = stmt_list;   RBRACE                      { Block sl }               (* Block: { s1; s2; ...; sn; } *)
  | LBRACE; error {
      let start_pos = $startpos($2) in
      let end_pos = $endpos($2) in
      raise_parse_error "Invalid block. Expected statements inside { }" start_pos end_pos
    }
  ;

(* Statement list rules - define how sequences of statements are parsed *)
stmt_list:
  | s = stmt;                           { [s] }      (* Single statement *)
  | s = stmt; SEMICOLON?; sl = stmt_list { s :: sl } (* Multiple statements: s1; s2; ...; sn; with optional semicolons *)
  ;

type_expr:
  | INTTYPE    { IntType }
  | FLOATTYPE  { FloatType }
  | STRINGTYPE { StringType }
  | BOOLTYPE   { BoolType }
  | VOIDTYPE   { VoidType }
  | t = type_expr; LBRACKET; RBRACKET { ArrayType t }
  | error { 
      let start_pos = $symbolstartpos in
      let end_pos = $endpos in
      raise_parse_error "Invalid type expression. Expected 'int', 'float', 'string', 'bool', 'void', or array type (e.g., 'int[]')" start_pos end_pos
    }
  ;

param_list:
  |  { [] }
  | p = param { [p] }
  | p = param; COMMA; ps = param_list { p :: ps }
  ;

param:
  | x = ID; COLON; t = type_expr { (x, t) }
  ;

arg_list:
  |  { [] }
  | e = expr { [e] }
  | e = expr; COMMA; es = arg_list { e :: es }
  ;
