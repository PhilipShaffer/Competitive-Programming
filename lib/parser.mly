(* Uses Menhir to compile to a proper parser *)

%{
  (* This is the header section where OCaml code can be included.
     It's typically used for imports and helper functions. *)
  open Ast  (* Import the AST module to use its types *)
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
%token IF THEN ELSE PRINT WHILE DO LET IN ASSIGN

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
  | x = ID                       { Var x }                 (* Variable reference *)
  | i = INT                      { Int i }                 (* Integer literal *)
  | b = BOOL                     { Bool b }                (* Boolean literal *)
  | str = STRING                 { String str }
  | f = FLOAT                    { Float f }
  | e1 = expr; PLUS;  e2 = expr  { Binop (Add, e1, e2) }   (* Addition: e1 + e2 *)
  | e1 = expr; MINUS; e2 = expr  { Binop (Sub, e1, e2) }   (* Subtraction: e1 - e2 *)
  | e1 = expr; MULT;  e2 = expr  { Binop (Mult, e1, e2) }  (* Multiplication: e1 * e2 *)
  | e1 = expr; DIV;   e2 = expr  { Binop (Div, e1, e2) }   (* Division: e1 / e2 *)
  | e1 = expr; LT;    e2 = expr  { Binop (Lt, e1, e2) }    (* Less than: e1 < e2 *)
  | e1 = expr; LEQ;   e2 = expr  { Binop (Leq, e1, e2) }   (* Less than or equal: e1 <= e2 *)
  | e1 = expr; GT;    e2 = expr  { Binop (Gt, e1, e2) }    (* Greater than: e1 > e2 *)
  | e1 = expr; GEQ;   e2 = expr  { Binop (Geq, e1, e2) }   (* Greater than or equal: e1 >= e2 *)
  | e1 = expr; EQ;    e2 = expr  { Binop (Eq, e1, e2) }    (* Equality: e1 == e2 *)
  | e1 = expr; NEQ;   e2 = expr  { Binop (Neq, e1, e2) }   (* Inequality: e1 != e2 *)
  | e1 = expr; AND;   e2 = expr  { Binop (And, e1, e2) }   (* Logical AND: e1 and e2 *)
  | e1 = expr; OR;    e2 = expr  { Binop (Or, e1, e2) }    (* Logical OR: e1 or e2 *)
  | e1 = expr; MOD;   e2 = expr  { Binop (Mod, e1, e2) }   (* Modulo: e1 % e2 *)
  | NOT; e = expr                { Unop (Not, e) }         (* Logical NOT: not e *)
  | MINUS; e = expr %prec UMINUS { Unop (Neg, e) }         (* Unary negation: -e *)
  | LPAREN; e = expr; RPAREN     { e }                     (* Parenthesized expression: (e) *)
  ;

(* Statement rules - define how statements are parsed *)
stmt:
  | id = ID; LPAREN; params = param_list; RPAREN; ARROW; ret_type = type_expr; ASSIGN; LBRACE; body = stmt_list; RBRACE { FunDecl(id, params, ret_type, Block body) }  (* Function declaration *)
  | RETURN; e = expr { Return e }  (* Return statement *)
  | x = ID;           ASSIGN; e = expr                    { Assign (x, e) }         (* Assignment: x = e *)
  | x = ID;   COLON;   t = type_expr;     ASSIGN; e = expr { Declare (x, t, e) }  (* Typed let binding: let x: t = e in s *)
  | LET;    x = ID;   ASSIGN; e = expr;  IN;   s = stmt   { Let (x, e, s) }         (* Let binding: let x = e in s *)
  | IF; e = expr; THEN; LBRACE; s1 = stmt_list; RBRACE; ELSE; LBRACE; s2 = stmt_list; RBRACE { If (e, Block s1, Block s2) }  (* Conditional: if e then { ... } else { ... } *)
  | IF; e = expr; THEN; LBRACE; s = stmt_list; RBRACE { If (e, Block s, Block []) }  (* Conditional: if e then { ... } *)
  | WHILE;  e = expr; DO;     s = stmt                    { While (e, s) }          (* Loop: while e do s *)
  | PRINT;  e = expr                                      { Print e }               (* Print statement: print e *)
  | LBRACE; sl = stmt_list;   RBRACE                      { Block sl }              (* Block: { s1; s2; ...; sn; } *)
  ;

(* Statement list rules - define how sequences of statements are parsed *)
stmt_list:
  | s = stmt;                           { [s] }     (* Single statement *)
  | s = stmt; SEMICOLON?; sl = stmt_list { s :: sl } (* Multiple statements: s1; s2; ...; sn; with optional semicolons *)
  ;

type_expr:
  | INTTYPE    { IntType }
  | FLOATTYPE  { FloatType }
  | STRINGTYPE { StringType }
  | BOOLTYPE   { BoolType }
  | VOIDTYPE   { VoidType }
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
