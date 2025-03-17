(* Uses Menhir to compile to a proper parser *)

%{
  (* This is the header section where OCaml code can be included.
     It's typically used for imports and helper functions. *)
  open Ast  (* Import the AST module to use its types *)
%}

(* Token declarations section - defines all tokens the lexer will produce *)
(* Each token can optionally have a type, which is the type of value associated with the token *)
%token <int> INT        (* Integer literals, carrying an int value *)
%token <string> ID      (* Identifiers, carrying a string value *)
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
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON

(* End-of-file token *)
%token EOF

(* Precedence and associativity declarations - lower lines have higher precedence *)
(* These declarations help resolve ambiguities in the grammar *)
(* For example, in "a + b * c", * has higher precedence than +, so it binds tighter *)
%nonassoc IN            (* 'in' has lowest precedence *)
%nonassoc ELSE          (* 'else' has low precedence to handle the dangling else problem *)
%left OR                (* 'or' is left-associative: a or b or c = (a or b) or c *)
%left AND               (* 'and' is left-associative and has higher precedence than 'or' *)
%nonassoc LT LEQ GT GEQ EQ NEQ  (* Comparison operators are non-associative *)
%left PLUS MINUS        (* Arithmetic + and - are left-associative *)
%left MULT DIV MOD      (* Arithmetic *, /, % have higher precedence than + and - *)
%right NOT              (* 'not' is right-associative: not not a = not (not a) *)
%nonassoc UMINUS           (* Unary negation has highest precedence *)

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
  | x = ID                       { Var x }                 (* Variable reference *)
  | i = INT                      { Int i }                 (* Integer literal *)
  | b = BOOL                     { Bool b }                (* Boolean literal *)
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
  | x = ID;           ASSIGN; e = expr                    { Assign (x, e) }         (* Assignment: x = e *)
  | LET;    x = ID;   ASSIGN; e = expr;  IN;   s = stmt   { Let (x, e, s) }         (* Let binding: let x = e in s *)
  | IF;     e = expr; THEN;   s1 = stmt; ELSE; s2 = stmt  { If (e, s1, s2) }        (* Conditional: if e then s1 else s2 *)
  | IF;     e = expr; THEN;   s = stmt;                   { If (e, s, Block []) }   (* Conditional: if e then s *)
  | WHILE;  e = expr; DO;     s = stmt                    { While (e, s) }          (* Loop: while e do s *)
  | PRINT;  e = expr                                      { Print e }               (* Print statement: print e *)
  | LBRACE; sl = stmt_list;   RBRACE                      { Block sl }              (* Block: { s1; s2; ...; sn; } *)
  ;

(* Statement list rules - define how sequences of statements are parsed *)
stmt_list:
  | s = stmt;                           { [s] }     (* Single statement *)
  | s = stmt; SEMICOLON; sl = stmt_list { s :: sl } (* Multiple statements: s1; s2; ...; sn; *)
  ;
