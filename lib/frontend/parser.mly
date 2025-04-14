(* Uses Menhir to compile to a proper parser *)

%{
  (* This is the header section where OCaml code can be included.
     It's typically used for imports and helper functions. *)
  open Common.Ast  (* Import the AST module to use its types *)

  (* Helper function to create a location *)
  let mk_loc start_pos end_pos = {
    start_line = start_pos.Lexing.pos_lnum;
    start_col = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol;
    end_line = end_pos.Lexing.pos_lnum;
    end_col = end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol;
  }

  (* Helper function to create an expression with location *)
  let mk_expr e start_pos end_pos = { 
    expr = e; 
    type_info = None;
    loc = mk_loc start_pos end_pos;
  }
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
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON COLON
%token EOF

(* Precedence and associativity declarations - lower lines have higher precedence *)
(* These declarations help resolve ambiguities in the grammar *)
(* For example, in "a + b * c", * has higher precedence than +, so it binds tighter *)
%left OR                (* 'or' is left-associative: a or b or c = (a or b) or c *)
%left AND               (* 'and' is left-associative and has higher precedence than 'or' *)
%nonassoc LT LEQ GT GEQ EQ NEQ  (* Comparison operators are non-associative *)
%left PLUS MINUS        (* Arithmetic + and - are left-associative *)
%left MULT DIV MOD      (* Arithmetic *, /, % have higher precedence than + and - *)
%right NOT              (* 'not' is right-associative: not not a = not (not a) *)
%nonassoc UMINUS        (* Unary negation has highest precedence *)

(* Start symbol declaration - specifies the entry point of the grammar *)
(* The type annotation specifies what type the start symbol produces *)
%start <Common.Ast.stmt> main

(* Grammar rules section - defines the grammar of the language *)
%%

(* The main rule - entry point of the parser *)
main:
  | sl = stmt_list; EOF { Block sl }  (* Parse a statement followed by EOF, return the statement *)
  ;

(* Expression rules - define how expressions are parsed *)
expr:
  | x = ID                       { mk_expr (Var x) $startpos $endpos }
  | i = INT                      { mk_expr (Int i) $startpos $endpos }
  | b = BOOL                     { mk_expr (Bool b) $startpos $endpos }
  | str = STRING                 { mk_expr (String str) $startpos $endpos }
  | f = FLOAT                    { mk_expr (Float f) $startpos $endpos }
  | e1 = expr; PLUS;  e2 = expr  { mk_expr (Binop (Add, e1, e2)) $startpos $endpos }
  | e1 = expr; MINUS; e2 = expr  { mk_expr (Binop (Sub, e1, e2)) $startpos $endpos }
  | e1 = expr; MULT;  e2 = expr  { mk_expr (Binop (Mult, e1, e2)) $startpos $endpos }
  | e1 = expr; DIV;   e2 = expr  { mk_expr (Binop (Div, e1, e2)) $startpos $endpos }
  | e1 = expr; LT;    e2 = expr  { mk_expr (Binop (Lt, e1, e2)) $startpos $endpos }
  | e1 = expr; LEQ;   e2 = expr  { mk_expr (Binop (Leq, e1, e2)) $startpos $endpos }
  | e1 = expr; GT;    e2 = expr  { mk_expr (Binop (Gt, e1, e2)) $startpos $endpos }
  | e1 = expr; GEQ;   e2 = expr  { mk_expr (Binop (Geq, e1, e2)) $startpos $endpos }
  | e1 = expr; EQ;    e2 = expr  { mk_expr (Binop (Eq, e1, e2)) $startpos $endpos }
  | e1 = expr; NEQ;   e2 = expr  { mk_expr (Binop (Neq, e1, e2)) $startpos $endpos }
  | e1 = expr; AND;   e2 = expr  { mk_expr (Binop (And, e1, e2)) $startpos $endpos }
  | e1 = expr; OR;    e2 = expr  { mk_expr (Binop (Or, e1, e2)) $startpos $endpos }
  | e1 = expr; MOD;   e2 = expr  { mk_expr (Binop (Mod, e1, e2)) $startpos $endpos }
  | NOT; e = expr                { mk_expr (Unop (Not, e)) $startpos $endpos }
  | MINUS; e = expr %prec UMINUS { mk_expr (Unop (Neg, e)) $startpos $endpos }
  | LPAREN; e = expr; RPAREN     { e }
  ;

(* Statement rules - define how statements are parsed *)
stmt:
  | x = ID;           ASSIGN; e = expr                    { Assign (x, e) }         (* Assignment: x = e *)
  | x = ID;   COLON;   t = type_expr;     ASSIGN; e = expr { Declare (x, t, e) }  (* Typed let binding: let x: t = e in s *)
  | LET;    x = ID;   ASSIGN; e = expr;  IN;   s = stmt   { Let (x, e, s) }         (* Let binding: let x = e in s *)
  | IF;     e = expr; THEN;   s1 = block_stmt; ELSE; s2 = block_stmt  { If (e, s1, s2) }        (* Conditional: if e then { s1 } else { s2 } *)
  | IF;     e = expr; THEN;   s = block_stmt                    { If (e, s, Block []) }   (* Conditional: if e then { s } *)
  | WHILE;  e = expr; DO;     s = block_stmt                    { While (e, s) }          (* Loop: while e do { s } *)
  | PRINT;  e = expr                                      { Print e }               (* Print statement: print e *)
  | block = block_stmt                                    { block }                 (* Block statement *)
  ;

block_stmt:
  | LBRACE; sl = stmt_list; RBRACE { Block sl }           (* Block: { s1; s2; ...; sn; } *)
  ;

(* Statement list rules - define how sequences of statements are parsed *)
stmt_list:
  | s = stmt;                           { [s] }     (* Single statement *)
  | s = stmt; SEMICOLON?; sl = stmt_list { s :: sl } (* Multiple statements: s1; s2; ...; sn; *)
  ;

type_expr:
  | INTTYPE    { IntType }
  | FLOATTYPE  { FloatType }
  | STRINGTYPE { StringType }
  | BOOLTYPE   { BoolType }
  ;
