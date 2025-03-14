%{
  open Ast
%}

// Token declaration
%token <int> INT
%token <string> ID
%token <bool> BOOL
%token LT LEQ GT GEQ EQ NEQ
%token IF THEN ELSE PRINT WHILE DO IN LET PRINT ASSIGN
%token AND OR NOT NEG
%token PLUS MINUS MULT DIV MOD
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON
%token EOF

// Priorities and associativity
%nonassoc IN
%nonassoc ELSE
%left OR
%left AND
%left LT LEQ GT GEQ EQ NEQ
%left PLUS MINUS
%left MULT DIV MOD
%right NOT
%nonassoc NEG

// Start symbol
%start <Ast.stmt option> main

%%

main:
  | s = stmt; EOF { s } // S -> E#
  ;

expr:
  | x = ID                      { Var x }                 // x
  | i = INT                     { Int i }                 // i
  | b = BOOL                    { Bool b }                // b
  | e1 = expr; PLUS;  e2 = expr { Binop (Add, e1, e2) }   // e1 + e2
  | e1 = expr; MINUS; e2 = expr { Binop (Sub, e1, e2) }   // e1 - e2
  | e1 = expr; MULT;  e2 = expr { Binop (Mult, e1, e2) }  // e1 * e2
  | e1 = expr; DIV;   e2 = expr { Binop (Div, e1, e2) }   // e1 // e2
  | e1 = expr; LT;    e2 = expr { Binop (Lt, e1, e2) }    // e1 < e2
  | e1 = expr; LEQ;   e2 = expr { Binop (Leq, e1, e2) }   // e1 <= e2
  | e1 = expr; GT;    e2 = expr { Binop (Gt, e1, e2) }    // e1 > e2
  | e1 = expr; EQ;    e2 = expr { Binop (Geq, e1, e2) }   // e1 >= e2
  | e1 = expr; NEQ;   e2 = expr { Binop (Neq, e1, e2) }   // e1 != e2
  | e1 = expr; AND;   e2 = expr { Binop (Eq, e1, e2) }    // e1 == e2
  | e1 = expr; OR;    e2 = expr { Binop (Or, e1, e2) }    // e1 or e2
  | e1 = expr; MOD;   e2 = expr { Binop (Mod, e1, e2) }   // e1 % e2
  | NOT; e = expr               { Unop (Not, e) }         // not e
  | NEG; e = expr               { Unop (Neg, e) }         // -e
  | LPAREN; e = expr; RPAREN    { e }                     // (e)
  ;

stmt:
  | x = ID;           ASSIGN; e = expr                    { Assign (x, e) }   // x = e
  | LET;    x = ID;   ASSIGN; e = expr;  IN;   s = stmt   { Let (x, e, s) }   // let x = e in s
  | IF;     e = expr; THEN;   s1 = stmt; ELSE; s2 = stmt  { If (e, s1, s2) }  // if e then s1 else s2
  | WHILE;  e = expr; DO;     s = stmt                    { While (e, s) }    // while e do s
  | PRINT;  e = expr                                      { Print e }         // print e
  | LBRACE; sl = stmt_list;   RBRACE                      { Block sl }        // { s1; s2; ...; sn; }
  ;

stmt_list:
  | s = stmt;                           { [s] }     // s
  | s = stmt; SEMICOLON; sl = stmt_list { s :: sl } // s1; s2; ...; sn;
  ;
