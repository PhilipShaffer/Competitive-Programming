%{
  open Ast
%}

// Token declaration
%token <int> INT
%token <string> ID
%token <bool> BOOL
%token LT LEQ GT GEQ EQ NEQ
%token IF THEN ELSE PRINT WHILE IN LET PRINT ASSIGN
%token AND OR NOT NEG
%token PLUS MINUS TIMES DIV MOD
%token LPAREN RPAREN LBRACE RBRACE
%token EOF

// Priorities and associativity
%nonassoc IN
%nonassoc ELSE
%left OR
%left AND
%left LT LEQ GT GEQ EQ NEQ
%left PLUS MINUS
%left TIMES DIV MOD
%right NOT
%nonassoc NEG

// Types
%type

// Start symbol
%start

%%
