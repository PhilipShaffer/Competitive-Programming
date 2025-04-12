# Frontend Components

The frontend of the compiler is responsible for analyzing the source code and converting it into an intermediate representation. It consists of three main phases: lexical analysis, parsing, and semantic analysis.

## Lexical Analysis (`lexer.mll`)

The lexer is implemented using OCamllex and is responsible for:
- Breaking the source code into tokens
- Removing whitespace and comments
- Identifying keywords, identifiers, literals, and operators
- Handling basic syntax errors

Key features:
- Token recognition for the language's syntax
- Position tracking for error reporting
- Support for comments and whitespace
- Error handling for invalid tokens

## Parsing (`parser.mly`)

The parser is implemented using Menhir and handles:
- Converting the token stream into an Abstract Syntax Tree (AST)
- Enforcing grammar rules
- Building the program's structure
- Reporting syntax errors

The parser defines:
- Grammar rules for the language
- Operator precedence and associativity
- Expression and statement structures
- Error recovery mechanisms

## Semantic Analysis (`semantics.ml`)

The semantic analyzer performs:
- Type checking
- Variable declaration validation
- Scope analysis
- Control flow validation
- Other semantic validations

Key responsibilities:
- Building and maintaining symbol tables
- Type inference and checking
- Checking for undefined variables
- Validating function calls and arguments
- Ensuring program correctness beyond syntax

## Error Handling

The frontend implements comprehensive error handling:
- Lexical errors (invalid tokens)
- Syntax errors (grammar violations)
- Semantic errors (type mismatches, undefined variables)
- All errors include position information for debugging

## Integration

The frontend components work together in sequence:
1. Lexer processes source code into tokens
2. Parser converts tokens into AST
3. Semantic analyzer validates the AST
4. Validated AST is passed to the backend for code generation 