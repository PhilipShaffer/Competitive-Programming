# Architecture Overview

The compiler follows a traditional multi-phase architecture, with clear separation between frontend and backend components. Here's a high-level overview of how the different components work together:

## Compilation Pipeline

1. **Driver Layer** (`lib/driver/`)
   - Entry point for the compiler
   - Handles command-line options and orchestration
   - Coordinates the compilation process
   - Provides error handling and reporting

2. **Frontend** (`lib/frontend/`)
   - **Lexical Analysis** (`lexer.mll`): Converts source code into tokens
   - **Parsing** (`parser.mly`): Converts tokens into an Abstract Syntax Tree (AST)
   - **Semantic Analysis** (`semantics.ml`): Performs type checking and other semantic validations

3. **Backend** (`lib/backend/`)
   - **Code Generation** (`codegen.ml`): Converts the AST into target code
   - Handles optimization and target-specific code generation

4. **Common Components** (`lib/common/`)
   - Shared data structures and utilities
   - AST definitions
   - Type system
   - Environment management

## Data Flow

1. Source code is read and passed to the driver
2. Driver invokes the frontend components in sequence:
   - Lexer → Parser → Semantic Analyzer
3. If frontend processing succeeds, the backend is invoked:
   - Code generator processes the validated AST
4. The resulting code is either executed or saved to a file

## Error Handling

The compiler implements a comprehensive error handling system:
- Parse errors (syntax issues)
- Semantic errors (type checking, etc.)
- Compilation errors (code generation issues)
- Runtime errors (during execution)

Each error type is properly propagated through the system and reported to the user with meaningful messages. 