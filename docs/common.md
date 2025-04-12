# Common Components

The common components provide shared functionality and data structures used throughout the compiler. These components are essential for maintaining consistency and reducing code duplication across the frontend and backend.

## Abstract Syntax Tree (`ast.ml`, `ast.mli`)

The AST module defines the structure of the program's abstract syntax tree:

### Key Components
- Expression types (literals, variables, operations)
- Statement types (assignments, control flow)
- Declaration types (variables, functions)
- Program structure

### Features
- Type-safe AST construction
- Pattern matching support
- Visitor pattern implementation
- Pretty printing capabilities

## Type System (`types.ml`, `types.mli`)

The types module defines the language's type system:

### Type Definitions
- Primitive types (int, float, bool, etc.)
- Composite types (arrays, structs)
- Function types
- Type variables for generics

### Type Operations
- Type inference
- Type checking
- Type unification
- Subtyping relationships

## Environment Management (`env.ml`, `env.mli`)

The environment module handles symbol tables and scoping:

### Features
- Symbol table implementation
- Scope management
- Variable lookup
- Type environment
- Function environment

### Key Operations
- Entering/leaving scopes
- Adding/removing symbols
- Looking up symbols
- Checking for shadowing
- Managing nested scopes

## Integration

These common components are used throughout the compiler:

1. **Frontend Usage**
   - AST construction during parsing
   - Type checking in semantic analysis
   - Environment management for scoping

2. **Backend Usage**
   - AST traversal for code generation
   - Type information for optimization
   - Environment data for register allocation

3. **Driver Usage**
   - Error reporting
   - Debug information
   - Program analysis

## Error Handling

The common components include error handling for:
- Type errors
- Scope errors
- AST construction errors
- Environment lookup failures 