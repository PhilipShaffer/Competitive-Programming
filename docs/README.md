# Competitive Programming Language Compiler

This is a compiler for a custom programming language designed for competitive programming. The compiler is implemented in OCaml and follows a traditional compiler architecture with frontend, backend, and common components.

## Project Structure

The project is organized into several main components:

1. **Driver** (`lib/driver/`): The main entry point and orchestration layer
2. **Frontend** (`lib/frontend/`): Lexical analysis, parsing, and semantic analysis
3. **Backend** (`lib/backend/`): Code generation and optimization
4. **Common** (`lib/common/`): Shared types and utilities

## Documentation

- [Architecture Overview](architecture.md): High-level overview of the compiler architecture
- [Frontend](frontend.md): Details about lexical analysis, parsing, and semantic analysis
- [Backend](backend.md): Information about code generation and optimization
- [Common Components](common.md): Documentation of shared types and utilities
- [Usage Guide](usage.md): How to use the compiler

## Building and Running

The project uses Dune as its build system. To build and run:

```bash
dune build
dune exec main -- [options] <input_file>
```

For more details on usage and available options, see the [Usage Guide](usage.md). 