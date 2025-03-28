# CompeteX

A programming language designed specifically for competitive programming contests like ICPC and Codeforces.

## Overview

CompeteX is a specialized programming language created to maximize efficiency and productivity when solving competitive programming problems. It combines the best features of languages commonly used in competitions (like C++, Python, and Java) while adding powerful abstractions and built-in algorithms to reduce boilerplate code and development time.

## Compiler Path Configuration

This compiler uses external tools like `llc` and `clang` to generate assembly and executables. You can configure the paths to these tools using environment variables:

1. Copy the example environment file:
   ```
   cp .env.example .env
   ```

2. Edit the `.env` file to match your system configuration:
   ```
   # Path to LLVM's llc compiler
   LLC_PATH=/path/to/your/llc
   
   # Path to clang compiler
   CLANG_PATH=/path/to/your/clang
   ```

If no environment variables are set, the compiler will use these defaults:
- `llc`: `/opt/homebrew/opt/llvm@18/bin/llc` (macOS Homebrew path)
- `clang`: `clang` (system path)

## Usage

To compile your source code:

```
dune exec Competitive_Programming -- [options] your_program.src
```

Options:
- `-o <name>`: Set output file name (default: program)
- `-t <bool>`: Show tokens (default: false)
- `-target <type>`: Target type: llvm-ir, assembly, executable (default: executable)

Examples:
```
# Compile to executable
dune exec Competitive_Programming -- test.while

# Compile to assembly
dune exec Competitive_Programming -- -target assembly test.while

# Compile to LLVM IR with custom output name
dune exec Competitive_Programming -- -target llvm-ir -o my_program test.while
```
