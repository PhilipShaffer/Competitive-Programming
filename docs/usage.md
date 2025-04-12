# Usage Guide

This guide explains how to use the compiler, including command-line options and common usage patterns.

## Basic Usage

The basic syntax for using the compiler is:

```bash
dune exec main -- [options] <input_file>
```

Where:
- `input_file` is the source file to compile
- `options` are various compiler flags (see below)

## Command Line Options

The compiler supports the following options:

### Compilation Options
- `-o <output>`: Specify the output file name
- `-t <target>`: Specify the target architecture
- `--show-tokens`: Display token stream during compilation

### Debug Options
- `--debug`: Enable debug mode
- `--verbose`: Show detailed compilation information
- `--dump-ast`: Print the AST after parsing
- `--dump-ir`: Print the intermediate representation

### Optimization Options
- `-O0`: No optimization
- `-O1`: Basic optimization
- `-O2`: Aggressive optimization
- `-O3`: Maximum optimization

## Examples

1. Basic compilation:
```bash
dune exec main -- input.cp
```

2. Compile with specific output name:
```bash
dune exec main -- -o program input.cp
```

3. Compile with debug information:
```bash
dune exec main -- --debug --verbose input.cp
```

4. Compile with optimization:
```bash
dune exec main -- -O2 input.cp
```

## Error Messages

The compiler provides detailed error messages for various issues:

1. **Syntax Errors**
   - Line and column information
   - Expected vs. found tokens
   - Suggestions for fixes

2. **Semantic Errors**
   - Type mismatches
   - Undefined variables
   - Invalid operations

3. **Compilation Errors**
   - Code generation issues
   - Optimization failures
   - Target-specific problems

## Debugging

To debug your program:

1. Enable debug mode:
```bash
dune exec main -- --debug input.cp
```

2. Use the AST printer:
```bash
dune exec main -- --dump-ast input.cp
```

3. Check the intermediate representation:
```bash
dune exec main -- --dump-ir input.cp
```

## Best Practices

1. **Code Organization**
   - Keep source files organized
   - Use meaningful variable names
   - Comment complex logic

2. **Compilation**
   - Start with no optimization (-O0)
   - Enable optimizations gradually
   - Use debug mode during development

3. **Error Handling**
   - Read error messages carefully
   - Use line numbers to locate issues
   - Check for common mistakes

4. **Performance**
   - Profile before optimizing
   - Use appropriate optimization levels
   - Consider target architecture 