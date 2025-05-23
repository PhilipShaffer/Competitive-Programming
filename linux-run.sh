#!/bin/bash

# Get the test file from command line argument or use default
TEST_FILE=${1:-test/user_test.pig}

# Change to project directory
cd "$(dirname "$0")"

# Run full compilation and execution process
echo "Building compiler..."
dune build || { echo "Build failed! Exiting."; exit 1; }

echo "Running compiler on $TEST_FILE..."
dune exec compiler "$TEST_FILE" || { echo "Compiler execution failed! Exiting."; exit 1; }

echo "Compiling generated LLVM IR..."
clang output.ll -o program || { echo "LLVM IR compilation failed! Exiting."; exit 1; }

echo "Executing program..."
./program || { echo "Program execution failed! Exiting."; exit 1; }

echo "Done!"