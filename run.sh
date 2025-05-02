#!/bin/bash

# Get the test file from command line argument or use default
TEST_FILE=${1:-test/array_assignment.pig}

# Change to project directory
cd "$(dirname "$0")"

# Run full compilation and execution process
echo "Building compiler..."
dune build

echo "Running compiler on $TEST_FILE..."
dune exec compiler "$TEST_FILE"

echo "Compiling generated LLVM IR..."
clang output.ll -o program

echo "Executing program..."
./program

echo "Done!"