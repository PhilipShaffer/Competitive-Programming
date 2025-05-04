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

# The memcpy function in LLVM IR may not have the correct alignment.
# This is due to the OCaml LLVM binding does not facilitate the "align" attribute, which is needed for the memcpy function to work correctly in some cases.
# We need to fix the alignment in the generated LLVM IR manually.
echo "Fixing alignment for the memcpy function..."
sed -i '' -E 's/call void @llvm\.memcpy\.p0\.p0\.i64\(ptr ([^,]+), ptr ([^,]+),/call void @llvm.memcpy.p0.p0.i64(ptr align 8 \1, ptr align 8 \2,/' output.ll

echo "Compiling generated LLVM IR..."
clang output.ll -o program

echo "Executing program..."
./program

echo "Done!"