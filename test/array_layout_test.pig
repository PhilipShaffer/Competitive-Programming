// Test program for array layout and bounds checking optimization
arr : int[] := [100, 200, 300, 400, 500]

// This should use compile-time bounds checking (skipping runtime checks)
print arr[0]  // First element
print arr[4]  // Last element

// These use runtime bounds checking
x : int := 2
print arr[x]  // Valid index with variable