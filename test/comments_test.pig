// This is a single-line comment
arr : int[] := [1, 2, 3, 4, 5]  // Comment at end of line

/* This is a
   multi-line 
   C-style comment */
print "Testing comments"

(* This is an OCaml-style
   multi-line comment *)
print arr[0]  

// Out-of-bounds access - should be caught at compile time
// print arr[5]

// Variable for runtime bounds check
x : int := 3
print arr[x]