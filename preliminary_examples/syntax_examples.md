// Assignments
  x = 3;  // Global scope
  // Should there be a keyword for global scope?

  if () {
    y = 4;  // Local scope
  }

// Standard in and out
  print(x);
  print("Hello, " + "world!\n"); || print("Hello,", "world!");
  print(x + " " + y); || print(x, y); // Should this be allowed?
  println("Hello, " + "world!");
  print("Hello, world!" + x + " " + y); || print("Hello, world! %d %%", x + z, y); || print("Hello, world! ${x + z} ${y}"); // Which one is best?
  
  input(x); // Only reads one input if number of lines to read is not specified
  input(x, y); // Reads 2 inputs
  n = 2;
  input(...n, x); // Reads 2 lines
  // output of reading 2 lines = [[input1, input2],
  //                              [input3, input4, input5]]
  input("%2 %4", x, y); // Reads the next 2 input into x and the next 4 into y
  input("%dx2 %sx4", x, y); // Should we be able to specify the data type of the input?

// If statements
  if (x < 5) {
    print(x);
  }
  else if (x < 10) {  // elif or else if?
    print(x);
  }
  else {
    print(0);
  }

// While loops
  while (x < 10) {
    print(x);
    x = x + 1;
  }

  do {
    print(x);
    x = x + 1;
  } while (x < 10)

// For loops
  for (x = 0; x < 10; x = x + 1) {
    print(x);
  }

// For each loops
  foreach (x in [1, 2, 3]) {
    print(x);
  }

// Functions
  function f(x) {
    return x + 1;
  }

// Structs
  struct Coordinate {
    x: 0,
    y: 0
  }