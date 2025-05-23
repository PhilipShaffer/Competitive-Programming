// Test file for new syntax without 'then' and 'do'

x: int := 5
y: int := 10

// If statement without 'then'
if x < y {
    print x
} else {
    print y
}

// While loop without 'do'
counter: int := 0
while counter < 3 {
    print counter
    counter := counter + 1
}

// Nested structures
if x > 0 {
    while x > 0 {
        print x
        x := x - 1
    }
}