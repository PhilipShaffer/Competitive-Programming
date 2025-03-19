
### Phil & Mikkel ###

### Variable Types ###               
               
int x = 10
float x = 10.0

### If ###
if (x > 0) { 
    print("Positive") 
    }

### While ###
while (1) {
    print(2)
}

### For Loops ### 
for (i in range(1, n)) { 
    ... 
    } 

for (item in items) {
     ... 
     }

### Functions ###
func print(x)  {
    print(x)
}

func minmax(array(int) arr) int {
    // Returns maximum
}

func minmax(array(int) arr) -> (int, int) {
    // Returns minimum and maximum
}

(int min, int max) = minmax(arr)

### Arrays ###
hashArray of int x = [1, 2, 3, 4, 5]
array[5] of int x = [1, 2, 3, 4, 5]

array of string y = ["a", "b", "c", "d", "e"]

array(int) x = [1, 2, 3, 4, 5]
array[5](int) x = [1, 2, 3, 4, 5]

array.sort

array(int) a = [1, 2, 3]
array(int) b = [4, 5, 6]
array(int) c = a + b                # Vector addition: [5, 7, 9]
int dot = a * b                     # Dot product: 32


array(int) evens = [for x in range(1, n) if x % 2 == 0, x]


### Data Structures ###

struct Person { name: string, age: int }
enum Color { RED, GREEN, BLUE }
map(string, int) scores = {"Alice": 95, "Bob": 87}

switch (x) { 
             case 1: ... 
             case 2: ... 
             default: ... 
            }


### Read ###
int x = input()

string x = input(...t)          # x : ["4","8","hello"], converts input to string

int x = input(...t)             # x : [4,2,1], define type

int x = input("#")              # Read input as int until the string is read

### Print ###
print(x, y, "hello")            # Print values separated by space
println(x, y, z)          # Print values and add newline

### Lamda ###
func(int a, int b) -> int = (a, b) => a + b

add = lambda(x, y -> x + y)

if (x = true and b = true) if (x = true or  b = true)
