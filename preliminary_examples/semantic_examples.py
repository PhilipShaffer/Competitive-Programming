### input ##
x = input()        # t : "5",
x,y,z = input()     # x : "3", y : "6", z: "2"


### input_and_convert ###
t = 3
x = input(...t, string)     # x : ["4","8","hello"], converts input to string

x = input(...t, int)     # x : [4,2,1], define type

x = input("#", int)  # Read input as int until the string is read

### output ###
print("Hello: %d", val)


### for ###
foreach(variable, list){

}

for(i, i < 7, i++) {

}


### Match Data ###
Entity PERSON = { age : int, name : string, id : string, allow : bool }
Entity BARS = { ... }


PERSON person1 = (22, "Torben", "xdxa432", true)
list = [entytit, entytit, ...]

match(list) {
    case ( PERSON(age, id) : age >= 18 && id ) {
        person.allow = true
    }
    case ( PERSON(age, id) : age < 18 && id ) {
        wait to 18
    }
    case ( BARS(id) : id == undef) {
        delete
    }
}


match(person in list) {
    case (person.age >= 18 && person.id != undef) {
        person.allow = true
    }
    case (person.age < 18 && person.id != undef) {
        wait to 18
    }
    case (person.id == undef) {
        delete
    }
}


### Dijkstra ###
## vi har deffineret edges_list, matrix, adj_list i sproget.
Edge_list input = input()  # graph
Matrix input = input()
Adj_list input = input()

best_paths = dijkstra(input, source_note, input_type)  
best_paths = dijkstra(input, source_note)              #dicstra ved her hvilken type af graf den arbejder med

graph = input(...m, [int, int, int])  # Read m edges with weights
dist = graph.dijkstra(source)

### Data Types ###
grid = Grid(n, m, 0)
grid[i][j] = value  # Replace value in grid index i,j
grid.neighbours(i, j)   # returns: [top, right, bottom, left]

matrix = input(...n, Matrix, int)    # Reads integer input as type Matrix
matrix = input(...n, Matrix, int, INF?)  # Define value used in input to show null on an index
matrix.toEdgeList()
matrix.toAdjList()
matrix.toMatrix()

### complexity måler ### ###outputter til bruger om de når deres mål
@complexity, time O(log n), space (n).
fucntion sort 


### forskellige parralle functioner der kan køres parrallelt - øger hastigheden betydeligt.
@parallel, concat
function add_two_lists(A, B)

@parallel, add
function add_two_lists(A, B)

@parallel, custom(a+b)
function add_two_lists(A, B)

### QuickSort ###
list.QuickSort(preimplemented_comparators = ASC?, If_Struct_what_property?, CUSTOM_COMPARATOR?) # ? = optional argument