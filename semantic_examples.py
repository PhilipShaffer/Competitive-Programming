### input ##
x = input()        # t : "5",
x,y,z = input()     # x : "3", y : "6", z: "2"


### input_and_convert ###
t = 3
x = input(...t)     # x : ["4","8","1"]

x = input(...t, int)     # x : [4,2,1]

### output ###
print("Hello: %d", val)


### for ###
foreach(variable, list){

}

for(i, i < 7, i++) {

}


### Match Data ###
entytit PERSON = { age : int, name : string, id : string, allow : bool }
entytit BARS = { ... }


PERSON person1 = (22, "Torben", "xdxa432", true)
list = [entytit, entytit, ...]

match(list) {
    case ( PERSON(age, id) : age >= 18 && id ) {
        person.allow = true
    }
    case ( PERSON(age, id) : age < 18 && id ) {
        wait to 18
    }
    case ( PERSON(id) : id == undef) {
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


### Dicstra ###
## vi har deffineret edges_list, matrix, adj_list i sproget.
edges_list input = input()
matrix input = input()
adj_list input = input()

best_paths = dicstra(input, source_note, input_type)  
best_paths = dicstra(input, source_note)              #dicstra ved her hvilken type af graf den arbejder med


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
