// Test the new Arc syntax
func hello() {
    let name = "Arc"
    mut counter = 0
    
    if counter == 0 {
        counter += 1
    } elif counter == 1 {
        counter *= 2
    } else {
        counter = 0
    }
}


let value: i32 = 42
const PI = 3.14159

func print(a: i32) -> void {
    std::print(a)

    return a + 2
}


func add(a: i32, b: i32) -> i32 {
    return a + b
}

let wrong_type: i32 = "string"  
let bad_math = 42 + "text"     

let undefined_var = missing_var  

let result = add(42)            
let bad_call = nonexistent()
let type_error = add("a", "b")


let invalid_op = true && 42
let bad_compare = "hello" > 100

