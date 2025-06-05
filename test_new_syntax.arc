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

func add(a: i32, b: i32) -> i32 {
    return a + b
}
