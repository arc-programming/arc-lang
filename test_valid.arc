// Test valid Arc syntax

func test_valid_syntax() {
    let immutable_var = 42           // OK: immutable variable
    mut mutable_var = 10             // OK: mutable variable
    const CONSTANT = 3.14            // OK: constant
    
    // This should be valid
    mutable_var = 20                 // OK: mut variable can be reassigned
    
    // Valid operations
    let sum = 42 + 58                // OK: arithmetic with numbers
    let is_equal = sum == 100        // OK: comparison
    let logical_and = true && false  // OK: logical operation with booleans
}

func add(a: i32, b: i32) -> i32 {
    return a + b
}

func test_function_calls() {
    let result = add(10, 20)         // Should work if scoping is fixed
}
