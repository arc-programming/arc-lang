// Test Arc mutability semantics

func test_mutability() {
    let immutable_var = 42           // immutable
    mut mutable_var = 10             // mutable
    const CONSTANT = 3.14            // compile-time constant
    
    // These should be valid
    mutable_var = 20                 // OK: mut variable can be reassigned
    
    // These should be errors
    immutable_var = 50               // ERROR: let variable is immutable
    CONSTANT = 2.71                  // ERROR: const is immutable
    
    // Type mismatches
    let wrong_type: i32 = "string"   // ERROR: type mismatch
    mut bad_math = 42 + "text"       // ERROR: invalid operation
    
    // Undefined variables
    let undefined_var = missing_var  // ERROR: undefined variable
    
    // Function call errors
    let result = add(42)             // ERROR: wrong argument count (if add exists)
    let bad_call = nonexistent()     // ERROR: undefined function
    
    // Invalid operations
    let invalid_op = true && 42      // ERROR: logical operation with non-boolean
    let bad_compare = "hello" > 100  // ERROR: incompatible comparison
}

func add(a: i32, b: i32) -> i32 {
    return a + b
}

func test_return_types() -> i32 {
    return "string"                  // ERROR: return type mismatch
}
