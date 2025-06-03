// Test file to trigger various semantic analysis features
fn main() {
    // Test 1: Variable declaration with type mismatch
    var x: i32 = "hello";  // Should error: cannot assign string to i32
    
    // Test 2: Use of uninitialized variable
    var y: i32;
    var z: i32 = y + 5;  // Should error: use of uninitialized variable
    
    // Test 3: Undeclared variable
    var w: i32 = unknown_var;  // Should error: undeclared variable
    
    // Test 4: Duplicate variable declaration
    var a: i32 = 10;
    var a: f64 = 3.14;  // Should error: duplicate declaration
    
    // Test 5: Type inference
    var inferred = 42;  // Should infer i32
    var b: f64 = inferred;  // Should error: cannot assign i32 to f64 (no implicit conversion)
    
    // Test 6: Assignment to immutable variable (const)
    // Note: We need to implement const first
    
    // Test 7: Binary operation type checking
    var bool_val: bool = true;
    var num_result: i32 = bool_val + 5;  // Should error: cannot add bool and i32
    
    // Test 8: Valid operations
    var x1: i32 = 10;
    var x2: i32 = 20;
    var sum: i32 = x1 + x2;  // Should be valid
    
    var comp: bool = x1 < x2;  // Should be valid
    
    return;
}

fn test_function(param: i32) -> i32 {
    // Test function return type checking would go here
    return param * 2;
}
