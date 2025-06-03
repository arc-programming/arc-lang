// Simple semantic test
fn main() {
    // Test 1: Variable declaration with type mismatch
    var x: i32 = "hello";
    
    // Test 2: Use of uninitialized variable
    var y: i32;
    var z: i32 = y + 5;
    
    // Test 3: Undeclared variable
    var w: i32 = unknown_var;
    
    // Test 4: Duplicate variable declaration
    var a: i32 = 10;
    var a: f64 = 3.14;
    
    // Test 5: Type inference
    var inferred = 42;
    var b: f64 = inferred;
    
    // Test 6: Binary operation type checking
    var bool_val: bool = true;
    var num_result: i32 = bool_val + 5;
    
    // Test 7: Valid operations
    var x1: i32 = 10;
    var x2: i32 = 20;
    var sum: i32 = x1 + x2;
    var comp: bool = x1 < x2;
    
    return;
}

fn test_function(param: i32) -> i32 {
    return param * 2;
}
