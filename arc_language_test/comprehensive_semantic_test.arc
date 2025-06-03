
// Comprehensive semantic analysis test
// This file tests various semantic analysis features

// Valid function declarations
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}

fn print_number(n: i32) -> void {
    // Function body (implementation not required for semantic analysis)
}

fn get_magic_number() -> i32 {
    return 42;
}

// Function to test various semantic errors
fn test_function() -> i32 {
    // Valid variable declarations
    var x: i32 = 10;
    var y: f64 = 3.14;
    var name: string = "Arc Language";
    var is_valid: bool = true;
    
    // Valid function call with correct arguments
    var sum: i32 = add(5, 10);
    
    // Valid function call with no arguments  
    var magic: i32 = get_magic_number();
    
    // Valid function call with void return
    print_number(42);
    
    // === SEMANTIC ERRORS TO TEST ===
    
    // Error: Function call with wrong number of arguments
    var bad_sum: i32 = add(5);  // Missing second argument
    
    // Error: Function call with too many arguments
    var another_bad_sum: i32 = add(1, 2, 3);  // Too many arguments
    
    // Error: Calling undefined function
    var undefined_result: i32 = undefined_function(1, 2);
    
    // Error: Using variable as function
    var result: i32 = x(1, 2);  // 'x' is a variable, not a function
    
    // Error: Type mismatch in assignment from function call
    var wrong_type: string = get_magic_number();  // i32 cannot be assigned to string
    
    // Error: Using void function result in assignment
    var void_result: i32 = print_number(10);  // void cannot be assigned to i32
    
    // Error: Undeclared variable in function argument
    print_number(undeclared_var);
    
    // Error: Type mismatch in function argument
    print_number("hello");  // string passed to i32 parameter
    
    // Error: Wrong return type
    return "should be i32";  // Returning string instead of i32
}

// Test variable initialization and assignment errors
fn test_variables() -> void {
    var uninitialized: i32;  // Declared but not initialized
    var another: i32 = uninitialized;  // Error: using uninitialized variable
    
    var initialized: i32 = 100;
    var copy: i32 = initialized;  // OK: using initialized variable
    
    // Error: Type mismatch in assignment
    initialized = 3.14;  // f64 cannot be assigned to i32
    
    // Error: Assignment to undeclared variable
    nonexistent = 42;
    
    // Error: Duplicate variable declaration
    var initialized: f64 = 2.5;  // 'initialized' already declared
}

// Test arithmetic and logical operations
fn test_operations() -> void {
    var a: i32 = 10;
    var b: i32 = 20;
    var c: f64 = 1.5;
    var flag: bool = true;
    
    // Valid operations
    var sum: i32 = a + b;
    var diff: i32 = a - b; 
    var product: i32 = a * b;
    var quotient: i32 = a / b;
    var remainder: i32 = a % b;
    
    var float_sum: f64 = c + 2.5;
    var comparison: bool = a > b;
    var logical: bool = flag && true;
    
    // Error: Type mismatch in arithmetic
    var bad_sum: i32 = a + c;  // i32 + f64 mismatch
    
    // Error: Invalid operation with boolean
    var bad_math: i32 = flag + a;  // bool + i32 is invalid
    
    // Error: String in arithmetic
    var bad_string_math: i32 = a + "hello";  // i32 + string is invalid
}

// Test array access (if supported)
fn test_arrays() -> void {
    // This will test array semantic analysis when implemented
    // For now, these might cause parse errors
}

// Main function
fn main() -> i32 {
    test_function();
    test_variables();
    test_operations();
    return 0;
}
