// Math module for Arc language
// This module provides basic mathematical functions

pub func add(x: i32, y: i32) -> i32 {
    return x + y;
}

pub func multiply(x: i32, y: i32) -> i32 {
    return x * y;
}

pub func subtract(x: i32, y: i32) -> i32 {
    return x - y;
}

pub func divide(x: i32, y: i32) -> i32 {
    if y == 0 {
        // TODO: Handle division by zero properly
        return 0;
    }
    return x / y;
}

pub const PI = 3.14159;
pub const E = 2.71828;

// Private helper function
func internal_helper() -> i32 {
    return 42;
}
