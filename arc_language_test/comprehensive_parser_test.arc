// Comprehensive Arc Language Parser and Semantic Analysis Test
// This file tests all currently supported language features according to the specification

// ============================================================================
// MODULE SYSTEM TESTS
// ============================================================================

// Public module declaration
pub mod graphics {
    // Nested module
    mod rendering {
        // Function in nested module
        fn render_primitive() -> void {
            // Empty implementation
        }
    }
    
    // Public function in module
    pub fn draw_line(x1: i32, y1: i32, x2: i32, y2: i32) -> void {
        // Implementation would go here
    }
}

// External module references
mod collections;
mod io;

// Use declarations
use std::collections;
use graphics::rendering;
use io::println;

// ============================================================================
// COMPREHENSIVE TYPE SYSTEM TESTS
// ============================================================================

// Primitive type variable declarations
fn test_primitive_types() -> void {
    // Signed integers
    var i8_val: i8 = -128;
    var i16_val: i16 = -32768;
    var i32_val: i32 = -2147483648;
    var i64_val: i64 = -9223372036854775808;
    var isize_val: isize = -1;
    
    // Unsigned integers
    var u8_val: u8 = 255;
    var u16_val: u16 = 65535;
    var u32_val: u32 = 4294967295;
    var u64_val: u64 = 18446744073709551615;
    var usize_val: usize = 1024;
    
    // Floating point
    var f32_val: f32 = 3.14159;
    var f64_val: f64 = 2.718281828459045;
    
    // Other primitives
    var bool_val: bool = true;
    var char_val: char = 'A';
    var void_return: void;
}

// Complex type declarations
fn test_complex_types() -> void {
    // Pointer types
    var mutable_ptr: ^i32;
    var immutable_ptr: *const i32;
    var nested_ptr: ^*const f64;
    
    // Optional types
    var optional_int: i32?;
    var optional_ptr: ^i32?;
    var optional_const_ptr: *const i32?;
    
    // Array types
    var fixed_array: [i32; 10];
    var char_array: [char; 256];
    var nested_array: [[f32; 3]; 3];
    var large_array: [u8; 4096];
    
    // Slice types
    var int_slice: []i32;
    var char_slice: []char;
    var ptr_slice: []^f64;
    var const_slice: []*const u8;
    
    // Function types
    var simple_func: fn() -> void;
    var math_func: fn(i32, i32) -> i32;
    var named_params: fn(width: u32, height: u32) -> bool;
    var complex_func: fn(data: []i32, callback: fn(i32) -> bool) -> []i32;
    var optional_func: fn(i32?) -> []^f64;
    
    // Parenthesized types for precedence
    var optional_func_type: (fn(i32) -> bool)?;
    var array_of_funcs: [fn() -> void; 5];
}

// ============================================================================
// FUNCTION DECLARATION TESTS
// ============================================================================

// Basic function declarations
fn no_params() -> void {
    // Empty function
}

fn single_param(x: i32) -> i32 {
    return x;
}

fn multiple_params(a: i32, b: f64, c: bool) -> bool {
    return c && a > 0;
}

// Complex parameter types
fn complex_params(
    buffer: []u8,
    callback: fn(u8) -> bool,
    optional_ptr: ^i32?,
    array_ref: [f32; 16]
) -> void {
    // Function body
}

// Function with complex return types
fn return_function() -> fn(i32, i32) -> i32 {
    // Would return a function
}

fn return_optional() -> i32? {
    // Would return optional value
}

fn return_array() -> [i32; 100] {
    // Would return array
}

fn return_slice() -> []char {
    // Would return slice
}

// Public functions
pub fn public_function(data: []u8) -> usize {
    return 0;
}

pub fn public_complex_function(
    input: [^f64; 8],
    transformer: fn(^f64) -> f64,
    output: []f64
) -> bool {
    return true;
}

// ============================================================================
// VARIABLE DECLARATION TESTS
// ============================================================================

fn test_variable_declarations() -> void {
    // Basic let declarations
    let immutable_int: i32 = 42;
    let immutable_float: f64 = 3.14159;
    let immutable_bool: bool = false;
    let immutable_char: char = 'X';
    
    // Const declarations
    const PI: f64 = 3.14159265359;
    const MAX_BUFFER_SIZE: usize = 8192;
    const DEBUG_MODE: bool = true;
    const VERSION_CHAR: char = 'v';
    
    // Var declarations (mutable)
    var mutable_counter: i32 = 0;
    var mutable_name: string = "Arc Language";
    var mutable_flag: bool = false;
    
    // Type inference (without explicit types)
    let inferred_int = 123;
    let inferred_float = 45.67;
    let inferred_string = "Hello, World!";
    let inferred_bool = true;
    
    // Uninitialized variables
    var uninitialized_int: i32;
    var uninitialized_array: [f32; 10];
    var uninitialized_ptr: ^u8;
    
    // Complex type variables
    var function_var: fn(i32) -> bool;
    var optional_var: f64?;
    var array_var: [string; 5];
    var slice_var: []^i32;
}

// ============================================================================
// EXPRESSION TESTS
// ============================================================================

fn test_arithmetic_expressions() -> void {
    var a: i32 = 10;
    var b: i32 = 20;
    var c: f64 = 3.5;
    var d: f64 = 2.0;
    
    // Basic arithmetic
    var sum: i32 = a + b;
    var difference: i32 = a - b;
    var product: i32 = a * b;
    var quotient: i32 = a / b;
    var remainder: i32 = a % b;
    
    // Float arithmetic
    var float_sum: f64 = c + d;
    var float_diff: f64 = c - d;
    var float_product: f64 = c * d;
    var float_quotient: f64 = c / d;
    
    // Mixed expressions (should cause semantic errors)
    var mixed_sum: i32 = a + c;  // Error: type mismatch
    var mixed_product: f64 = b * c;  // Error: type mismatch
}

fn test_comparison_expressions() -> void {
    var x: i32 = 15;
    var y: i32 = 25;
    var p: f64 = 1.5;
    var q: f64 = 2.5;
    
    // Integer comparisons
    var equal: bool = x == y;
    var not_equal: bool = x != y;
    var less_than: bool = x < y;
    var greater_than: bool = x > y;
    var less_equal: bool = x <= y;
    var greater_equal: bool = x >= y;
    
    // Float comparisons
    var float_equal: bool = p == q;
    var float_not_equal: bool = p != q;
    var float_less: bool = p < q;
    var float_greater: bool = p > q;
    
    // Mixed comparisons (should cause semantic errors)
    var mixed_compare: bool = x < p;  // Error: type mismatch
}

fn test_logical_expressions() -> void {
    var flag1: bool = true;
    var flag2: bool = false;
    var flag3: bool = true;
    
    // Logical operations
    var and_result: bool = flag1 && flag2;
    var or_result: bool = flag1 || flag2;
    var not_result: bool = !flag1;
    
    // Complex logical expressions
    var complex_logic: bool = (flag1 && flag2) || (!flag3 && flag1);
    var chained_logic: bool = flag1 && flag2 && flag3;
    var mixed_logic: bool = flag1 || (flag2 && !flag3);
    
    // Invalid logical operations (should cause semantic errors)
    var invalid_and: bool = flag1 && 42;  // Error: type mismatch
    var invalid_or: bool = 3.14 || flag2;  // Error: type mismatch
}

fn test_unary_expressions() -> void {
    var number: i32 = 42;
    var float_num: f64 = 3.14;
    var flag: bool = true;
    var ptr: ^i32;
    
    // Unary minus
    var negative_int: i32 = -number;
    var negative_float: f64 = -float_num;
    
    // Logical not
    var not_flag: bool = !flag;
    
    // Address-of and dereference (when pointers are implemented)
    var address: ^i32 = &number;  // Address-of
    var dereferenced: i32 = ^ptr;  // Dereference
    
    // Invalid unary operations (should cause semantic errors)
    var invalid_minus: i32 = -flag;  // Error: cannot negate boolean
    var invalid_not: bool = !number;  // Error: cannot NOT integer
}

// ============================================================================
// FUNCTION CALL TESTS
// ============================================================================

fn add_two_numbers(x: i32, y: i32) -> i32 {
    return x + y;
}

fn multiply_floats(a: f64, b: f64) -> f64 {
    return a * b;
}

fn process_array(data: []i32, size: usize) -> bool {
    return size > 0;
}

fn no_param_function() -> i32 {
    return 100;
}

fn void_function(message: string) -> void {
    // Print message (implementation not shown)
}

fn test_function_calls() -> void {
    // Valid function calls
    var result1: i32 = add_two_numbers(10, 20);
    var result2: i32 = add_two_numbers(5, 15);
    var float_result: f64 = multiply_floats(2.5, 4.0);
    var array_result: bool = process_array([], 0);
    var no_param_result: i32 = no_param_function();
    
    // Void function calls
    void_function("Hello, Arc!");
    void_function("Testing void calls");
    
    // Nested function calls
    var nested_result: i32 = add_two_numbers(no_param_function(), 5);
    var complex_nested: f64 = multiply_floats(multiply_floats(1.5, 2.0), 3.0);
    
    // Function calls in expressions
    var expression_call: i32 = add_two_numbers(1, 2) + add_two_numbers(3, 4);
    var comparison_call: bool = add_two_numbers(5, 5) > no_param_function();
    
    // === SEMANTIC ERROR TESTS ===
    
    // Wrong number of arguments
    var too_few_args: i32 = add_two_numbers(5);  // Error: missing argument
    var too_many_args: i32 = add_two_numbers(1, 2, 3);  // Error: too many arguments
    
    // Wrong argument types
    var wrong_type1: i32 = add_two_numbers(1.5, 2);  // Error: float to int
    var wrong_type2: f64 = multiply_floats(10, 3.14);  // Error: int to float
    var wrong_type3: bool = process_array("string", 5);  // Error: string to array
    
    // Calling undefined functions
    var undefined_call: i32 = undefined_function(1, 2);  // Error: undefined function
    
    // Using variables as functions
    var x: i32 = 42;
    var invalid_call: i32 = x(1, 2);  // Error: variable is not a function
    
    // Wrong return type assignment
    var wrong_return1: string = add_two_numbers(1, 2);  // Error: int to string
    var wrong_return2: i32 = void_function("test");  // Error: void to int
    
    // Using undeclared variables in calls
    void_function(undeclared_variable);  // Error: undeclared variable
}

// ============================================================================
// CONTROL FLOW TESTS
// ============================================================================

fn test_if_statements() -> void {
    var condition: bool = true;
    var x: i32 = 10;
    var y: i32 = 20;
    
    // Simple if
    if condition {
        x = 5;
    }
    
    // If-else
    if x > y {
        x = y;
    } else {
        y = x;
    }
    
    // If-else if-else chain
    if x < 0 {
        x = 0;
    } else_if x > 100 {
        x = 100;
    } else_if x == 50 {
        x = 25;
    } else {
        x = x + 1;
    }
    
    // Nested if statements
    if condition {
        if x > 0 {
            if y > 0 {
                var result: i32 = x + y;
            }
        }
    }
    
    // Complex conditions
    if (x > 0) && (y > 0) {
        var sum: i32 = x + y;
    }
    
    if (x == 0) || (y == 0) {
        var product: i32 = 0;
    }
}

fn test_while_loops() -> void {
    var counter: i32 = 0;
    var sum: i32 = 0;
    var flag: bool = true;
    
    // Simple while loop
    while counter < 10 {
        counter = counter + 1;
    }
    
    // While loop with complex condition
    while (counter > 0) && flag {
        counter = counter - 1;
        if counter == 5 {
            flag = false;
        }
    }
    
    // Nested while loops
    var i: i32 = 0;
    while i < 5 {
        var j: i32 = 0;
        while j < 5 {
            sum = sum + i + j;
            j = j + 1;
        }
        i = i + 1;
    }
    
    // While with function calls
    while add_two_numbers(counter, 1) < 20 {
        counter = counter + 2;
    }
}

fn test_for_loops() -> void {
    var total: i32 = 0;
    var numbers: [i32; 10];
    var chars: [char; 26];
    
    // Basic for loop (syntax parsing only)
    for item in numbers {
        total = total + item;
    }
    
    // For loop with different types
    for character in chars {
        // Process character
    }
    
    // Nested for loops
    for outer_item in numbers {
        for inner_item in numbers {
            total = total + outer_item + inner_item;
        }
    }
}

fn test_return_statements() -> i32 {
    var x: i32 = 10;
    var y: i32 = 20;
    
    // Simple return
    if x > y {
        return x;
    }
    
    // Return with expression
    if x == y {
        return x + y;
    }
    
    // Return with function call
    if x < y {
        return add_two_numbers(x, y);
    }
    
    // Multiple returns
    while x < 100 {
        if x == 50 {
            return x;
        }
        x = x + 1;
    }
    
    // Default return
    return 0;
}

fn test_break_continue() -> void {
    var i: i32 = 0;
    var sum: i32 = 0;
    
    // Break in while loop
    while true {
        if i > 10 {
            break;
        }
        i = i + 1;
    }
    
    // Continue in while loop
    i = 0;
    while i < 20 {
        i = i + 1;
        if i % 2 == 0 {
            continue;
        }
        sum = sum + i;
    }
    
    // Nested loops with break/continue
    i = 0;
    while i < 10 {
        var j: i32 = 0;
        while j < 10 {
            if j == 5 {
                break;
            }
            if (i + j) % 3 == 0 {
                continue;
            }
            sum = sum + i + j;
            j = j + 1;
        }
        i = i + 1;
    }
}

// ============================================================================
// BLOCK AND SCOPING TESTS
// ============================================================================

fn test_block_statements() -> void {
    var outer_var: i32 = 100;
    
    // Simple block
    {
        var inner_var: i32 = 200;
        outer_var = outer_var + inner_var;
    }
    
    // Nested blocks
    {
        var level1_var: i32 = 10;
        {
            var level2_var: i32 = 20;
            {
                var level3_var: i32 = 30;
                var total: i32 = level1_var + level2_var + level3_var;
            }
        }
    }
    
    // Blocks in control flow
    if outer_var > 50 {
        {
            var block_var: i32 = 5;
            outer_var = outer_var - block_var;
        }
    }
    
    // Complex nested structure
    while outer_var > 0 {
        {
            var loop_block_var: i32 = 10;
            if loop_block_var > 5 {
                {
                    var deep_var: i32 = 1;
                    outer_var = outer_var - deep_var;
                }
            }
        }
    }
}

// ============================================================================
// ERROR-INDUCING TESTS (FOR SEMANTIC ANALYSIS)
// ============================================================================

fn test_semantic_errors() -> void {
    // === VARIABLE ERRORS ===
    
    // Using undeclared variables
    var bad1: i32 = undeclared_var + 5;  // Error: undeclared variable
    undeclared_var2 = 42;  // Error: undeclared variable in assignment
    
    // Type mismatches in variable declarations
    var bad2: i32 = 3.14159;  // Error: float to int
    var bad3: bool = "string";  // Error: string to bool
    var bad4: f64 = true;  // Error: bool to float
    var bad5: char = 12345;  // Error: int to char
    
    // Type mismatches in assignments
    var int_var: i32 = 10;
    var float_var: f64 = 2.5;
    var bool_var: bool = false;
    var string_var: string = "text";
    
    int_var = 3.14;  // Error: float to int
    float_var = true;  // Error: bool to float
    bool_var = "false";  // Error: string to bool
    string_var = 42;  // Error: int to string
    
    // Using uninitialized variables
    var uninitialized: i32;
    var bad6: i32 = uninitialized + 5;  // Error: using uninitialized variable
    
    // Duplicate variable declarations
    var duplicate_name: i32 = 1;
    var duplicate_name: f64 = 2.0;  // Error: duplicate declaration
    
    // === TYPE OPERATION ERRORS ===
    
    // Invalid arithmetic operations
    var bad_arith1: i32 = int_var + string_var;  // Error: int + string
    var bad_arith2: f64 = bool_var * float_var;  // Error: bool * float
    var bad_arith3: bool = int_var - bool_var;  // Error: int - bool
    
    // Invalid comparison operations
    var bad_comp1: bool = int_var > string_var;  // Error: int > string
    var bad_comp2: bool = bool_var < float_var;  // Error: bool < float
    var bad_comp3: bool = string_var == int_var;  // Error: string == int
    
    // Invalid logical operations
    var bad_logic1: bool = int_var && bool_var;  // Error: int && bool
    var bad_logic2: bool = float_var || bool_var;  // Error: float || bool
    var bad_logic3: bool = !int_var;  // Error: !int
    
    // Invalid unary operations
    var bad_unary1: i32 = -bool_var;  // Error: -bool
    var bad_unary2: f64 = -string_var;  // Error: -string
    var bad_unary3: bool = !float_var;  // Error: !float
}

fn test_function_errors() -> i32 {
    // === FUNCTION CALL ERRORS ===
    
    // Already tested in test_function_calls, but adding more
    
    // Calling non-existent functions
    var error1: i32 = nonexistent_func();  // Error: undefined function
    var error2: bool = another_missing_func(1, 2, 3);  // Error: undefined function
    
    // Using variables as functions
    var not_a_function: i32 = 42;
    var error3: i32 = not_a_function();  // Error: not a function
    var error4: void = not_a_function(1, 2);  // Error: not a function
    
    // === RETURN TYPE ERRORS ===
    
    // This function should return i32, but returning wrong types
    if true {
        return "string";  // Error: returning string instead of i32
    }
    
    if false {
        return 3.14159;  // Error: returning float instead of i32
    }
    
    if true {
        return true;  // Error: returning bool instead of i32
    }
    
    // Missing return (should be caught by semantic analysis)
    // No return statement for all paths
}

fn test_void_function_errors() -> void {
    // This function should return void
    
    if true {
        return 42;  // Error: returning value from void function
    }
    
    if false {
        return "text";  // Error: returning value from void function
    }
    
    // Valid void returns
    if true {
        return;  // OK: void return
    }
}

// ============================================================================
// COMPLEX INTEGRATION TESTS
// ============================================================================

fn fibonacci(n: i32) -> i32 {
    if n <= 1 {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}

fn factorial(n: i32) -> i32 {
    if n <= 1 {
        return 1;
    }
    return n * factorial(n - 1);
}

fn test_recursive_functions() -> void {
    var fib_result: i32 = fibonacci(10);
    var fact_result: i32 = factorial(5);
    
    // Complex expressions with recursive calls
    var combined: i32 = fibonacci(5) + factorial(4);
    var comparison: bool = fibonacci(8) > factorial(3);
    
    // Recursive calls in control flow
    if fibonacci(7) > 10 {
        var temp: i32 = factorial(fibonacci(3));
    }
    
    while factorial(3) < fibonacci(10) {
        var counter: i32 = fibonacci(4);
    }
}

fn complex_calculation(
    data: []i32,
    multiplier: f64,
    threshold: i32,
    callback: fn(i32) -> bool
) -> i32 {
    var result: i32 = 0;
    var temp: f64 = 0.0;
    
    // Complex nested control flow
    for item in data {
        if item > threshold {
            temp = multiplier * 2.0;
            if temp > 10.0 {
                if callback(item) {
                    result = result + item;
                    
                    while result < 1000 {
                        result = result * 2;
                        if result > 500 {
                            break;
                        }
                    }
                } else {
                    continue;
                }
            }
        } else {
            {
                var local_calc: i32 = item + threshold;
                if local_calc % 2 == 0 {
                    result = result + local_calc;
                }
            }
        }
    }
    
    return result;
}

fn test_complex_expressions() -> void {
    // Complex arithmetic expressions
    var complex1: i32 = ((10 + 20) * 3) - (15 / 3) + (8 % 3);
    var complex2: f64 = (3.14 * 2.0) + (1.5 / 0.5) - (4.2 * 1.1);
    
    // Complex logical expressions
    var complex3: bool = (true && false) || (!true && (false || true));
    var complex4: bool = (10 > 5) && (20 < 30) && !(15 == 20);
    
    // Mixed expressions with function calls
    var complex5: i32 = add_two_numbers(fibonacci(5), factorial(3)) + no_param_function();
    var complex6: bool = (fibonacci(6) > factorial(4)) && (multiply_floats(2.0, 3.0) < 10.0);
    
    // Deeply nested expressions
    var complex7: i32 = add_two_numbers(
        add_two_numbers(1, 2) + add_two_numbers(3, 4),
        add_two_numbers(5, 6) * add_two_numbers(7, 8)
    );
    
    // Complex control flow conditions
    if ((complex1 > 50) && (complex2 < 100.0)) || (complex3 && !complex4) {
        while (complex5 != complex7) && (complex6 || !complex3) {
            if fibonacci(complex1 % 10) > factorial(3) {
                break;
            }
            complex1 = complex1 + 1;
        }
    }
}

// ============================================================================
// MAIN FUNCTION - COMPREHENSIVE TEST RUNNER
// ============================================================================

fn main() -> i32 {
    // Test primitive types
    test_primitive_types();
    
    // Test complex types
    test_complex_types();
    
    // Test variable declarations
    test_variable_declarations();
    
    // Test expressions
    test_arithmetic_expressions();
    test_comparison_expressions();
    test_logical_expressions();
    test_unary_expressions();
    
    // Test function calls
    test_function_calls();
    
    // Test control flow
    test_if_statements();
    test_while_loops();
    test_for_loops();
    
    // Test return statements and flow control
    var return_test: i32 = test_return_statements();
    test_break_continue();
    
    // Test block statements
    test_block_statements();
    
    // Test semantic errors (should generate error messages)
    test_semantic_errors();
    
    // Test function errors
    var error_test: i32 = test_function_errors();
    test_void_function_errors();
    
    // Test complex integration scenarios
    test_recursive_functions();
    test_complex_expressions();
    
    // Module functions
    graphics::draw_line(0, 0, 100, 100);
    
    return 0;
}

// ============================================================================
// ADDITIONAL EDGE CASE TESTS
// ============================================================================

// Function with maximum complexity
fn stress_test_function(
    param1: [[[^*const fn([]i32?, ^f64) -> bool?; 5]; 10]; 20],
    param2: fn(fn(i32) -> void, []^char) -> fn() -> i32?,
    param3: *const [fn(bool) -> ^[]u8; 100]
) -> fn(^*const i32?, []fn() -> void) -> bool {
    // This function tests the parser's ability to handle deeply nested types
    
    var local_complex: fn(fn(i32) -> bool) -> fn() -> void;
    var array_of_funcs: [fn(u8) -> char; 256];
    var optional_array: [i32?; 50];
    
    // Complex nested expressions
    if ((param1[0][0][0] != null) && (param2 != null)) || (param3 != null) {
        while true {
            for item in optional_array {
                if item != null {
                    break;
                }
            }
            break;
        }
    }
    
    // Return complex function type (placeholder)
    return null;  // This would be a semantic error, but tests parsing
}

// Testing edge cases with identifiers and keywords
fn test_edge_cases() -> void {
    // Variables with names similar to keywords (but valid)
    var if_var: i32 = 10;  // 'if_var' is valid (contains keyword but isn't keyword)
    var while_loop: bool = true;  // 'while_loop' is valid
    var for_each: string = "iteration";  // 'for_each' is valid
    var function_ptr: fn() -> void;  // 'function_ptr' is valid
    
    // Maximum length identifiers (testing lexer limits)
    var very_long_identifier_name_that_tests_the_maximum_length_supported_by_the_lexer_implementation: i32 = 42;
    
    // Unicode identifiers (if supported)
    //var café: f64 = 3.14;
    //var δ: f64 = 0.1;
    //var αβγ: string = "Greek";
    
    // Numbers with various formats
    var decimal_int: i32 = 12345;
    var hex_int: i32 = 0xFF00FF;
    var binary_int: i32 = 0b11010101;
    var octal_int: i32 = 0o755;
    
    var float_normal: f64 = 123.456;
    var float_no_decimal: f64 = 123.;
    var float_no_integer: f64 = .456;
    var float_scientific: f64 = 1.23e10;
    var float_scientific_neg: f64 = 4.56e-5;
    
    // String with escape sequences
    var escaped_string: string = "Line 1\nLine 2\tTabbed\r\nWindows line ending";
    var quoted_string: string = "She said \"Hello, World!\"";
    var backslash_string: string = "Path\\to\\file.txt";
    
    // Character literals
    var char_normal: char = 'A';
    var char_escaped: char = '\n';
    var char_tab: char = '\t';
    var char_quote: char = '\'';
    var char_backslash: char = '\\';
}

// Testing deeply nested structures
fn test_deep_nesting() -> void {
    // Deeply nested blocks
    {
        {
            {
                {
                    {
                        var deep_var: i32 = 10;
                        if deep_var > 0 {
                            while deep_var > 0 {
                                if deep_var % 2 == 0 {
                                    {
                                        var even_deeper: i32 = deep_var / 2;
                                        deep_var = even_deeper;
                                    }
                                } else {
                                    deep_var = deep_var - 1;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    // Deeply nested function calls
    var nested_calls: i32 = add_two_numbers(
        add_two_numbers(
            add_two_numbers(
                add_two_numbers(1, 2),
                add_two_numbers(3, 4)
            ),
            add_two_numbers(
                add_two_numbers(5, 6),
                add_two_numbers(7, 8)
            )
        ),
        add_two_numbers(
            add_two_numbers(
                add_two_numbers(9, 10),
                add_two_numbers(11, 12)
            ),
            add_two_numbers(
                add_two_numbers(13, 14),
                add_two_numbers(15, 16)
            )
        )
    );
}

// Testing maximum expression complexity
fn test_expression_complexity() -> i32 {
    var a: i32 = 1;
    var b: i32 = 2;
    var c: i32 = 3;
    var d: i32 = 4;
    var e: i32 = 5;
    
    // Extremely complex expression
    var result: i32 = ((((a + b) * (c - d)) / (e + a)) % (b * c)) + 
                      ((((b - c) + (d * e)) - (a / b)) * (c + d)) -
                      ((((e - a) * (b + c)) + (d - e)) / (a * b)) +
                      ((((c + e) - (a * d)) * (b - c)) % (e + d));
    
    return result;
}
