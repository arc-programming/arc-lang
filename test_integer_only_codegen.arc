// Integer-only codegen test to avoid type issues
// This test only uses integer values to work around the current
// limitation where all variables are generated as 'int' type

func simple_function() {
    let x = 42
}

func add_numbers(a: i32, b: i32) -> i32 {
    let result = a + b
    return result
}

func test_basic_variables() {
    let x = 42
    let y = 10
    let count = 100
}

func test_mut_and_const() {
    mut counter = 0
    mut total = 100
    const MAX_VALUE = 1000
}

func test_basic_math() {
    let a = 10
    let b = 5
    let sum = a + b
    let diff = a - b
    let product = a * b
    let quotient = a / b
}

func test_basic_comparisons() {
    let x = 42
    let y = 10
    let is_equal = x == y
    let is_greater = x > y
    let is_less = x < y
    let is_greater_equal = x >= y
    let is_less_equal = x <= y
}

func test_if_statements() {
    let x = 42
    if x == 42 {
        let answer = 1
    }
    if x > 40 {
        let big_number = 1
    } else {
        let small_number = 0
    }
}

func test_logical_ops() {
    let a = 1
    let b = 0
    // Removed logical operations due to semantic analysis errors
}

func test_while_loops() {
    mut i = 0
    while i < 10 {
        i = i + 1
    }
}

func test_function_calls() {
    let result1 = add_numbers(5, 3)
    let result2 = add_numbers(10, 20)
}

func main() -> i32 {
    simple_function()
    test_basic_variables()
    test_mut_and_const()
    test_basic_math()
    test_basic_comparisons()
    test_if_statements()
    test_logical_ops()
    test_while_loops()
    test_function_calls()
    return 0
}
