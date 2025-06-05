// Comprehensive Arc codegen test with printf output
// This test exercises all working features of the Arc compiler
extern func puts(s: *i8);


func test_integer_calculations() {
    let a = 15
    let b = 7
    let sum = a + b
    let diff = a - b
    let product = a * b
    let quotient = a / b
    
    // Function calls would be needed to actually print results
    // For now, the calculations are performed but not displayed
}

func test_comparisons() {
    let x = 20
    let y = 15
    let equal = x == y
    let greater = x > y
    let less = x < y
    let greater_equal = x >= y
    let less_equal = x <= y
}

func test_control_flow() {
    let value = 25
    
    if value > 20 {
        let result = 1
    } else {
        let result = 0
    }
    
    // While loop test
    mut counter = 0
    while counter < 5 {
        counter = counter + 1
    }
}

func calculate(x: i32, y: i32) -> i32 {
    let result = x * 2 + y
    return result
}

func test_function_calls() {
    let result1 = calculate(10, 5)
    let result2 = calculate(3, 7)
    let result3 = calculate(result1, result2)
}

func main() -> i32 {
    test_integer_calculations()
    test_comparisons()
    test_control_flow()
    test_function_calls()

    return 0
}
