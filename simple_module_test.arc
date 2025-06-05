// Simple test without module imports
//use math;

extern func printf(format: *i8) -> i32;

func test_modules() -> i32 {
    // Test simple variable declaration and arithmetic
    let a: i32 = 3;
    let b: i32 = 5;
    let result: i32 = a + b;
    return result;
}

func main() -> i32 {
    let result: i32 = test_modules();
    printf("Result: %d\n");

    return 0;
}
