// Comprehensive module system test
use math;
use string_utils::{length, concat};
use math::{add, PI};

// External function declaration
extern func printf(format: *i8, ...) -> i32;

func test_module_imports() {
    // Test simple module import
    let result = math::add(5, 3);
    printf("math::add(5, 3) = %d\n", result);
    
    // Test specific imports
    let sum = add(10, 20);
    printf("add(10, 20) = %d\n", sum);
    
    // Test constant import
    printf("PI = %f\n", PI);
    
    // Test string utilities
    let str = "Hello";
    let len = length(str);
    printf("length('%s') = %d\n", str, len);
}

// Inline module definition
pub mod utils {
    pub func helper() -> i32 {
        return 42;
    }
    
    pub const MAX_SIZE = 1024;
    
    // Nested module
    pub mod inner {
        pub func nested_func() -> i32 {
            return 100;
        }
    }
}

func test_inline_modules() {
    let value = utils::helper();
    printf("utils::helper() = %d\n", value);
    
    printf("utils::MAX_SIZE = %d\n", utils::MAX_SIZE);
    
    let nested = utils::inner::nested_func();
    printf("utils::inner::nested_func() = %d\n", nested);
}

func main() -> i32 {
    printf("=== Module System Test ===\n");
    
    test_module_imports();
    test_inline_modules();
    
    printf("=== Test Complete ===\n");
    return 0;
}
