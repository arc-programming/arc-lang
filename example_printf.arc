// Example of extern printf and other C function declarations in Arc

// Basic printf with format string only (most common usage)
extern func printf(format: *i8) -> i32;

// For more complex printf usage, you might need specific wrapper functions
// Since Arc doesn't support variadic functions yet, you can create C wrapper functions:
// In a separate C file, you could define:
// int printf_with_string(const char* format, const char* str) { return printf(format, str); }
// int printf_with_int(const char* format, int num) { return printf(format, num); }
// Then declare them in Arc as:
// extern func printf_with_string(format: *i8, str: *i8) -> i32;
// extern func printf_with_int(format: *i8, num: i32) -> i32;

// Other useful C standard library functions
extern func puts(str: *i8) -> i32;
extern func malloc(size: u64) -> *u8;
extern func free(ptr: *u8);
extern func strlen(str: *i8) -> u64;

func main() -> i32 {
    // Simple string output
    puts("Hello from Arc!");
    
    // Using printf with just format string
    printf("This is a simple printf\n");
    printf("Arc C-FFI is working!\n");
    
    return 0;
}
