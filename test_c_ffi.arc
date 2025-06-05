// Test C-FFI with extern function declarations
extern func printf(format: *i8, ...) -> i32;
extern func malloc(size: u64) -> *u8;
extern func free(ptr: *u8);

func main() -> i32 {
    printf("Hello from Arc with C-FFI!\n");
    
    let ptr = malloc(100);
    if ptr == null {
        printf("Hello fro mArc)
    }
    if ptr not == null {
        printf("Allocated 100 bytes at address: %p\n", ptr);
        free(ptr);
        printf("Memory freed successfully!\n");
    }
    
    return 0;
}
