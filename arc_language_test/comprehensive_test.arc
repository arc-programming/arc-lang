// Comprehensive test of working Arc type system features

// Primitive types
let x: i32 = 42;
let y: f64 = 3.14;
let flag: bool = true;
let ch: char = 'a';

// Pointer types
let mut_ptr: ^i32 = null;
let immut_ptr: *const i32 = null;

// Array types (declarations only)
let arr: [i32; 10];
let matrix: [[f32; 3]; 3];

// Slice types  
let slice: []i32;
let str_slice: []char;

// Optional types
let opt_val: i32? = 42;
let opt_ptr: ^i32? = null;
let opt_slice: []char?;

// Function types
let callback: fn(i32, i32) -> i32 = add;
let proc: fn() = do_something;
let transform: fn([]i32) -> []f32 = convert;

// Complex nested types
let complex_fn: fn(i32?) -> []^f64;
let optional_fn: (fn(i32) -> bool)?;
