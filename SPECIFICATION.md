# Arc Language Specification v0.3.0 - Refined Syntax

## Core Design Philosophy
- **Clean & Readable**: Minimize visual noise
- **Distinctive**: Unique syntax elements that aren't just Rust/Zig clones
- **Developer Experience**: Intuitive and productive to write
- **Specialty Features**: Cool syntax for advanced features

---

## 1. Refined Lexical Structure

### 1.1 Keywords (Refined)
```arc
// Declaration keywords
func const let mut mod use pub
type struct enum interface impl

// Control flow
if elif else while for in match when
break continue return yield

// Special Arc keywords
defer comptime stream phantom
context using with grant revoke
pipeline async await sync

// Literals
true false nil void
```

### 1.2 Operators (Enhanced)
```arc
// Arithmetic
+ - * / % ** (power)

// Comparison  
== != < > <= >= <=>  (spaceship operator)

// Logical
and or not

// Bitwise
& | ^ << >> ~

// Assignment
= := += -= *= /= %= **= &= |= ^= <<= >>=

// Special Arc operators
|> (pipeline)        ~> (async pipeline)
?? (null coalescing) !! (force unwrap)
=> (function arrow)  -> (type arrow)
:: (scope resolution) @@ (attribute application)
<| (reverse pipeline) |< (composition)
```

---

## 2. Variable Declarations (Refined)

### 2.1 Clean Variable Syntax
```arc
// Immutable by default (like Kotlin/Swift)
let name = "Alice"              // Type inferred
let age: i32 = 25              // Explicit type
let score: f64                 // Uninitialized (must assign before use)

// Mutable when needed
mut counter = 0                // Mutable, inferred type
mut buffer: [u8; 1024]        // Mutable with explicit type

// Constants (compile-time)
const PI = 3.14159             // Compile-time constant
const MAX_SIZE: usize = 1024   // With explicit type
```

### 2.2 Destructuring (New)
```arc
// Tuple destructuring
let (x, y) = get_coordinates()
let (first, ..rest) = get_array()

// Struct destructuring  
let Point{x, y} = get_point()
let Person{name, age: user_age} = get_person()
```

---

## 3. Function Syntax (Refined)

### 3.1 Clean Function Declaration
```arc
// Basic function (no 'fn' keyword)
func greet() {
    print("Hello!")
}

// With parameters and return type
func add(a: i32, b: i32) -> i32 {
    return a + b
}

// Expression body (no braces needed)
func square(x: i32) -> i32 => x * x

// Multiple return values
func divmod(a: i32, b: i32) -> (i32, i32) => (a / b, a % b)

// Optional parameters with defaults
func connect(host: str, port: i32 = 8080, timeout: i32 = 30) -> Connection {
    // implementation
}
```

### 3.2 Lambda/Closure Syntax
```arc
// Short lambda syntax
let add = |a, b| => a + b
let filter_positive = |x| => x > 0

// Block lambda
let processor = |data| {
    validate(data)
    transform(data)
    return finalize(data)
}

// Capturing context
func make_counter() -> func() -> i32 {
    mut count = 0
    return || {
        count += 1
        return count
    }
}
```

---

## 4. Type System (Enhanced)

### 4.1 Type Declarations
```arc
// Type aliases
type UserId = u64
type Point2D = (f32, f32)

// Generic type aliases
type Result<T, E> = enum {
    ok(T)
    err(E)
}

// Function types (cleaner syntax)
type Handler = func(Event) -> Response
type Predicate<T> = func(T) -> bool
```

### 4.2 Struct Syntax (Refined)
```arc
// Simple struct
type Person = struct {
    name: str
    age: i32
    email?: str  // Optional field
}

// Struct with methods
type Rectangle = struct {
    width: f32
    height: f32
    
    // Methods inside struct definition
    func area(self) -> f32 => self.width * self.height
    
    func scale(mut self, factor: f32) {
        self.width *= factor
        self.height *= factor
    }
}

// Generic struct
type Container<T> = struct {
    data: T
    size: usize
    
    func get(self) -> T => self.data
}
```

### 4.3 Enum Syntax (Enhanced)
```arc
// Simple enum
type Color = enum {
    Red, Green, Blue
}

// Enum with associated data
type Message = enum {
    Text(str)
    Image(str, i32, i32)  // url, width, height
    Video { url: str, duration: i32 }  // named fields
}

// Pattern matching
match message {
    Text(content) => print(content)
    Image(url, w, h) => render_image(url, w, h)
    Video{url, duration} => play_video(url, duration)
}
```

---

## 5. Memory Management (Refined)

### 5.1 Pointer Syntax (Cleaner)
```arc
// Owned pointer (like Box<T>)
let owned: own<i32> = own.new(42)

// Borrowed reference (like &T)
let borrowed: ref<i32> = ref.to(owned)

// Mutable reference
let mut_ref: mut<i32> = mut.to(owned)

// Raw pointer (for unsafe operations)
let raw: ptr<i32> = ptr.from(owned)

// Optional pointers
let maybe_ptr: own<i32>? = get_optional_value()
```

### 5.2 Array and Slice Syntax
```arc
// Arrays (stack allocated)
let numbers: [i32; 5] = [1, 2, 3, 4, 5]
let matrix: [[f32; 3]; 3] = [[1,0,0], [0,1,0], [0,0,1]]

// Slices (views into arrays)
let slice: [i32] = numbers[1..4]  // Elements 1, 2, 3
let all: [i32] = numbers[..]      // All elements

// Dynamic arrays
let mut vec: Vec<i32> = vec![1, 2, 3, 4, 5]
vec.push(6)
```

---

## 6. Control Flow (Enhanced)

### 6.1 Conditional Syntax
```arc
// Standard if-else
if condition {
    do_something()
} elif other_condition {
    do_other()
} else {
    do_default()
}

// Expression form
let result = if x > 0 then "positive" else "non-positive"

// Guard clauses
func process(data: Data?) {
    guard let data = data else return
    guard data.is_valid() else throw InvalidDataError
    
    // Continue with valid data
}
```

### 6.2 Loop Syntax
```arc
// While loop
while condition {
    // body
}

// For loops
for item in collection {
    process(item)
}

for i in 0..10 {  // Range 0 to 9
    print(i)
}

for (index, value) in collection.enumerate() {
    print("{}:{}", index, value)
}

// Loop with else (executes if no break)
for item in items {
    if item.matches(criteria) {
        found = item
        break
    }
} else {
    print("No matching item found")
}
```

---

## 7. Error Handling (Refined)

### 7.1 Result Type and Error Propagation
```arc
// Result type (built-in)
type Result<T, E> = enum {
    ok(T)
    err(E)
}

// Error propagation with try operator
func divide(a: f64, b: f64) -> Result<f64, MathError> {
    if b == 0.0 {
        return err(MathError.DivisionByZero)
    }
    return ok(a / b)
}

func calculate() -> Result<f64, MathError> {
    let x = try divide(10.0, 2.0)  // Propagates error
    let y = try divide(x, 3.0)     // Chain operations
    return ok(y)
}

// Alternative: using ? operator (familiar but refined)
func calculate_alt() -> Result<f64, MathError> {
    let x = divide(10.0, 2.0)?
    let y = divide(x, 3.0)?
    return ok(y)
}
```

### 7.2 Defer and Resource Management
```arc
func process_file(path: str) -> Result<void, FileError> {
    let file = try open_file(path)
    defer file.close()  // Always executes on scope exit
    
    let buffer = try allocate(1024)
    defer deallocate(buffer)  // LIFO order
    
    return process_data(file, buffer)
}
```

---

## 8. Special Arc Features (New Cool Syntax)

### 8.1 Pipeline Operator
```arc
// Transform data through pipeline
let result = data
    |> filter(_, |x| => x > 0)
    |> map(_, |x| => x * 2)
    |> take(_, 10)
    |> collect()

// Async pipeline
let response = request
    ~> validate_async(_)
    ~> process_async(_)
    ~> send_async(_)
    await
```

### 8.2 Context System (Distinctive)
```arc
// Define contexts
context Logger {
    level: LogLevel
    output: OutputStream
}

context Database {
    connection: DbConnection
    transaction?: Transaction
}

// Function using contexts
func save_user(user: User) using Logger, Database {
    logger.info("Saving user: {}", user.name)
    database.insert("users", user)
}

// Provide context
with Logger(level: .Info, output: stdout), 
     Database(connection: db_conn) {
    save_user(new_user)
}
```

### 8.3 Phantom Resources (Compile-time State Tracking)
```arc
// Define phantom resource
phantom type DatabaseLock
phantom type FileHandle

// Function that grants phantom resource
func acquire_lock() -> grant<DatabaseLock> {
    system_acquire_lock()
    return grant()  // Compile-time only
}

// Function requiring phantom resource
func update_data(data: Data) using phantom<DatabaseLock> {
    // Can only be called with lock held
    unsafe_update_database(data)
}

// Usage
with acquire_lock() {
    update_data(user_data)  // Compiles - lock is held
}
// update_data(user_data)  // Compile error - no lock
```

### 8.4 Attribute System
```arc
// Built-in attributes with @@ syntax
@@inline @@hot_path
func critical_function(x: f32) -> f32 {
    return expensive_calculation(x)
}

@@derive(Debug, Clone, Serialize)
@@align(64)
type CacheOptimizedData = struct {
    @@volatile status: u32
    data: [60]u8
}

// Custom attributes
@@benchmark(iterations: 1000000)
@@profile(memory: true)
func sort_algorithm(data: mut [i32]) {
    // Implementation
}
```

### 8.5 Comptime and Metaprogramming
```arc
// Compile-time execution
comptime {
    const BUFFER_SIZE = calculate_optimal_size()
    static_assert(BUFFER_SIZE > 0, "Buffer size must be positive")
}

// Generic function with comptime parameters
func create_array<T>(comptime size: usize, default: T) -> [T; size] {
    let mut result: [T; size]
    comptime for i in 0..size {
        result[i] = default
    }
    return result
}

// Code generation
comptime func generate_getters<T>() {
    foreach field in T.fields() {
        @@generate func get_{field.name}(self: T) -> {field.type} {
            return self.{field.name}
        }
    }
}
```

### 8.6 Pattern Matching (Enhanced)
```arc
// Advanced pattern matching
match value {
    // Basic patterns
    0 => "zero"
    1..10 => "small"
    n if n > 100 => "large"
    
    // Destructuring
    Point{x: 0, y} => "on y-axis at {}"(y)
    Point{x, y} if x == y => "diagonal point"
    
    // Array patterns
    [] => "empty"
    [first] => "single: {}"(first)
    [first, ..rest] => "first: {}, rest: {}"(first, rest.len())
    
    // Enum patterns
    Result.ok(value) => process(value)
    Result.err(error) => handle_error(error)
    
    // Guard with capture
    Some(x) if x > threshold => "valid: {}"(x)
    
    // Default
    _ => "unknown"
}
```

---

## 9. Module System (Refined)

```arc
// Module declaration
mod math {
    pub const PI = 3.14159
    
    pub func sin(x: f64) -> f64 {
        // implementation
    }
    
    // Private by default
    func internal_helper() {
        // not exported
    }
}

// Import syntax
use std::collections::{Vec, HashMap}
use math::{PI, sin}
use graphics::* // Import all public items

// Aliased imports
use very::long::module::name as short
use std::collections::Vec as Array
```

---

## 10. Concurrency (Refined)

### 10.1 Stream-based Concurrency
```arc
// Spawn a stream (lightweight thread)
stream worker(id: i32, work: Channel<Task>) {
    while let task = work.receive()? {
        process_task(task)
        yield  // Cooperative yielding
    }
}

// Channel operations
let (sender, receiver) = channel<Message>(capacity: 100)

// Async/await syntax
async func fetch_data(url: str) -> Result<Data, NetError> {
    let response = await http_get(url)?
    let data = await response.json::<Data>()?
    return ok(data)
}

// Sync point
let results = sync [
    fetch_data("url1"),
    fetch_data("url2"),
    fetch_data("url3")
]
```

---

## Summary of Distinctive Features

1. **`func` instead of `fn`** - cleaner, more readable
2. **Expression-body functions with `=>`** - concise syntax
3. **`mut` instead of `mut` prefix** - cleaner mutability
4. **Pipeline operator `|>`** - functional composition
5. **Async pipeline `~>`** - unique async chaining
6. **Context system with `using`** - dependency injection
7. **Phantom resources** - compile-time state tracking
8. **`@@` attributes** - distinctive from `#[]` or `@`
9. **`guard` statements** - early returns
10. **`with` blocks** - scoped resource management
11. **`stream` for concurrency** - unique to Arc
12. **Pattern matching with advanced guards**
13. **`comptime` blocks** - compile-time execution
14. **`nil` instead of `null`** - cleaner
15. **`and`/`or`/`not` logical operators** - more readable than `&&`/`||`/`!`

This syntax is modern, clean, and distinctive while maintaining excellent



# Arc Language Specification v0.2.0

**Version**: 0.2.0  
**Date**: December 2024  
**Status**: Parser Implementation - Core Features

## Changelog

### v0.2.0 (December 2024)
- ✅ Added comprehensive type system parsing
- ✅ Implemented module system (`mod`, `use`, `pub`)
- ✅ Added variable declarations (`let`, `const`, `var`)
- ✅ Added function declarations with full type support
- ✅ Added primitive type parsing (i8, i16, i32, i64, isize, u8, u16, u32, u64, usize, f32, f64, bool, char, void)
- ✅ Added pointer types (`^Type`, `*const Type`)
- ✅ Added optional types (`Type?`)
- ✅ Added array types (`[Type; size]`)
- ✅ Added slice types (`[]Type`)
- ✅ Added function types (`fn(params) -> ReturnType`)
- ✅ Added parenthesized types for precedence
- ✅ Added basic control flow parsing (if/else, while, for, return, break, continue)
- ✅ Added basic expression parsing (binary ops, unary ops, function calls, field access)
- ✅ Added comment support (line and block comments)
- ⚠️ Removed features not yet implemented in parser

### v0.1.0 (Initial)
- Initial specification draft

---

## Currently Supported Language Features

This specification documents **only** the features that are currently implemented in the Arc language parser. Features marked as "Future" are planned but not yet supported.

## 1. Lexical Structure

### Comments
```arc
// Line comment
/* Block comment */
```

### Identifiers
- Must start with letter or underscore
- Can contain letters, digits, underscores
- Case-sensitive

### Keywords
```
// Control flow
if else_if else while for in match break continue return

// Declarations  
fn const let var mod use pub

// Types
i8 i16 i32 i64 isize u8 u16 u32 u64 usize f32 f64 bool char void

// Literals
true false null

// Future keywords (recognized but not parsed)
defer comptime stream capability phantom_resource
struct enum interface impl union type extern export
try catch orelse context using with_context
static_assert phantom
```

### Operators
```
// Arithmetic
+ - * / %

// Comparison  
== != < > <= >=

// Logical
&& || !

// Bitwise
& | ^ << >> ~

// Assignment
= += -= *= /= %= &= |= ^= <<= >>=

// Access and navigation
. :: -> |>

// Type and memory
? @ # 

// Grouping
( ) [ ] { }

// Separators
, ; ..  ...
```

### Literals

#### Integer Literals
```arc
42          // Decimal
0x2A        // Hexadecimal  
0b101010    // Binary
0o52        // Octal
```

#### Floating Point Literals
```arc
3.14
.5
2.0
1e6
1.5e-3
```

#### String Literals
```arc
"Hello, World!"
"Line 1\nLine 2"
"Unicode: \u{1F60A}"
```

#### Character Literals
```arc
'a'
'\n'
'\u{41}'    // Unicode
```

#### Boolean and Null Literals
```arc
true
false
null
```

## 2. Type System

### Primitive Types
```arc
// Signed integers
i8 i16 i32 i64 isize

// Unsigned integers  
u8 u16 u32 u64 usize

// Floating point
f32 f64

// Other primitives
bool char void
```

### Pointer Types
```arc
^i32           // Mutable pointer
*const i32     // Immutable pointer
```

### Optional Types
```arc
i32?           // Optional integer
^i32?          // Optional pointer
```

### Array Types
```arc
[i32; 10]      // Fixed-size array
[[f32; 3]; 3]  // Nested arrays
```

### Slice Types
```arc
[]i32          // Slice of integers
[]char         // Slice of characters
```

### Function Types
```arc
fn() -> void                    // No parameters
fn(i32, i32) -> i32            // Multiple parameters
fn(name: i32, age: i32) -> bool // Named parameters
fn(i32?) -> []^f64             // Complex nested types
```

### Parenthesized Types
```arc
(fn(i32) -> bool)?     // Optional function type
```

## 3. Variable Declarations

### Basic Variable Declaration
```arc
let name: i32 = 42;
let age: u32;
```

### Constant Declaration
```arc
const PI: f64 = 3.14159;
const MAX_SIZE: usize = 1024;
```

### Mutable Variable Declaration
```arc
var counter: i32 = 0;
```

### Type Inference
```arc
let name = "Alice";        // Type inferred as string
let count = 42;            // Type inferred as i32
```

## 4. Function Declarations

### Basic Function
```arc
fn greet() -> void {
    // Function body
}
```

### Function with Parameters
```arc
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}
```

### Function with Complex Types
```arc
fn process(data: []i32, callback: fn(i32) -> bool) -> []i32 {
    // Function body
}
```

## 5. Module System

### Module Declaration
```arc
mod math;                   // External module
mod utils {                 // Inline module
    // Module contents
}
```

### Module Imports
```arc
use std::io;               // Import module
use collections::Vec;      // Import specific item
```

### Public Visibility
```arc
pub fn public_function() -> void {
    // Public function
}

pub mod public_module {
    // Public module
}
```

## 6. Control Flow

### Conditional Statements
```arc
if condition {
    // then branch
} else_if other_condition {
    // else if branch  
} else {
    // else branch
}
```

### Loops
```arc
// While loop
while condition {
    // loop body
}

// For loop (basic parsing supported)
for item in collection {
    // loop body
}
```

### Flow Control
```arc
return value;       // Return from function
break;             // Break from loop
continue;          // Continue loop
```

## 7. Expressions

### Binary Expressions
```arc
a + b              // Arithmetic
x == y             // Comparison
p && q             // Logical
```

### Unary Expressions  
```arc
!flag              // Logical NOT
-number            // Negation
^ptr               // Dereference
&value             // Address-of
```

### Function Calls
```arc
func()             // No arguments
add(1, 2)          // With arguments
```

### Field Access
```arc
obj.field          // Field access
module::item       // Module path
```

## 8. Blocks and Statements

### Block Statements
```arc
{
    let x = 10;
    let y = 20;
    // More statements
}
```

### Expression Statements
```arc
function_call();
x + y;
```

## Parser Limitations

The current parser implementation has these limitations:

1. **Array literals** - `[1, 2, 3]` syntax not yet supported
2. **Struct/enum definitions** - User-defined types not implemented
3. **Match expressions** - Pattern matching not implemented  
4. **Generic types** - `Vec<T>` syntax not supported
5. **Error handling** - `try`/`catch` not implemented
6. **Advanced features** - `defer`, `comptime`, etc. not implemented
7. **Complex expressions** - Limited expression parsing
8. **String interpolation** - Not implemented
9. **Attributes** - `@packed`, `@inline` not implemented

## Grammar Summary

```ebnf
Program = Declaration*

Declaration = ModuleDecl | UseDecl | FunctionDecl | VariableDecl

ModuleDecl = "pub"? "mod" IDENTIFIER (";" | "{" Declaration* "}")
UseDecl = "use" ModulePath ";"
FunctionDecl = "pub"? "fn" IDENTIFIER "(" ParameterList? ")" ("->" Type)? Block
VariableDecl = ("let" | "const" | "var") IDENTIFIER (":" Type)? ("=" Expression)? ";"

Type = BaseType TypePostfix*
BaseType = PrimitiveType | "(" Type ")" | FunctionType
TypePostfix = "?" | "[" Type ";" Expression "]" | "[" "]" | "^" | "*" "const"
FunctionType = "fn" "(" ParameterList? ")" ("->" Type)?

PrimitiveType = "i8" | "i16" | "i32" | "i64" | "isize" | 
                "u8" | "u16" | "u32" | "u64" | "usize" |
                "f32" | "f64" | "bool" | "char" | "void"

Expression = BinaryExpr | UnaryExpr | PrimaryExpr
PrimaryExpr = IDENTIFIER | Literal | FunctionCall | FieldAccess | "(" Expression ")"

Statement = ExpressionStmt | VariableDecl | IfStmt | WhileStmt | ForStmt | 
            ReturnStmt | BreakStmt | ContinueStmt | Block

Block = "{" Statement* "}"
```

---

## Implementation Status

**Lexer**: ~90% complete  
**Parser**: ~30% complete  
**Type System**: ~70% complete  
**Module System**: ~60% complete  
**Control Flow**: ~40% complete  
**Expressions**: ~35% complete  

The current implementation provides a solid foundation for basic Arc programs with strong type system support and module organization capabilities.



# Arc Language Specification
**Version 0.1-draft**

---

## Table of Contents

1. [Introduction](#1-introduction)
2. [Lexical Structure](#2-lexical-structure)
3. [Type System](#3-type-system)
4. [Memory Management](#4-memory-management)
5. [Error Handling](#5-error-handling)
6. [Concurrency Model](#6-concurrency-model)
7. [Metaprogramming](#7-metaprogramming)
8. [Attribute System](#8-attribute-system)
9. [Unique Features](#9-unique-features)
10. [Standard Library](#10-standard-library)
11. [Interoperability](#11-interoperability)

---

## 1. Introduction

Arc is a systems programming language designed for performance-critical applications requiring explicit resource control and predictable execution characteristics. The language prioritizes compile-time verification, zero-cost abstractions, and developer productivity through integrated tooling.

### 1.1 Design Principles

- **Explicit Control**: All resource allocation, memory management, and potential performance costs are visible in source code
- **Compile-time Verification**: Maximum error detection during compilation rather than runtime
- **Zero-overhead Abstractions**: High-level constructs compile to optimal machine code
- **Predictable Performance**: No hidden allocations, garbage collection, or runtime overhead

### 1.2 Target Applications

Arc targets domains where performance predictability and resource control are essential:
- Operating system kernels and drivers
- Real-time systems and embedded applications
- Game engines and multimedia processing
- High-frequency trading systems
- Network infrastructure software

---

## 2. Lexical Structure

### 2.1 Character Set

Arc source files are encoded in UTF-8. The language supports Unicode identifiers following UAX#31 guidelines.

### 2.2 Comments

```arc
// Line comment extends to end of line

/* Block comment
   can span multiple lines */
```

### 2.3 Identifiers

- Variables, functions, modules: `snake_case`
- Types: `PascalCase` 
- Constants: `UPPER_SNAKE_CASE`

### 2.4 Keywords

```
mod use type struct enum interface impl fn const var
if else_if else while for in match break continue return
defer comptime stream capability phantom_resource
true false null
```

### 2.5 Operators

```
+ - * / % = == != < > <= >= ! & | ^ << >>
+= -= *= /= %= &= |= ^= <<= >>=
&& || .. ... |> ! ? @ #
```

---

## 3. Type System

### 3.1 Primitive Types

```arc
// Signed integers
i8 i16 i32 i64 isize

// Unsigned integers  
u8 u16 u32 u64 usize

// Floating point
f32 f64

// Other primitives
bool char void
```

### 3.2 Composite Types

#### 3.2.1 Arrays and Slices

```arc
var fixed_array [i32; 10];           // Fixed-size array
var slice []i32;                     // Slice (view into array)
var mutable_slice []mut i32;         // Mutable slice
```

#### 3.2.2 Pointers

```arc
var mutable_ptr ^i32;                // Mutable pointer
var immutable_ptr *const i32;        // Immutable pointer
var optional_ptr ?^i32;              // Optional mutable pointer
var optional_const_ptr ?*const i32;  // Optional immutable pointer
```

#### 3.2.3 Structures

```arc
type Point struct {
    x f32;
    y f32;
}

// Initialization
var p = Point{ .x = 1.0, .y = 2.0 };
```

#### 3.2.4 Enumerations

```arc
type Color enum {
    Red,
    Green, 
    Blue,
    RGB(u8, u8, u8),  // Variant with data
}

// Pattern matching required for access
match color {
    .Red: { /* handle red */ };
    .Green: { /* handle green */ };
    .Blue: { /* handle blue */ };
    .RGB(r, g, b): { /* handle rgb with values */ };
}
```

#### 3.2.5 Unions

```arc
type Value union {
    integer i64;
    floating f64;
    text str_slice;
}
```

### 3.3 Generic Types

```arc
type Container(T) struct {
    data ^T;
    size usize;
}

fn create_container(comptime T type, alloc Allocator) Container(T) {
    var ptr = alloc.alloc(@sizeof(T));
    return Container(T){ .data = @ptrCast(^T, ptr), .size = 1 };
}
```

### 3.4 Interfaces

```arc
interface Drawable {
    fn draw(self ^Self, canvas ^Canvas) void;
    fn bounds(self ^Self) Rectangle;
}

// Implementation
impl Point Drawable {
    fn draw(self ^Point, canvas ^Canvas) void {
        canvas.draw_point(self.x, self.y);
    }
    
    fn bounds(self ^Point) Rectangle {
        return Rectangle{ .x = self.x, .y = self.y, .w = 1, .h = 1 };
    }
}
```

---

## 4. Memory Management

### 4.1 Allocator Interface

All heap allocations in Arc require an explicit allocator parameter. This design prevents hidden allocation costs and enables flexible memory management strategies.

```arc
interface Allocator {
    fn alloc(self ^Self, bytes usize, align usize) ?^u8;
    fn free(self ^Self, ptr ^u8, bytes usize, align usize) void;
    fn realloc(self ^Self, ptr ^u8, old_bytes usize, new_bytes usize, align usize) ?^u8;
}
```

### 4.2 Memory Safety Features

#### 4.2.1 Explicit Null Handling

```arc
var maybe_ptr ?^i32 = get_optional_pointer();

// Must handle null case explicitly
var value = maybe_ptr orelse {
    io.println("Pointer was null");
    return;
};

// Or use pattern matching
match maybe_ptr {
    null: { /* handle null case */ };
    ptr: { /* use ptr safely */ };
}
```

#### 4.2.2 Resource Tracking

```arc
@resource_type(cleanup_fn = "close_file")
type File struct {
    handle i32;
    path str_slice;
}

fn close_file(file ^File) void {
    system_close(file.handle);
}

// Automatic cleanup at scope exit
fn process_file() void {
    var file = open_file("data.txt")!;
    // file automatically closed when function exits
}
```

---

## 5. Error Handling

Arc uses explicit error handling through result types and the error propagation operator.

### 5.1 Result Type

```arc
type Result(T, E) enum {
    ok(T),
    err(E),
}
```

### 5.2 Error Propagation

```arc
fn divide(a f64, b f64) Result(f64, MathError) {
    if b == 0.0 {
        return .err(.DivisionByZero);
    }
    return .ok(a / b);
}

fn calculate() Result(f64, MathError) {
    var x = divide(10.0, 2.0)!;  // Propagates error if division fails
    var y = divide(x, 3.0)!;     // Chain operations
    return .ok(y);
}
```

### 5.3 Defer Statements

```arc
fn process_file(path str_slice) Result(void, FileError) {
    var file = open_file(path)!;
    defer close_file(file);  // Always executes on function exit
    
    var buffer = allocate_buffer(1024)!;
    defer free_buffer(buffer);  // Cleanup in reverse order
    
    return process_file_data(file, buffer);
}
```

---

## 6. Concurrency Model

### 6.1 Streams

Arc provides lightweight, cooperatively-scheduled execution contexts called streams.

```arc
fn worker_stream(id u32, work_channel Channel(Task)) void {
    while true {
        var task = concur.receive(work_channel);
        match task {
            .ok(t): { process_task(t); };
            .err(.Closed): { break; };
            .err(.Empty): { concur.yield_stream(); };
        }
    }
}

fn main() void {
    var work_queue = concur.make_channel(Task, 100);
    
    // Spawn worker streams
    for i in 0..4 {
        stream worker_stream(i, work_queue);
    }
    
    // Add work to queue
    for task in load_tasks() {
        concur.send(work_queue, task);
    }
    
    concur.run_scheduler();
}
```

### 6.2 Channels

```arc
type Channel(T) opaque;

// Channel operations
fn make_channel(comptime T type, capacity usize) Channel(T);
fn send(channel Channel(T), value T) Result(void, ChannelError);
fn receive(channel Channel(T)) Result(T, ChannelError);
fn close_channel(channel Channel(T)) void;
```

### 6.3 Circuit Breaker Pattern

```arc
@circuit_breaker(
    failure_threshold = 5,
    timeout_ms = 1000,
    recovery_time_ms = 5000
)
fn call_external_api(request ApiRequest) Result(ApiResponse, ApiError) {
    return http.post("https://api.example.com", request);
}
```

---

## 7. Metaprogramming

### 7.1 Compile-time Execution

Arc supports arbitrary code execution at compile time through the `comptime` keyword.

```arc
comptime {
    // This code runs during compilation
    const BUFFER_SIZE = calculate_optimal_buffer_size();
    static_assert(BUFFER_SIZE > 0, "Buffer size must be positive");
}

fn generic_function(comptime T type, comptime count usize) [T; count] {
    var result: [T; count];
    comptime var i = 0;
    inline while (i < count) : (i += 1) {
        result[i] = T.default();
    }
    return result;
}
```

### 7.2 Type Reflection

```arc
fn print_struct_info(comptime T type) void {
    comptime {
        io.print("Struct {} has {} fields:\n", T.name(), T.field_count());
        for T.fields() |field| {
            io.print("  {}: {}\n", field.name, field.type.name());
        }
    }
}
```

### 7.3 Code Generation

```arc
fn generate_accessors(comptime T type) void {
    comptime {
        for T.fields() |field| {
            // Generate getter
            @generate_function(
                "get_" ++ field.name,
                fn(self ^T) field.type {
                    return self.*field.name;
                }
            );
            
            // Generate setter
            @generate_function(
                "set_" ++ field.name,
                fn(self ^T, value field.type) void {
                    self.*field.name = value;
                }
            );
        }
    }
}
```

---

## 8. Attribute System

### 8.1 Built-in Attributes

#### 8.1.1 Memory Layout
```arc
@packed
@align(64)
type CacheAlignedData struct {
    @volatile status u32;
    @little_endian value u64;
    data [56]u8;
}
```

#### 8.1.2 Code Generation
```arc
@serialize(.binary, .little_endian)
@derive_debug
@derive_clone
type NetworkMessage struct {
    header MessageHeader;
    @skip_serialize
    cached_hash u32;
    payload []u8;
}
```

#### 8.1.3 Optimization Hints
```arc
@inline_when(comptime build_mode() == .Release)
@hot_path
fn critical_calculation(x f64) f64 {
    return expensive_math_operation(x);
}
```

### 8.2 Custom Attributes

```arc
// Define custom attribute
@attribute
fn benchmark(comptime iterations u32) void {
    // Generate benchmarking code
    comptime {
        @generate_wrapper_function(fn() {
            var timer = Timer.start();
            for 0..iterations {
                @call_original_function();
            }
            var elapsed = timer.stop();
            io.println("Benchmark completed in {}ms", elapsed);
        });
    }
}

// Usage
@benchmark(1000000)
fn sort_array(data []mut i32) void {
    // Implementation
}
```

---

## 9. Unique Features

### 9.1 Phantom Resources

Phantom resources enable compile-time tracking of abstract program state.

```arc
type DatabaseLock phantom;
type UserPermission phantom;

@grants(DatabaseLock)
fn acquire_database_lock() Result(DatabaseLock, LockError) {
    if try_lock_database() {
        return .ok(phantom_resource(DatabaseLock));
    }
    return .err(.LockBusy);
}

fn update_user(
    user_id u64,
    data UserData,
    comptime _lock DatabaseLock,
    comptime _perm UserPermission
) Result(void, DatabaseError) {
    // Function requires both phantom resources
    return database.update(user_id, data);
}
```

### 9.2 Context Injection

```arc
context Logger {
    level LogLevel;
    output ^OutputStream;
}

context Allocator {
    alloc_fn fn(usize) ?^u8;
    free_fn fn(^u8, usize) void;
}

fn process_data(data []u8) Result(ProcessedData, ProcessError)
    using Logger, Allocator
{
    logger.info("Processing {} bytes", data.len);
    var buffer = allocator.alloc(data.len * 2)!;
    defer allocator.free(buffer, data.len * 2);
    // Process data...
}

fn main() void {
    var logger = create_logger(.Info, &io.stdout);
    var alloc = create_heap_allocator();
    
    with_context(logger, alloc) {
        var data = load_input()!;
        var result = process_data(data)!;
        save_output(result)!;
    }
}
```

### 9.3 Pipeline Operators

```arc
fn transform_data(input []f64) []f64 {
    return input
        |> filter(_, fn(x f64) bool { return x > 0.0; })
        |> map(_, fn(x f64) f64 { return x * 2.0; })
        |> take(_, 100);
}
```

### 9.4 Capability-Based Security

```arc
type FileSystemAccess capability;
type NetworkAccess capability;

fn read_config(comptime _fs FileSystemAccess) Config {
    return parse_config(io.read_file("config.toml"));
}

fn send_telemetry(data TelemetryData, comptime _net NetworkAccess) void {
    http.post("https://telemetry.example.com", data);
}

fn main() capability(FileSystemAccess, NetworkAccess) {
    var config = read_config(filesystem_cap);
    
    // Restrict capabilities for plugin
    run_plugin(plugin_code, capability());
}
```

---

## 10. Standard Library

### 10.1 Core Modules

- `arc.mem`: Memory allocation and management
- `arc.io`: Input/output operations
- `arc.concur`: Concurrency primitives
- `arc.math`: Mathematical functions
- `arc.time`: Time and duration utilities
- `arc.collections`: Data structures (Vec, HashMap, etc.)

### 10.2 Memory Allocators

```arc
// General purpose heap allocator
var gpa = arc.mem.GeneralPurposeAllocator.init();
defer gpa.deinit();

// Arena allocator for batch allocations
var arena = arc.mem.ArenaAllocator.init(gpa.allocator());
defer arena.deinit();

// Fixed buffer allocator
var buffer: [4096]u8;
var fba = arc.mem.FixedBufferAllocator.init(buffer[0..]);
```

---

## 11. Interoperability

### 11.1 C Integration

Arc provides seamless interoperability with C code through explicit FFI declarations.

```arc
// C function declaration
extern "C" fn malloc(size usize) ?^u8;
extern "C" fn free(ptr ^u8) void;
extern "C" fn strlen(str *const u8) usize;

// C struct compatibility
@c_struct
type CPoint struct {
    x i32;
    y i32;
}

// Export Arc function to C
@export("arc_function")
fn arc_function(x i32, y i32) i32 {
    return x + y;
}
```

### 11.2 ABI Compatibility

Arc follows the C ABI for external function calls and data structure layout when marked with appropriate attributes.

---

## Conclusion

Arc represents a modern approach to systems programming, combining the performance and control of C with enhanced safety features, powerful metaprogramming capabilities, and integrated concurrency support. The language design prioritizes compile-time verification and explicit resource management while providing developer-friendly abstractions that compile to efficient machine code.