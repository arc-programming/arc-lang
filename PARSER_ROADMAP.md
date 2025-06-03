# Arc Parser Implementation Roadmap

This document outlines the remaining work needed to complete the Arc language parser implementation.

## Current Status ✅

The parser currently supports:
- Function declarations with parameters and return types
- Variable declarations with type annotations and initializers
- Basic expressions (binary operations, literals, identifiers)
- Block statements and return statements
- Proper error handling and recovery
- Arena-based memory management
- AST construction and printing

## Missing Language Features

### 1. Module System
**Priority: High**

#### Lexer Changes Needed:
- `TOKEN_KEYWORD_MOD` - module declarations
- `TOKEN_KEYWORD_USE` - import statements
- `TOKEN_KEYWORD_PUB` - public visibility
- `TOKEN_DOUBLE_COLON` - `::` path separator

#### Parser Changes Needed:
- `AST_DECL_MODULE` - module declarations
- `AST_DECL_USE` - import statements
- `parse_module_declaration()`
- `parse_use_declaration()`
- `parse_module_path()` - for `module::item` syntax

#### Example Syntax:
```arc
mod math {
    pub fn add(a: i32, b: i32) -> i32 { a + b }
}

use std::io;
use math::add;
```

### 2. Type System
**Priority: High**

#### 2.1 Type Definitions
**Lexer Changes:**
- `TOKEN_KEYWORD_TYPE` - type aliases

**Parser Changes:**
- `AST_DECL_TYPE_ALIAS`
- `parse_type_alias_declaration()`

```arc
type UserId = u64;
type Result<T> = Either<T, Error>;
```

#### 2.2 Struct Definitions
**Lexer Changes:**
- `TOKEN_KEYWORD_STRUCT` - struct declarations

**Parser Changes:**
- `AST_DECL_STRUCT`
- `AST_STRUCT_FIELD`
- `parse_struct_declaration()`
- `parse_struct_field()`

```arc
struct Point {
    x: f64,
    y: f64,
}

struct Person {
    name: String,
    age: u32,
    email: Option<String>,
}
```

#### 2.3 Enum Definitions
**Lexer Changes:**
- `TOKEN_KEYWORD_ENUM` - enum declarations

**Parser Changes:**
- `AST_DECL_ENUM`
- `AST_ENUM_VARIANT`
- `parse_enum_declaration()`
- `parse_enum_variant()`

```arc
enum Color {
    Red,
    Green,
    Blue,
    RGB(u8, u8, u8),
}

enum Option<T> {
    Some(T),
    None,
}
```

#### 2.4 Interface Definitions
**Lexer Changes:**
- `TOKEN_KEYWORD_INTERFACE` - interface declarations
- `TOKEN_KEYWORD_IMPL` - implementation blocks

**Parser Changes:**
- `AST_DECL_INTERFACE`
- `AST_DECL_IMPL`
- `parse_interface_declaration()`
- `parse_impl_declaration()`

```arc
interface Display {
    fn display(&self) -> String;
}

impl Display for Point {
    fn display(&self) -> String {
        format!("({}, {})", self.x, self.y)
    }
}
```

### 3. Constants and Static Variables
**Priority: Medium**

#### Lexer Changes:
- `TOKEN_KEYWORD_CONST` - constant declarations
- `TOKEN_KEYWORD_STATIC` - static variables

#### Parser Changes:
- `AST_DECL_CONST`
- `AST_DECL_STATIC`
- `parse_const_declaration()`
- `parse_static_declaration()`

```arc
const PI: f64 = 3.14159;
const MAX_SIZE: usize = 1024;
static mut COUNTER: i32 = 0;
```

### 4. Advanced Control Flow
**Priority: Medium**

#### 4.1 Pattern Matching
**Lexer Changes:**
- `TOKEN_KEYWORD_MATCH` - match expressions
- `TOKEN_DOUBLE_ARROW` - `=>` match arms

**Parser Changes:**
- `AST_EXPR_MATCH`
- `AST_MATCH_ARM`
- `AST_PATTERN` (with variants for different pattern types)
- `parse_match_expression()`
- `parse_match_arm()`
- `parse_pattern()`

```arc
match color {
    Color::Red => "red",
    Color::Green => "green", 
    Color::Blue => "blue",
    Color::RGB(r, g, b) => format!("rgb({}, {}, {})", r, g, b),
}
```

#### 4.2 Loop Enhancements
**Currently missing:**
- Range-based for loops: `for i in 0..10`
- Iterator for loops: `for item in collection`

**Parser Changes:**
- Update `parse_for_statement()` to handle ranges
- `AST_EXPR_RANGE` for range expressions
- `parse_range_expression()`

```arc
for i in 0..10 {
    println!("{}", i);
}

for item in items.iter() {
    process(item);
}
```

### 5. Error Handling
**Priority: Medium**

#### Lexer Changes:
- `TOKEN_KEYWORD_TRY` - try expressions
- `TOKEN_KEYWORD_CATCH` - catch blocks
- `TOKEN_KEYWORD_THROW` - throw statements
- `TOKEN_QUESTION` - `?` operator

#### Parser Changes:
- `AST_EXPR_TRY`
- `AST_STMT_THROW`
- `AST_EXPR_ERROR_PROPAGATION` - for `?` operator
- `parse_try_expression()`
- `parse_throw_statement()`

```arc
fn divide(a: f64, b: f64) -> Result<f64, String> {
    if b == 0.0 {
        throw "Division by zero";
    }
    Ok(a / b)
}

let result = try divide(10.0, 2.0)?;
```

### 6. Memory Management
**Priority: Medium**

#### Lexer Changes:
- `TOKEN_KEYWORD_DEFER` - defer statements

#### Parser Changes:
- `AST_STMT_DEFER`
- `parse_defer_statement()`

```arc
fn process_file(path: &str) -> Result<(), Error> {
    let file = open_file(path)?;
    defer close_file(file);
    
    // Process file...
    Ok(())
}
```

### 7. FFI and External Functions
**Priority: Low**

#### Lexer Changes:
- `TOKEN_KEYWORD_EXTERN` - external function declarations
- `TOKEN_KEYWORD_EXPORT` - exported functions

#### Parser Changes:
- `AST_DECL_EXTERN`
- `parse_extern_declaration()`
- Add export flag to function declarations

```arc
extern "C" {
    fn malloc(size: usize) -> *mut u8;
    fn free(ptr: *mut u8);
}

export fn arc_add(a: i32, b: i32) -> i32 {
    a + b
}
```

### 8. Compile-time Features
**Priority: Low**

#### Lexer Changes:
- `TOKEN_KEYWORD_COMPTIME` - compile-time evaluation
- `TOKEN_KEYWORD_STATIC_ASSERT` - static assertions

#### Parser Changes:
- `AST_STMT_STATIC_ASSERT`
- `AST_EXPR_COMPTIME`
- `parse_static_assert()`
- `parse_comptime_expression()`

```arc
comptime {
    const SIZE = 1024;
}

static_assert(SIZE > 0, "Size must be positive");
```

### 9. Advanced Type Features
**Priority: Low**

#### 9.1 Generics
**Parser Changes:**
- Generic type parameters in functions, structs, enums
- Type constraints
- `parse_generic_params()`
- `parse_type_constraints()`

```arc
fn max<T: Ord>(a: T, b: T) -> T {
    if a > b { a } else { b }
}

struct Vec<T> {
    data: *mut T,
    len: usize,
    cap: usize,
}
```

#### 9.2 Phantom Types
**Parser Changes:**
- Support for phantom type parameters
- Zero-sized type markers

```arc
struct PhantomData<T>;
struct Id<T> {
    value: u64,
    _phantom: PhantomData<T>,
}
```

### 10. Advanced Expressions
**Priority: Medium**

#### 10.1 Lambda Expressions
**Lexer Changes:**
- `TOKEN_PIPE` - `|` for lambda parameters

**Parser Changes:**
- `AST_EXPR_LAMBDA`
- `parse_lambda_expression()`

```arc
let add = |a, b| a + b;
let filter_fn = |x: i32| x > 0;
```

#### 10.2 Array and Object Literals
**Parser Changes:**
- `AST_EXPR_ARRAY_LITERAL`
- `AST_EXPR_OBJECT_LITERAL`
- `parse_array_literal()`
- `parse_object_literal()`

```arc
let numbers = [1, 2, 3, 4, 5];
let person = Person { name: "Alice", age: 30 };
```

### 11. Macro System
**Priority: Very Low**

#### Lexer Changes:
- `TOKEN_HASH` - `#` for macro invocations
- `TOKEN_KEYWORD_MACRO` - macro definitions

#### Parser Changes:
- `AST_DECL_MACRO`
- `AST_EXPR_MACRO_CALL`
- `parse_macro_declaration()`
- `parse_macro_call()`

```arc
macro debug_print($expr) {
    println!("{} = {}", stringify!($expr), $expr);
}

debug_print!(x + y);
```

## Implementation Priority

### Phase 1: Core Language (High Priority)
1. Module system (`mod`, `use`)
2. Struct definitions
3. Constants (`const`)
4. Enhanced for loops

### Phase 2: Type System (Medium Priority)
1. Enum definitions
2. Interface definitions and implementations
3. Type aliases
4. Pattern matching (`match`)

### Phase 3: Advanced Features (Low Priority)
1. Error handling (`try`, `catch`, `throw`)
2. Defer statements
3. External functions (`extern`, `export`)
4. Generics

### Phase 4: Meta-programming (Very Low Priority)
1. Compile-time features (`comptime`, `static_assert`)
2. Macro system
3. Advanced type features

## Testing Strategy

For each feature implementation:

1. **Unit Tests**: Test individual parsing functions
2. **Integration Tests**: Test complete programs using the feature
3. **Error Tests**: Test error handling and recovery
4. **AST Tests**: Verify correct AST structure generation

## Notes

- The parser foundation is solid and supports the core language constructs
- Each feature should be implemented incrementally with comprehensive testing
- Error handling and recovery should be maintained for all new features
- Memory management through arena allocation should be preserved
- AST printing should be updated for each new node type
