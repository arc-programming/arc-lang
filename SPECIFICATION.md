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