// --- Arc Lexical Test Suite ---
// This file aims to cover all lexical elements of the Arc specification.

// Module and Use
mod my_complex_module;
use arc.core.io;
use arc.net.http as web_client; // Aliasing

/*
 * This is a multi-line block comment.
 * It can span several lines and include *asterisks* or /slashes/.
 * Nested block comments /* like this */ should also be handled if supported.
 */

// --- Keywords and Basic Structure ---
const GLOBAL_PI f32 = 3.1415926535;
const MAX_RETRIES u8 = 5;
var global_counter i64 = 0_i64; // Integer with suffix (lexically just part of identifier/number for now)

type MyInt = i32;
type Point struct {
    x f32;
    y f32;
    label ?str_slice; // Optional string slice
}

type Color enum {
    Red,
    Green(u8),
    Blue(u8, u8),
    Hex(str_slice),
}

type Result(T, E) enum {
    Ok(T),
    Err(E),
}

interface Drawable {
    fn draw(self ^Self, canvas ^Canvas) void;
    fn area(self ^Self) f32;
}

union DataValue {
    i i64;
    f f64;
    s str_slice;
    b bool;
}

// --- Functions ---
extern "C" fn c_printf(format *const u8, ...) i32;

export fn arc_add(a i32, b i32) i32 {
    return a + b;
}

fn main(args []str_slice) Result(void, Error) {
    // Literals
    var integer_val i32 = 42;
    var hex_val u32 = 0xDEADBEEF;
    var binary_val u8 = 0b10101010;
    var octal_val u16 = 0o755; // If Arc supports octal
    var float_val f64 = 3.14e-2;
    var another_float f32 = .5f; // If Arc supports leading dot and f suffix
    var char_val char = 'A';
    var newline_char char = '\n';
    var escaped_quote char = '\'';
    var backslash_char char = '\\';
    var unicode_char char = '\u{1F60A}'; // If Arc supports unicode escapes

    var string_val str_slice = "Hello, \"Arc\" World!\n\tEscapes: \\";
    var raw_string str_slice = r"C:\raw\path\no\escapes"; // If Arc supports raw strings
    var multi_line_string str_slice = """
    This is a
    multi-line string.
    Indentation might be significant or stripped.
    """;

    var is_active bool = true;
    var is_done bool = false;
    var maybe_value ?i32 = null;

    // Operators
    var sum = integer_val + 10 - 5 * 2 / 3 % 2;
    var bitwise = hex_val & binary_val | octal_val ^ 0xFF;
    var shifted = binary_val << 2 >> 1;
    var not_val = !is_active;
    var bit_not_val = ~binary_val;

    var assign = 10;
    assign += 5; // 15
    assign -= 2; // 13
    assign *= 3; // 39
    assign /= 3; // 13
    assign %= 5; // 3
    assign &= 0b111; // 3
    assign |= 0b100; // 7
    assign ^= 0b010; // 5
    var shift_assign = 0b1000;
    shift_assign <<= 1; // 0b10000
    shift_assign >>= 2; // 0b100

    var a = 10; var b = 20;
    if a == b || a != 10 && b > 5 && a < 100 && b >= 20 && a <= 10 {
        io.println("Complex condition");
    }

    // Control Flow
    if is_active {
        io.println("Active!");
    } else_if is_done {
        io.println("Done!");
    } else {
        io.println("Neither active nor done.");
    }

    var i = 0;
    while i < 3 {
        io.println("Loop: {i}");
        i += 1;
        if i == 2 { continue; }
        if i == 3 { break; }
    }

    var numbers = [1, 2, 3, 4, 5];
    for num in numbers {
        io.println("For loop: {num}");
    }
    for idx in 0..5 { // Range
        io.println("Range loop: {idx}");
    }

    var color_val = Color.Green(128);
    match color_val {
        .Red: { io.println("It's red!"); },
        .Green(g): { io.println("It's green: {g}"); },
        .Blue(r, g): { io.println("It's blue-ish (missing one component in pattern)"); }, // Example of pattern matching
        .Hex(hex_str): { io.println("Hex: {hex_str}"); },
        _: { io.println("Some other color."); }, // Default case
    }

    // Pointers and Dereferencing
    var ptr_val = &integer_val;
    var deref_val = *ptr_val;
    var opt_ptr ?^Point = null;

    // Struct and Enum Usage
    var p1 = Point{ .x = 1.0, .y = 2.0, .label = "start" };
    var p2 = Point{ .x = 4.0, .y = 6.0 };
    var dist = distance(p1, p2);
    io.println("Distance: {dist}");
    var access = p1.x;

    // Error Handling (Lexical perspective)
    var result_ok = Result(i32, str_slice).Ok(100);
    var result_err = Result(i32, str_slice).Err("failed");
    var value = some_func_that_returns_result() orelse {
        arc__report_error(null, "Operation failed, using default."); // Using our error reporting
        return .Err(Error.DefaultValueUsed);
    };
    var tried_value = try? another_func(); // Optional try

    defer io.println("This runs at scope exit.");
    defer {
        io.println("Another defer block.");
    }

    // Comptime (Lexical perspective)
    comptime {
        var compile_time_var = 10;
        static_assert(compile_time_var * 2 == 20, "Comptime math failed!");
    }
    var array = comptime make_array(i32, 5);

    // Concurrency (Lexical perspective)
    stream my_worker_stream(channel);
    var data_ch = make_channel(DataPacket, 10);

    // Attributes (Lexical perspective)
    @packed
    @align(16)
    type MyPackedStruct struct {
        a u8;
        b u32;
    }

    @inline
    @deprecated("Use new_function instead")
    fn old_function() void { /* ... */ }

    // Unique Features (Lexical perspective)
    var db_lock = acquire_lock()!; // Error propagation operator '!'
    var user_perm: UserPermission phantom; // Phantom type usage

    context Logger { level: LogLevel; }
    fn needs_context() void using Logger {
        logger.info("Context injected!");
    }
    with_context(my_logger) {
        needs_context();
    }

    var pipeline_result = input_data
        |> step_one(arg1)
        |> step_two()
        |> step_three; // Pipeline operator

    var cap: FileSystemAccess capability; // Capability type usage

    // Miscellaneous punctuation
    var arr_slice []i32 = numbers[1..3]; // Slice with range
    var variadic_args ...any;
    var question_mark_op = opt_ptr?.field; // Optional chaining (if Arc supports)
    var hash_symbol = #some_directive; // If Arc uses # for anything specific
    var tilde_op = ~0x0F;
    var at_symbol_val = @comptime_var; // Using @ for comptime access

    return .Ok; // Returning from main
}

// Another function
fn distance(p1: Point, p2: Point) f32 { // Parameter type annotation
    var dx = p1.x - p2.x;
    var dy = p1.y - p2.y;
    // Using a built-in or stdlib function (lexically an identifier)
    return math.sqrt(dx*dx + dy*dy);
}

// Struct implementation
impl Drawable for Point {
    fn draw(self ^Self, canvas ^Canvas) void {
        // ...
    }
    fn area(self ^Self) f32 {
        return 0.0; // Placeholder
    }
}

// Final check for EOF handling after content