#ifndef ARC_PARSER_H
#define ARC_PARSER_H

#include "arc/common.h"
#include "arc/lexer.h"

// Forward declarations
typedef struct ArcAstNode ArcAstNode;
typedef struct ArcParser ArcParser;
typedef struct ArcArena ArcArena;
typedef struct ArcDiagnostic ArcDiagnostic;

// --- Enhanced Source Information ---
// For multi-file projects and better error reporting
typedef struct {
    ArcSourceLocation location;  // Line, column, offset within file
    const char *filename;        // Source filename (NULL for string sources)
    const char *source_text;     // Pointer to source text for this location
} ArcSourceInfo;

// --- Diagnostic Information ---
// For collecting errors, warnings, and notes during parsing
typedef enum {
    ARC_DIAGNOSTIC_ERROR,
    ARC_DIAGNOSTIC_WARNING,
    ARC_DIAGNOSTIC_NOTE,
    ARC_DIAGNOSTIC_HINT
} ArcDiagnosticLevel;

typedef struct ArcDiagnostic {
    ArcDiagnosticLevel level;
    ArcSourceInfo source_info;
    char *message;               // Owned by diagnostic, freed on cleanup
    struct ArcDiagnostic *next;  // Linked list of related diagnostics
} ArcDiagnostic;

// --- Arena Allocator for AST Nodes ---
// Efficiently allocates AST nodes in large blocks
typedef struct ArcArenaBlock {
    void *memory;
    size_t size;
    size_t used;
    struct ArcArenaBlock *next;
} ArcArenaBlock;

typedef struct ArcArena {
    ArcArenaBlock *current_block;
    ArcArenaBlock *first_block;
    size_t default_block_size;
    size_t total_allocated;
} ArcArena;

// --- AST Node Types ---
typedef enum {
    // Literals
    AST_LITERAL_INT,
    AST_LITERAL_FLOAT,
    AST_LITERAL_STRING,
    AST_LITERAL_CHAR,
    AST_LITERAL_BOOL,
    AST_LITERAL_NULL,

    // Identifiers and types
    AST_IDENTIFIER,
    AST_TYPE_PRIMITIVE,
    AST_TYPE_POINTER,
    AST_TYPE_ARRAY,
    AST_TYPE_SLICE,
    AST_TYPE_OPTIONAL,
    AST_TYPE_FUNCTION,

    // Expressions
    AST_EXPR_BINARY,          // a + b, a == b, etc.
    AST_EXPR_UNARY,           // !a, -b, &c, *d
    AST_EXPR_CALL,            // func(args)
    AST_EXPR_INDEX,           // array[index]
    AST_EXPR_FIELD_ACCESS,    // obj.field
    AST_EXPR_CAST,            // value as Type
    AST_EXPR_PIPELINE,        // value |> func
    AST_EXPR_RANGE,           // start..end, start...end
    AST_EXPR_ARRAY_LITERAL,   // [1, 2, 3]
    AST_EXPR_STRUCT_LITERAL,  // Point{x: 1, y: 2}

    // Statements
    AST_STMT_EXPRESSION,  // expression;
    AST_STMT_VAR_DECL,    // var x = value;
    AST_STMT_CONST_DECL,  // const x = value;
    AST_STMT_ASSIGNMENT,  // x = value;
    AST_STMT_BLOCK,       // { statements }
    AST_STMT_IF,          // if condition { } else { }
    AST_STMT_WHILE,       // while condition { }
    AST_STMT_FOR,         // for item in collection { }
    AST_STMT_MATCH,       // match value { cases }
    AST_STMT_RETURN,      // return value;
    AST_STMT_BREAK,       // break;
    AST_STMT_CONTINUE,    // continue;
    AST_STMT_DEFER,       // defer statement;

    // Declarations
    AST_DECL_FUNCTION,    // fn name(params) -> Type { }
    AST_DECL_EXTERN,      // extern fn name(params) -> Type;
    AST_DECL_STRUCT,      // struct Name { fields }
    AST_DECL_ENUM,        // enum Name { variants }
    AST_DECL_TYPE_ALIAS,  // type Name = Type;
    AST_DECL_INTERFACE,   // interface Name { methods }
    AST_DECL_IMPL,        // impl Interface for Type { }
    AST_DECL_MODULE,      // mod name;
    AST_DECL_USE,         // use module::item;

    // Special
    AST_PROGRAM,       // Top-level program node
    AST_PARAMETER,     // Function parameter
    AST_FIELD,         // Struct field
    AST_ENUM_VARIANT,  // Enum variant
    AST_MATCH_CASE,    // Match case
} ArcAstNodeType;

// --- Binary Operators ---
typedef enum {
    // Arithmetic
    BINARY_OP_ADD,  // +
    BINARY_OP_SUB,  // -
    BINARY_OP_MUL,  // *
    BINARY_OP_DIV,  // /
    BINARY_OP_MOD,  // %
    BINARY_OP_POW,  // **

    // Comparison
    BINARY_OP_EQ,         // ==
    BINARY_OP_NE,         // !=
    BINARY_OP_LT,         // <
    BINARY_OP_LE,         // <=
    BINARY_OP_GT,         // >
    BINARY_OP_GE,         // >=
    BINARY_OP_SPACESHIP,  // <=>

    // Logical
    BINARY_OP_AND,  // &&, and
    BINARY_OP_OR,   // ||, or

    // Bitwise
    BINARY_OP_BIT_AND,  // &
    BINARY_OP_BIT_OR,   // |
    BINARY_OP_BIT_XOR,  // ^
    BINARY_OP_BIT_SHL,  // <<
    BINARY_OP_BIT_SHR,  // >>

    // Special Arc operators
    BINARY_OP_PIPELINE,          // |>
    BINARY_OP_ASYNC_PIPELINE,    // ~>
    BINARY_OP_REVERSE_PIPELINE,  // <|
    BINARY_OP_NULL_COALESCING,   // ??
    BINARY_OP_SCOPE_RESOLUTION,  // ::

    // Assignment
    BINARY_OP_ASSIGN,      // =
    BINARY_OP_WALRUS,      // :=
    BINARY_OP_ADD_ASSIGN,  // +=
    BINARY_OP_SUB_ASSIGN,  // -=
    BINARY_OP_MUL_ASSIGN,  // *=
    BINARY_OP_DIV_ASSIGN,  // /=
    BINARY_OP_MOD_ASSIGN,  // %=
    BINARY_OP_POW_ASSIGN,  // **=
    BINARY_OP_AND_ASSIGN,  // &=
    BINARY_OP_OR_ASSIGN,   // |=
    BINARY_OP_XOR_ASSIGN,  // ^=
    BINARY_OP_SHL_ASSIGN,  // <<=
    BINARY_OP_SHR_ASSIGN,  // >>=
} ArcBinaryOperator;

// --- Unary Operators ---
typedef enum {
    UNARY_OP_NOT,           // !
    UNARY_OP_NEGATE,        // -
    UNARY_OP_ADDRESS,       // &
    UNARY_OP_DEREFERENCE,   // *
    UNARY_OP_LOGICAL_NOT,   // not
    UNARY_OP_FORCE_UNWRAP,  // !!
} ArcUnaryOperator;

// --- AST Node Structure ---
struct ArcAstNode {
    ArcAstNodeType type;
    ArcSourceInfo source_info;  // Enhanced source location with filename

    union {
        // === LITERALS ===

        // AST_LITERAL_INT
        struct {
            int64_t value;
            ArcToken token;  // Original token for base/suffix info
        } literal_int;

        // AST_LITERAL_FLOAT
        struct {
            double value;
            ArcToken token;  // Original token
        } literal_float;

        // AST_LITERAL_STRING
        struct {
            char *value;     // Processed string (heap allocated, parser owns)
            size_t length;   // Length of processed string
            ArcToken token;  // Original token with quotes
        } literal_string;

        // AST_LITERAL_CHAR
        struct {
            uint32_t codepoint;  // Unicode codepoint
            ArcToken token;      // Original token with quotes
        } literal_char;

        // AST_LITERAL_BOOL
        struct {
            bool value;      // true or false
            ArcToken token;  // TOKEN_KEYWORD_TRUE or TOKEN_KEYWORD_FALSE
        } literal_bool;

        // AST_LITERAL_NULL
        struct {
            ArcToken token;  // TOKEN_KEYWORD_NULL
        } literal_null;

        // === IDENTIFIERS AND TYPES ===

        // AST_IDENTIFIER
        struct {
            ArcToken token;  // TOKEN_IDENTIFIER with name and location
        } identifier;

        // AST_TYPE_PRIMITIVE
        struct {
            ArcTokenType primitive_type;  // e.g., TOKEN_KEYWORD_I32, TOKEN_KEYWORD_BOOL
            ArcToken token;               // The actual keyword token
        } type_primitive;

        // AST_TYPE_POINTER
        struct {
            ArcAstNode *pointed_type;  // Type being pointed to
            ArcToken caret_token;      // The '^' token
        } type_pointer;

        // AST_TYPE_ARRAY
        struct {
            ArcAstNode *element_type;  // Type of array elements
            ArcAstNode *size_expr;     // Size expression (NULL for dynamic arrays)
            ArcToken lbracket_token;   // '[' token
        } type_array;

        // AST_TYPE_SLICE
        struct {
            ArcAstNode *element_type;  // Type of slice elements
            ArcToken lbracket_token;   // '[' token
        } type_slice;

        // AST_TYPE_OPTIONAL
        struct {
            ArcAstNode *inner_type;   // The wrapped type
            ArcToken question_token;  // '?' token
        } type_optional;

        // AST_TYPE_FUNCTION
        struct {
            ArcAstNode **parameter_types;  // Dynamically managed array
            size_t parameter_count;
            ArcAstNode *return_type;  // NULL if no return type
            ArcToken fn_token;        // 'fn' token
        } type_function;

        // === EXPRESSIONS ===

        // AST_EXPR_BINARY
        struct {
            ArcBinaryOperator op_type;  // Enum for the operation
            ArcToken operator_token;    // Actual operator token (for location/text)
            ArcAstNode *left;           // Left operand
            ArcAstNode *right;          // Right operand
        } binary_expr;

        // AST_EXPR_UNARY
        struct {
            ArcUnaryOperator op_type;  // Enum for the operation
            ArcToken operator_token;   // Actual operator token (for location/text)
            ArcAstNode *operand;       // The operand
        } unary_expr;

        // AST_EXPR_CALL
        struct {
            ArcAstNode *function;    // Function being called
            ArcAstNode **arguments;  // Dynamically managed array
            size_t argument_count;
            ArcToken lparen_token;  // '(' token
        } call_expr;

        // AST_EXPR_INDEX
        struct {
            ArcAstNode *object;       // Object being indexed
            ArcAstNode *index;        // Index expression
            ArcToken lbracket_token;  // '[' token
        } index_expr;

        // AST_EXPR_FIELD_ACCESS
        struct {
            ArcAstNode *object;   // Object being accessed
            ArcToken field_name;  // Field identifier token
            ArcToken dot_token;   // '.' token
        } field_access_expr;

        // AST_EXPR_CAST
        struct {
            ArcAstNode *expression;   // Expression being cast
            ArcAstNode *target_type;  // Target type
            ArcToken as_token;        // 'as' token
        } cast_expr;

        // AST_EXPR_PIPELINE
        struct {
            ArcAstNode *left;         // Left side of pipeline
            ArcAstNode *right;        // Right side (function)
            ArcToken pipeline_token;  // '|>' token
        } pipeline_expr;

        // AST_EXPR_RANGE
        struct {
            ArcAstNode *start;     // Start expression (NULL for open start)
            ArcAstNode *end;       // End expression (NULL for open end)
            bool is_inclusive;     // true for .., false for ...
            ArcToken range_token;  // '..' or '...' token
        } range_expr;

        // AST_EXPR_ARRAY_LITERAL
        struct {
            ArcAstNode **elements;  // Dynamically managed array
            size_t element_count;
            ArcToken lbracket_token;  // '[' token
        } array_literal_expr;

        // AST_EXPR_STRUCT_LITERAL
        struct {
            ArcAstNode *struct_type;   // Type being instantiated
            ArcAstNode **field_inits;  // Dynamically managed array of field initializers
            size_t field_count;
            ArcToken lbrace_token;  // '{' token
        } struct_literal_expr;

        // === STATEMENTS ===

        // AST_STMT_EXPRESSION
        struct {
            ArcAstNode *expression;  // The expression
        } expr_stmt;

        // AST_STMT_VAR_DECL
        struct {
            ArcToken name;                // Variable name token
            ArcAstNode *type_annotation;  // Type annotation (NULL if inferred)
            ArcAstNode *initializer;      // Initializer expression (NULL if uninitialized)
            ArcToken var_token;           // 'var' token
        } var_decl_stmt;

        // AST_STMT_CONST_DECL
        struct {
            ArcToken name;                // Constant name token
            ArcAstNode *type_annotation;  // Type annotation (NULL if inferred)
            ArcAstNode *initializer;      // Initializer expression (required)
            ArcToken const_token;         // 'const' token
        } const_decl_stmt;

        // AST_STMT_ASSIGNMENT
        struct {
            ArcAstNode *target;         // Assignment target (lvalue)
            ArcAstNode *value;          // Value being assigned
            ArcBinaryOperator op_type;  // Assignment type (=, +=, etc.)
            ArcToken operator_token;    // Assignment operator token
        } assignment_stmt;

        // AST_STMT_BLOCK
        struct {
            ArcAstNode **statements;  // Dynamically managed array
            size_t statement_count;
            ArcToken lbrace_token;  // '{' token
        } block_stmt;

        // AST_STMT_IF
        struct {
            ArcAstNode *condition;    // If condition
            ArcAstNode *then_branch;  // Then block
            ArcAstNode *else_branch;  // Else block (NULL if no else)
            ArcToken if_token;        // 'if' token
        } if_stmt;

        // AST_STMT_WHILE
        struct {
            ArcAstNode *condition;  // Loop condition
            ArcAstNode *body;       // Loop body
            ArcToken while_token;   // 'while' token
        } while_stmt;

        // AST_STMT_FOR
        struct {
            ArcToken iterator;     // Iterator variable token
            ArcAstNode *iterable;  // Expression being iterated
            ArcAstNode *body;      // Loop body
            ArcToken for_token;    // 'for' token
            ArcToken in_token;     // 'in' token
        } for_stmt;

        // AST_STMT_MATCH
        struct {
            ArcAstNode *expression;  // Expression being matched
            ArcAstNode **cases;      // Dynamically managed array of match cases
            size_t case_count;
            ArcToken match_token;  // 'match' token
        } match_stmt;

        // AST_STMT_RETURN
        struct {
            ArcAstNode *value;      // Return value (NULL for bare return)
            ArcToken return_token;  // 'return' token
        } return_stmt;

        // AST_STMT_BREAK
        struct {
            ArcToken break_token;  // 'break' token
        } break_stmt;

        // AST_STMT_CONTINUE
        struct {
            ArcToken continue_token;  // 'continue' token
        } continue_stmt;

        // AST_STMT_DEFER
        struct {
            ArcAstNode *statement;  // Statement to defer
            ArcToken defer_token;   // 'defer' token
        } defer_stmt;

        // === DECLARATIONS ===        // AST_DECL_FUNCTION
        struct {
            ArcToken name;            // Function name token
            ArcAstNode **parameters;  // Dynamically managed array
            size_t parameter_count;
            ArcAstNode *return_type;  // Return type (NULL if none)
            ArcAstNode *body;         // Function body
            ArcToken fn_token;        // 'fn' token
        } function_decl;

        // AST_DECL_EXTERN
        struct {
            ArcToken name;            // Function name token
            ArcAstNode **parameters;  // Dynamically managed array
            size_t parameter_count;
            ArcAstNode *return_type;  // Return type (NULL if none)
            ArcToken extern_token;    // 'extern' token
            ArcToken fn_token;        // 'fn' token
            const char *c_name;       // Optional C function name (if different from Arc name)
        } extern_decl;

        // AST_DECL_STRUCT
        struct {
            ArcToken name;        // Struct name token
            ArcAstNode **fields;  // Dynamically managed array
            size_t field_count;
            ArcToken struct_token;  // 'struct' token
        } struct_decl;

        // AST_DECL_ENUM
        struct {
            ArcToken name;          // Enum name token
            ArcAstNode **variants;  // Dynamically managed array
            size_t variant_count;
            ArcToken enum_token;  // 'enum' token
        } enum_decl;

        // AST_DECL_TYPE_ALIAS
        struct {
            ArcToken name;            // Alias name token
            ArcAstNode *target_type;  // Target type
            ArcToken type_token;      // 'type' token
        } type_alias_decl;

        // AST_DECL_INTERFACE
        struct {
            ArcToken name;         // Interface name token
            ArcAstNode **methods;  // Dynamically managed array
            size_t method_count;
            ArcToken interface_token;  // 'interface' token
        } interface_decl;

        // AST_DECL_IMPL
        struct {
            ArcAstNode *interface_type;  // Interface being implemented (NULL for inherent impl)
            ArcAstNode *target_type;     // Type being implemented for
            ArcAstNode **methods;        // Dynamically managed array
            size_t method_count;
            ArcToken impl_token;  // 'impl' token
        } impl_decl;              // AST_DECL_MODULE
        struct {
            ArcToken name;       // Module name token
            ArcToken mod_token;  // 'mod' token
            ArcAstNode *body;    // Module body (NULL for external modules)
            bool is_external;    // true for "mod name;", false for "mod name { ... }"
        } module_decl;           // AST_DECL_USE
        struct {
            ArcAstNode *path;      // Use path (chain of identifiers)
            ArcToken use_token;    // 'use' token
            ArcAstNode **imports;  // Array of specific imports (NULL for simple use)
            size_t import_count;   // Number of specific imports
            bool is_glob_import;   // true for "use module::*"
        } use_decl;

        // === SPECIAL ===

        // AST_PROGRAM
        struct {
            ArcAstNode **declarations;  // Dynamically managed array
            size_t declaration_count;
        } program;

        // AST_PARAMETER
        struct {
            ArcToken name;                // Parameter name token
            ArcAstNode *type_annotation;  // Parameter type
        } parameter;

        // AST_FIELD
        struct {
            ArcToken name;                // Field name token
            ArcAstNode *type_annotation;  // Field type
        } field;

        // AST_ENUM_VARIANT
        struct {
            ArcToken name;        // Variant name token
            ArcAstNode **fields;  // Variant fields (NULL for unit variants)
            size_t field_count;   // 0 for unit variants
        } enum_variant;

        // AST_MATCH_CASE
        struct {
            ArcAstNode *pattern;   // Match pattern
            ArcAstNode *body;      // Case body
            ArcToken arrow_token;  // '=>' token
        } match_case;
    };
};

// --- Parser Structure ---
struct ArcParser {
    // Lexer interface
    ArcLexer *lexer;
    ArcToken current_token;
    ArcToken previous_token;

    // Error handling and recovery
    bool had_error;
    bool panic_mode;
    bool synchronizing;  // For error recovery - skipping tokens until sync point

    // Diagnostic collection
    ArcDiagnostic *diagnostics;      // Linked list of collected errors/warnings
    ArcDiagnostic *last_diagnostic;  // Tail pointer for efficient appending
    size_t diagnostic_count;

    // Memory management - choose one approach:

    // Option 1: Arena allocator (recommended for large programs)
    ArcArena *ast_arena;  // Arena for all AST nodes and strings

    // Option 2: Traditional tracking (fallback if no arena)
    ArcAstNode **allocated_nodes;  // Dynamic array of all allocated nodes
    size_t allocated_count;
    size_t allocated_capacity;

    // Parser configuration
    bool use_arena;     // If true, use arena; if false, use tracking array
    size_t max_errors;  // Stop parsing after this many errors (0 = unlimited)
};

// --- Parser Public API ---

// Initialize parser with a lexer and optional arena
// If arena is NULL, falls back to traditional node tracking
void arc_parser_init(ArcParser *parser, ArcLexer *lexer, ArcArena *arena);

// Initialize parser with traditional node tracking (no arena)
void arc_parser_init_simple(ArcParser *parser, ArcLexer *lexer);

// Clean up parser resources
// If using arena: just resets arena, all AST memory freed at once
// If using tracking: iterates allocated_nodes, calls arc_ast_node_destroy, frees array
// Always frees diagnostic messages and resets diagnostic list
void arc_parser_cleanup(ArcParser *parser);

// Parse a complete program
ArcAstNode *arc_parser_parse_program(ArcParser *parser);

// Parse individual components (useful for testing)
ArcAstNode *arc_parser_parse_expression(ArcParser *parser);
ArcAstNode *arc_parser_parse_statement(ArcParser *parser);
ArcAstNode *arc_parser_parse_declaration(ArcParser *parser);

// --- Arena Allocator API ---

// Create a new arena with specified default block size
ArcArena *arc_arena_create(size_t default_block_size);

// Destroy arena and free all allocated memory
void arc_arena_destroy(ArcArena *arena);

// Allocate memory from arena (returns NULL on failure)
void *arc_arena_alloc(ArcArena *arena, size_t size);

// Reset arena (keeps first block, frees others, resets allocation pointers)
void arc_arena_reset(ArcArena *arena);

// Get total memory allocated by arena
size_t arc_arena_total_allocated(const ArcArena *arena);

// --- AST Utility Functions ---

// Creates a new AST node with proper source info
// Uses arena allocation if parser has arena, otherwise uses malloc and tracking
ArcAstNode *arc_ast_node_create(ArcParser *parser, ArcAstNodeType type, ArcSourceInfo source_info);

// Traditional create function (uses malloc, adds to tracking)
ArcAstNode *arc_ast_node_create_tracked(ArcParser *parser, ArcAstNodeType type,
                                        ArcSourceLocation location);

// Recursively destroys an AST node and all its children
// Only needed when NOT using arena allocation
// Frees all internal dynamic memory including:
// - String literal values (literal_string.value)
// - Dynamic arrays (arguments, statements, parameters, etc.)
// - Child nodes recursively
void arc_ast_node_destroy(ArcAstNode *node);

// Print AST node for debugging (recursive)
void arc_ast_node_print(const ArcAstNode *node, int indent);

// Convert AST node type to string name
const char *arc_ast_node_type_to_string(ArcAstNodeType type);

// --- Enhanced Error Handling ---

// Check if parser encountered any errors
bool arc_parser_had_error(const ArcParser *parser);

// Get number of diagnostics collected
size_t arc_parser_diagnostic_count(const ArcParser *parser);

// Get diagnostic iterator (returns first diagnostic)
const ArcDiagnostic *arc_parser_get_diagnostics(const ArcParser *parser);

// Reset error state (clears had_error flag, keeps diagnostics)
void arc_parser_reset_error(ArcParser *parser);

// Clear all diagnostics and reset error state
void arc_parser_clear_diagnostics(ArcParser *parser);

// Add a diagnostic to the parser's collection
void arc_parser_add_diagnostic(ArcParser *parser, ArcDiagnosticLevel level,
                               ArcSourceInfo source_info, const char *format, ...);

// Report error and enter panic mode for recovery
void arc_parser_error(ArcParser *parser, const char *format, ...);

// Report error at specific location
void arc_parser_error_at(ArcParser *parser, ArcSourceInfo source_info, const char *format, ...);

// Report warning
void arc_parser_warning(ArcParser *parser, ArcSourceInfo source_info, const char *format, ...);

// Synchronize parser after error (skip tokens until sync point)
void arc_parser_synchronize(ArcParser *parser);

// Helper to create ArcSourceInfo from current token
ArcSourceInfo arc_parser_current_source_info(const ArcParser *parser);

// Helper to create ArcSourceInfo from any token
ArcSourceInfo arc_source_info_from_token(const ArcToken *token, const char *filename);

/*
 * ENHANCED MEMORY MANAGEMENT NOTES:
 *
 * 1. ARENA ALLOCATION (RECOMMENDED):
 *    - Use arc_parser_init() with an ArcArena for optimal performance
 *    - All AST nodes and associated memory allocated from arena
 *    - Cleanup is O(1): just reset/destroy the arena
 *    - No need for individual node destruction or tracking
 *    - Excellent for large programs with many AST nodes
 *    - Memory locality improves cache performance
 *
 * 2. TRADITIONAL ALLOCATION (FALLBACK):
 *    - Use arc_parser_init_simple() or pass NULL arena
 *    - Individual malloc() for each node, tracked in allocated_nodes array
 *    - arc_parser_cleanup() iterates and destroys each node individually
 *    - Higher overhead but more familiar memory model
 *
 * 3. STRING LITERALS:
 *    - literal_string.value is allocated from arena OR heap
 *    - Contains processed string content (escape sequences resolved)
 *    - Arena: freed automatically when arena is reset/destroyed
 *    - Traditional: freed in arc_ast_node_destroy()
 *
 * 4. DYNAMIC ARRAYS:
 *    - All ArcAstNode** fields are dynamically allocated arrays
 *    - Arena: allocated from arena memory
 *    - Traditional: use malloc/realloc, freed in arc_ast_node_destroy()
 *    - Examples: arguments, statements, parameters, fields
 *
 * 5. OPTIONAL FIELDS:
 *    - Fields marked as optional in comments are set to NULL when absent
 *    - Examples: else_branch, initializer, return_type, etc.
 *    - No special cleanup needed for NULL fields
 *
 * 6. TOKEN STORAGE:
 *    - ArcToken fields store the original tokens for location/text info
 *    - Token.start points into lexer's source string (NOT owned by parser)
 *    - Tokens themselves may be copied into arena or allocated_nodes tracking
 *
 * 7. SOURCE INFO:
 *    - ArcSourceInfo provides enhanced location tracking
 *    - filename pointer should be stable (string literal or managed elsewhere)
 *    - source_text points into original source buffer
 *
 * 8. DIAGNOSTICS:
 *    - Diagnostic messages are heap-allocated and owned by parser
 *    - Freed in arc_parser_cleanup() regardless of allocation strategy
 *    - Linked list structure for efficient appending and iteration
 *
 * 9. ERROR RECOVERY:
 *    - synchronizing flag helps skip tokens during error recovery
 *    - max_errors limit prevents infinite error cascades
 *    - Diagnostics preserved even after errors for IDE integration
 *
 * USAGE RECOMMENDATIONS:
 * - Use arena allocation for production parsing of real source files
 * - Use traditional allocation for small tests or when memory tracking is needed
 * - Always check arc_parser_had_error() after parsing
 * - Iterate diagnostics for detailed error reporting
 * - Call arc_parser_synchronize() in error recovery scenarios
 */

#endif  // ARC_PARSER_H
