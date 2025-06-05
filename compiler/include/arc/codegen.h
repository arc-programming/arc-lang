#ifndef ARC_CODEGEN_H
#define ARC_CODEGEN_H

#include "arc/common.h"
#include "arc/parser.h"
#include "arc/semantic.h"
#include <stdio.h>

// Forward declarations
typedef struct ArcCodegen ArcCodegen;
typedef struct ArcCodegenContext ArcCodegenContext;

// --- Code Generation Options ---
typedef struct {
    bool emit_debug_info;      // Generate debug information
    bool optimize_code;        // Enable basic optimizations
    bool emit_line_numbers;    // Emit line number comments
    bool use_safe_arithmetic;  // Use overflow-safe arithmetic
    const char *output_dir;    // Output directory for generated files
    const char *target_name;   // Base name for generated files
} ArcCodegenOptions;

// --- Code Generation Context ---
// Tracks the current state during code generation
typedef struct ArcCodegenContext {
    FILE *output_file;             // Current output file
    int indent_level;              // Current indentation level
    bool in_function;              // Currently inside a function
    bool in_loop;                  // Currently inside a loop
    const char *current_function;  // Current function name

    // Variable tracking
    struct {
        char **names;    // Variable names in current scope
        char **c_names;  // Corresponding C names (mangled)
        size_t count;
        size_t capacity;
    } variables;

    // Function tracking
    struct {
        char **names;       // Function names
        char **signatures;  // Function signatures
        size_t count;
        size_t capacity;
    } functions;

    // Type tracking
    struct {
        char **arc_types;  // Arc type names
        char **c_types;    // Corresponding C type names
        size_t count;
        size_t capacity;
    } types;

    // String literal tracking
    struct {
        char **literals;  // String literals
        char **names;     // Generated constant names
        size_t count;
        size_t capacity;
    } strings;
} ArcCodegenContext;

// --- Main Code Generator ---
typedef struct ArcCodegen {
    ArcCodegenOptions options;
    ArcCodegenContext *context;
    ArcSemanticAnalyzer *semantic_analyzer;  // For type information

    // Output files
    FILE *header_file;  // Generated .h file
    FILE *source_file;  // Generated .c file
    FILE *main_file;    // Generated main.c file (if needed)

    // Error tracking
    bool had_error;
    char *error_message;

    // Generated file names
    char *header_filename;
    char *source_filename;
    char *main_filename;
} ArcCodegen;

// --- Public API ---

// Create a new code generator
ArcCodegen *arc_codegen_create(const ArcCodegenOptions *options);

// Destroy the code generator
void arc_codegen_destroy(ArcCodegen *codegen);

// Generate C code from an AST
bool arc_codegen_generate(ArcCodegen *codegen, ArcAstNode *ast,
                          ArcSemanticAnalyzer *semantic_analyzer);

// Generate C code to a specific directory
bool arc_codegen_generate_to_directory(ArcCodegen *codegen, ArcAstNode *ast,
                                       ArcSemanticAnalyzer *semantic_analyzer,
                                       const char *output_dir);

// Check if code generation encountered errors
bool arc_codegen_has_errors(const ArcCodegen *codegen);

// Get error message
const char *arc_codegen_get_error(const ArcCodegen *codegen);

// --- Utility Functions ---

// Create default codegen options
ArcCodegenOptions arc_codegen_default_options(void);

// Set output directory for generated files
void arc_codegen_set_output_directory(ArcCodegen *codegen, const char *output_dir);

// Set target name for generated files
void arc_codegen_set_target_name(ArcCodegen *codegen, const char *target_name);

// --- Internal API (for modular implementation) ---

// Context management
ArcCodegenContext *arc_codegen_context_create(void);
void arc_codegen_context_destroy(ArcCodegenContext *context);
void arc_codegen_context_reset(ArcCodegenContext *context);

// File output management
bool arc_codegen_open_files(ArcCodegen *codegen);
void arc_codegen_close_files(ArcCodegen *codegen);

// Code generation for specific node types
bool arc_codegen_generate_program(ArcCodegen *codegen, ArcAstNode *program);
bool arc_codegen_generate_function(ArcCodegen *codegen, ArcAstNode *function);
bool arc_codegen_generate_extern_function(ArcCodegen *codegen, ArcAstNode *extern_func);
bool arc_codegen_generate_statement(ArcCodegen *codegen, ArcAstNode *statement);
bool arc_codegen_generate_expression(ArcCodegen *codegen, ArcAstNode *expression);
bool arc_codegen_generate_declaration(ArcCodegen *codegen, ArcAstNode *declaration);

// Type conversion utilities
const char *arc_codegen_convert_type(ArcCodegen *codegen, ArcAstNode *type_node);
const char *arc_codegen_primitive_to_c_type(ArcTokenType primitive_type);

// Name mangling and utilities
char *arc_codegen_mangle_name(const char *arc_name);
char *arc_codegen_generate_string_literal_name(int index);

// Output utilities
void arc_codegen_emit(ArcCodegen *codegen, const char *format, ...);
void arc_codegen_emit_line(ArcCodegen *codegen, const char *format, ...);
void arc_codegen_emit_indent(ArcCodegen *codegen);
void arc_codegen_increase_indent(ArcCodegen *codegen);
void arc_codegen_decrease_indent(ArcCodegen *codegen);

// Header generation
void arc_codegen_emit_header_preamble(ArcCodegen *codegen);
void arc_codegen_emit_header_postamble(ArcCodegen *codegen);

// Source generation
void arc_codegen_emit_source_preamble(ArcCodegen *codegen);
void arc_codegen_emit_source_postamble(ArcCodegen *codegen);

// Error handling
void arc_codegen_set_error(ArcCodegen *codegen, const char *format, ...);

#endif  // ARC_CODEGEN_H
