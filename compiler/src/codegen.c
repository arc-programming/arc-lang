#include "arc/codegen.h"
#include <assert.h>
#include <stdarg.h>
#include <string.h>

// --- Default Options ---

ArcCodegenOptions arc_codegen_default_options(void) {
    ArcCodegenOptions options = {0};
    options.emit_debug_info = true;
    options.optimize_code = false;
    options.emit_line_numbers = true;
    options.use_safe_arithmetic = false;
    options.output_dir = ".";
    options.target_name = "arc_program";
    return options;
}

// --- Context Management ---

ArcCodegenContext *arc_codegen_context_create(void) {
    ArcCodegenContext *context = MALLOC(sizeof(ArcCodegenContext));
    if (!context)
        return NULL;

    memset(context, 0, sizeof(ArcCodegenContext));
    context->indent_level = 0;
    context->in_function = false;
    context->in_loop = false;
    context->current_function = NULL;

    // Initialize dynamic arrays
    context->variables.capacity = 16;
    context->variables.names = MALLOC(sizeof(char *) * context->variables.capacity);
    context->variables.c_names = MALLOC(sizeof(char *) * context->variables.capacity);

    context->functions.capacity = 16;
    context->functions.names = MALLOC(sizeof(char *) * context->functions.capacity);
    context->functions.signatures = MALLOC(sizeof(char *) * context->functions.capacity);

    context->types.capacity = 16;
    context->types.arc_types = MALLOC(sizeof(char *) * context->types.capacity);
    context->types.c_types = MALLOC(sizeof(char *) * context->types.capacity);

    context->strings.capacity = 16;
    context->strings.literals = MALLOC(sizeof(char *) * context->strings.capacity);
    context->strings.names = MALLOC(sizeof(char *) * context->strings.capacity);

    if (!context->variables.names || !context->variables.c_names || !context->functions.names ||
        !context->functions.signatures || !context->types.arc_types || !context->types.c_types ||
        !context->strings.literals || !context->strings.names) {
        arc_codegen_context_destroy(context);
        return NULL;
    }

    return context;
}

void arc_codegen_context_destroy(ArcCodegenContext *context) {
    if (!context)
        return;

    // Free variable names
    for (size_t i = 0; i < context->variables.count; i++) {
        FREE(context->variables.names[i]);
        FREE(context->variables.c_names[i]);
    }
    FREE(context->variables.names);
    FREE(context->variables.c_names);

    // Free function names and signatures
    for (size_t i = 0; i < context->functions.count; i++) {
        FREE(context->functions.names[i]);
        FREE(context->functions.signatures[i]);
    }
    FREE(context->functions.names);
    FREE(context->functions.signatures);

    // Free type names
    for (size_t i = 0; i < context->types.count; i++) {
        FREE(context->types.arc_types[i]);
        FREE(context->types.c_types[i]);
    }
    FREE(context->types.arc_types);
    FREE(context->types.c_types);

    // Free string literals and names
    for (size_t i = 0; i < context->strings.count; i++) {
        FREE(context->strings.literals[i]);
        FREE(context->strings.names[i]);
    }
    FREE(context->strings.literals);
    FREE(context->strings.names);

    FREE(context);
}

void arc_codegen_context_reset(ArcCodegenContext *context) {
    if (!context)
        return;

    context->indent_level = 0;
    context->in_function = false;
    context->in_loop = false;
    context->current_function = NULL;

    // Clear but don't free arrays (reuse capacity)
    for (size_t i = 0; i < context->variables.count; i++) {
        FREE(context->variables.names[i]);
        FREE(context->variables.c_names[i]);
    }
    context->variables.count = 0;

    for (size_t i = 0; i < context->functions.count; i++) {
        FREE(context->functions.names[i]);
        FREE(context->functions.signatures[i]);
    }
    context->functions.count = 0;

    for (size_t i = 0; i < context->types.count; i++) {
        FREE(context->types.arc_types[i]);
        FREE(context->types.c_types[i]);
    }
    context->types.count = 0;

    for (size_t i = 0; i < context->strings.count; i++) {
        FREE(context->strings.literals[i]);
        FREE(context->strings.names[i]);
    }
    context->strings.count = 0;
}

// --- Codegen Creation/Destruction ---

ArcCodegen *arc_codegen_create(const ArcCodegenOptions *options) {
    ArcCodegen *codegen = MALLOC(sizeof(ArcCodegen));
    if (!codegen)
        return NULL;

    memset(codegen, 0, sizeof(ArcCodegen));

    // Copy options or use defaults
    if (options) {
        codegen->options = *options;
    } else {
        codegen->options = arc_codegen_default_options();
    }

    // Create context
    codegen->context = arc_codegen_context_create();
    if (!codegen->context) {
        FREE(codegen);
        return NULL;
    }

    codegen->had_error = false;
    codegen->error_message = NULL;

    return codegen;
}

void arc_codegen_destroy(ArcCodegen *codegen) {
    if (!codegen)
        return;

    arc_codegen_close_files(codegen);
    arc_codegen_context_destroy(codegen->context);

    FREE(codegen->error_message);
    FREE(codegen->header_filename);
    FREE(codegen->source_filename);
    FREE(codegen->main_filename);

    FREE(codegen);
}

// --- Error Handling ---

void arc_codegen_set_error(ArcCodegen *codegen, const char *format, ...) {
    if (!codegen)
        return;

    codegen->had_error = true;

    if (codegen->error_message) {
        FREE(codegen->error_message);
    }

    va_list args;
    va_start(args, format);

    // Calculate required size
    int size = vsnprintf(NULL, 0, format, args);
    va_end(args);

    if (size <= 0) {
        codegen->error_message = arc_strdup("Unknown codegen error");
        return;
    }

    codegen->error_message = MALLOC(size + 1);
    if (!codegen->error_message) {
        return;
    }

    va_start(args, format);
    vsnprintf(codegen->error_message, size + 1, format, args);
    va_end(args);
}

bool arc_codegen_has_errors(const ArcCodegen *codegen) {
    return codegen ? codegen->had_error : true;
}

const char *arc_codegen_get_error(const ArcCodegen *codegen) {
    return codegen ? codegen->error_message : "Invalid codegen instance";
}

// --- File Management ---

bool arc_codegen_open_files(ArcCodegen *codegen) {
    if (!codegen)
        return false;

    // Generate filenames
    size_t dir_len = strlen(codegen->options.output_dir);
    size_t name_len = strlen(codegen->options.target_name);
    size_t path_len = dir_len + name_len + 20;  // Extra space for extensions and separators

    // Header file
    codegen->header_filename = MALLOC(path_len);
    if (!codegen->header_filename) {
        arc_codegen_set_error(codegen, "Failed to allocate memory for header filename");
        return false;
    }
    snprintf(codegen->header_filename, path_len, "%s%c%s.h", codegen->options.output_dir,
             PATH_SEPARATOR, codegen->options.target_name);

    // Source file
    codegen->source_filename = MALLOC(path_len);
    if (!codegen->source_filename) {
        arc_codegen_set_error(codegen, "Failed to allocate memory for source filename");
        return false;
    }
    snprintf(codegen->source_filename, path_len, "%s%c%s.c", codegen->options.output_dir,
             PATH_SEPARATOR, codegen->options.target_name);

    // Main file (if needed)
    codegen->main_filename = MALLOC(path_len);
    if (!codegen->main_filename) {
        arc_codegen_set_error(codegen, "Failed to allocate memory for main filename");
        return false;
    }
    snprintf(codegen->main_filename, path_len, "%s%cmain.c", codegen->options.output_dir,
             PATH_SEPARATOR);

    // Open files
    codegen->header_file = fopen(codegen->header_filename, "w");
    if (!codegen->header_file) {
        arc_codegen_set_error(codegen, "Failed to open header file: %s", codegen->header_filename);
        return false;
    }

    codegen->source_file = fopen(codegen->source_filename, "w");
    if (!codegen->source_file) {
        arc_codegen_set_error(codegen, "Failed to open source file: %s", codegen->source_filename);
        return false;
    }

    codegen->main_file = fopen(codegen->main_filename, "w");
    if (!codegen->main_file) {
        arc_codegen_set_error(codegen, "Failed to open main file: %s", codegen->main_filename);
        return false;
    }

    return true;
}

void arc_codegen_close_files(ArcCodegen *codegen) {
    if (!codegen)
        return;

    if (codegen->header_file) {
        fclose(codegen->header_file);
        codegen->header_file = NULL;
    }

    if (codegen->source_file) {
        fclose(codegen->source_file);
        codegen->source_file = NULL;
    }

    if (codegen->main_file) {
        fclose(codegen->main_file);
        codegen->main_file = NULL;
    }
}

// --- Output Utilities ---

void arc_codegen_emit(ArcCodegen *codegen, const char *format, ...) {
    if (!codegen || !codegen->context->output_file)
        return;

    va_list args;
    va_start(args, format);
    vfprintf(codegen->context->output_file, format, args);
    va_end(args);
}

void arc_codegen_emit_line(ArcCodegen *codegen, const char *format, ...) {
    if (!codegen || !codegen->context->output_file)
        return;

    arc_codegen_emit_indent(codegen);

    va_list args;
    va_start(args, format);
    vfprintf(codegen->context->output_file, format, args);
    va_end(args);

    fprintf(codegen->context->output_file, "\n");
}

void arc_codegen_emit_indent(ArcCodegen *codegen) {
    if (!codegen || !codegen->context->output_file)
        return;

    for (int i = 0; i < codegen->context->indent_level; i++) {
        fprintf(codegen->context->output_file, "    ");  // 4 spaces per indent
    }
}

void arc_codegen_increase_indent(ArcCodegen *codegen) {
    if (codegen && codegen->context) {
        codegen->context->indent_level++;
    }
}

void arc_codegen_decrease_indent(ArcCodegen *codegen) {
    if (codegen && codegen->context && codegen->context->indent_level > 0) {
        codegen->context->indent_level--;
    }
}

// --- Type Conversion ---

const char *arc_codegen_primitive_to_c_type(ArcTokenType primitive_type) {
    switch (primitive_type) {
        case TOKEN_KEYWORD_I8:
            return "int8_t";
        case TOKEN_KEYWORD_I16:
            return "int16_t";
        case TOKEN_KEYWORD_I32:
            return "int32_t";
        case TOKEN_KEYWORD_I64:
            return "int64_t";
        case TOKEN_KEYWORD_U8:
            return "uint8_t";
        case TOKEN_KEYWORD_U16:
            return "uint16_t";
        case TOKEN_KEYWORD_U32:
            return "uint32_t";
        case TOKEN_KEYWORD_U64:
            return "uint64_t";
        case TOKEN_KEYWORD_F32:
            return "float";
        case TOKEN_KEYWORD_F64:
            return "double";
        case TOKEN_KEYWORD_BOOL:
            return "bool";
        case TOKEN_KEYWORD_CHAR:
            return "char";
        default:
            return "void";
    }
}

const char *arc_codegen_convert_type(ArcCodegen *codegen, ArcAstNode *type_node) {
    if (!codegen || !type_node)
        return "void";

    switch (type_node->type) {
        case AST_TYPE_PRIMITIVE:
            return arc_codegen_primitive_to_c_type(type_node->type_primitive.primitive_type);

        case AST_TYPE_POINTER: {
            const char *pointed_type =
                arc_codegen_convert_type(codegen, type_node->type_pointer.pointed_type);
            // This is a simplification - in a real implementation, we'd need to handle
            // complex pointer types more carefully
            static char pointer_type[256];
            snprintf(pointer_type, sizeof(pointer_type), "%s*", pointed_type);
            return pointer_type;
        }

        case AST_TYPE_ARRAY: {
            const char *element_type =
                arc_codegen_convert_type(codegen, type_node->type_array.element_type);
            // For now, treat arrays as pointers
            static char array_type[256];
            snprintf(array_type, sizeof(array_type), "%s*", element_type);
            return array_type;
        }

        default:
            return "void";
    }
}

// --- Name Mangling ---

char *arc_codegen_mangle_name(const char *arc_name) {
    if (!arc_name)
        return NULL;

    // Simple mangling: just prefix with "arc_"
    size_t len = strlen(arc_name) + 5;  // "arc_" + name + null terminator
    char *mangled = MALLOC(len);
    if (!mangled)
        return NULL;

    snprintf(mangled, len, "arc_%s", arc_name);
    return mangled;
}

char *arc_codegen_generate_string_literal_name(int index) {
    char *name = MALLOC(32);
    if (!name)
        return NULL;

    snprintf(name, 32, "arc_string_%d", index);
    return name;
}

// --- Header/Source Preambles ---

void arc_codegen_emit_header_preamble(ArcCodegen *codegen) {
    if (!codegen || !codegen->header_file)
        return;

    codegen->context->output_file = codegen->header_file;

    arc_codegen_emit_line(codegen, "// Generated by Arc Language Compiler");
    arc_codegen_emit_line(codegen, "// Do not edit this file manually");
    arc_codegen_emit_line(codegen, "");

    // Header guard
    arc_codegen_emit_line(codegen, "#ifndef ARC_GENERATED_H");
    arc_codegen_emit_line(codegen, "#define ARC_GENERATED_H");
    arc_codegen_emit_line(codegen, "");

    // Standard includes
    arc_codegen_emit_line(codegen, "#include <stdint.h>");
    arc_codegen_emit_line(codegen, "#include <stdbool.h>");
    arc_codegen_emit_line(codegen, "#include <stdlib.h>");
    arc_codegen_emit_line(codegen, "#include <string.h>");
    arc_codegen_emit_line(codegen, "");

    // C++ compatibility
    arc_codegen_emit_line(codegen, "#ifdef __cplusplus");
    arc_codegen_emit_line(codegen, "extern \"C\" {");
    arc_codegen_emit_line(codegen, "#endif");
    arc_codegen_emit_line(codegen, "");
}

void arc_codegen_emit_header_postamble(ArcCodegen *codegen) {
    if (!codegen || !codegen->header_file)
        return;

    codegen->context->output_file = codegen->header_file;

    arc_codegen_emit_line(codegen, "");
    arc_codegen_emit_line(codegen, "#ifdef __cplusplus");
    arc_codegen_emit_line(codegen, "}");
    arc_codegen_emit_line(codegen, "#endif");
    arc_codegen_emit_line(codegen, "");
    arc_codegen_emit_line(codegen, "#endif // ARC_GENERATED_H");
}

void arc_codegen_emit_source_preamble(ArcCodegen *codegen) {
    if (!codegen || !codegen->source_file)
        return;

    codegen->context->output_file = codegen->source_file;

    arc_codegen_emit_line(codegen, "// Generated by Arc Language Compiler");
    arc_codegen_emit_line(codegen, "// Do not edit this file manually");
    arc_codegen_emit_line(codegen, "");

    // Include the generated header
    arc_codegen_emit_line(codegen, "#include \"%s.h\"", codegen->options.target_name);
    arc_codegen_emit_line(codegen, "#include <stdio.h>");
    arc_codegen_emit_line(codegen, "");
}

void arc_codegen_emit_source_postamble(ArcCodegen *codegen) {
    if (!codegen || !codegen->source_file)
        return;

    codegen->context->output_file = codegen->source_file;
    arc_codegen_emit_line(codegen, "");
    arc_codegen_emit_line(codegen, "// End of generated code");
}

// --- Set Configuration ---

void arc_codegen_set_output_directory(ArcCodegen *codegen, const char *output_dir) {
    if (codegen && output_dir) {
        codegen->options.output_dir = output_dir;
    }
}

void arc_codegen_set_target_name(ArcCodegen *codegen, const char *target_name) {
    if (codegen && target_name) {
        codegen->options.target_name = target_name;
    }
}
