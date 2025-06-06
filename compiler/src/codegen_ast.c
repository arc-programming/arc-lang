#include "arc/codegen.h"
#include <assert.h>
#include <stdarg.h>
#include <string.h>

// --- Forward Declarations ---
static bool codegen_generate_expression_internal(ArcCodegen *codegen, ArcAstNode *expr);
static bool codegen_generate_statement_internal(ArcCodegen *codegen, ArcAstNode *stmt);
static bool codegen_generate_declaration_internal(ArcCodegen *codegen, ArcAstNode *decl);
static const char *arc_codegen_convert_type_info(ArcCodegen *codegen, ArcTypeInfo *type_info);

// Helper function to check if a function is a standard C library function
static bool is_c_stdlib_function(const char *func_name) {
    // List of common C standard library functions that shouldn't be redeclared
    static const char *stdlib_functions[] = {
        "printf",  "fprintf", "sprintf", "snprintf", "scanf",   "fscanf",  "sscanf",  "puts",
        "fputs",   "putchar", "fputc",   "getchar",  "fgetc",   "gets",    "fgets",   "malloc",
        "calloc",  "realloc", "free",    "strlen",   "strcpy",  "strncpy", "strcat",  "strncat",
        "strcmp",  "strncmp", "strchr",  "strrchr",  "strstr",  "strtok",  "strspn",  "strcspn",
        "memcpy",  "memmove", "memset",  "memcmp",   "memchr",  "fopen",   "fclose",  "fread",
        "fwrite",  "fseek",   "ftell",   "rewind",   "fflush",  "sin",     "cos",     "tan",
        "asin",    "acos",    "atan",    "atan2",    "sinh",    "cosh",    "tanh",    "exp",
        "log",     "log10",   "pow",     "sqrt",     "ceil",    "floor",   "fabs",    "fmod",
        "frexp",   "ldexp",   "modf",    "rand",     "srand",   "abs",     "labs",    "div",
        "ldiv",    "atoi",    "atol",    "atof",     "strtol",  "strtoul", "strtod",  "exit",
        "abort",   "atexit",  "system",  "getenv",   "isalnum", "isalpha", "isdigit", "islower",
        "isupper", "isspace", "tolower", "toupper",  NULL};

    for (size_t i = 0; stdlib_functions[i] != NULL; i++) {
        if (strcmp(func_name, stdlib_functions[i]) == 0) {
            return true;
        }
    }
    return false;
}

static const char *arc_codegen_convert_type_info(ArcCodegen *codegen, ArcTypeInfo *type_info) {
    if (!type_info)
        return "void";  // Default fallback

    switch (type_info->kind) {
        case ARC_TYPE_PRIMITIVE:
            return arc_codegen_primitive_to_c_type(type_info->primitive.primitive_type);
        case ARC_TYPE_POINTER: {
            const char *pointed_type =
                arc_codegen_convert_type_info(codegen, type_info->pointer.pointed_type);
            static char buffer[256];
            snprintf(buffer, sizeof(buffer), "%s*", pointed_type);
            return buffer;
        }
        // TODO: Add other types like array, slice, etc. as needed
        default:
            return "void";
    }
}

// --- Main Generation Entry Points ---

bool arc_codegen_generate(ArcCodegen *codegen, ArcAstNode *ast,
                          ArcSemanticAnalyzer *semantic_analyzer) {
    if (!codegen || !ast) {
        if (codegen)
            arc_codegen_set_error(codegen, "Invalid AST provided to codegen");
        return false;
    }

    codegen->semantic_analyzer = semantic_analyzer;

    // Open output files
    if (!arc_codegen_open_files(codegen)) {
        return false;
    }

    // Generate header and source preambles
    arc_codegen_emit_header_preamble(codegen);
    arc_codegen_emit_source_preamble(codegen);

    // Generate the program
    bool success = arc_codegen_generate_program(codegen, ast);

    if (success) {
        // Generate postambles
        arc_codegen_emit_header_postamble(codegen);
        arc_codegen_emit_source_postamble(codegen);
    }

    arc_codegen_close_files(codegen);
    return success;
}

bool arc_codegen_generate_to_directory(ArcCodegen *codegen, ArcAstNode *ast,
                                       ArcSemanticAnalyzer *semantic_analyzer,
                                       const char *output_dir) {
    if (!codegen)
        return false;

    // Set output directory
    arc_codegen_set_output_directory(codegen, output_dir);

    return arc_codegen_generate(codegen, ast, semantic_analyzer);
}

// --- Program Generation ---

bool arc_codegen_generate_program(ArcCodegen *codegen, ArcAstNode *program) {
    if (!codegen || !program || program->type != AST_PROGRAM) {
        arc_codegen_set_error(codegen, "Invalid program node");
        return false;
    }

    // First pass: collect all string literals and generate constants
    for (size_t i = 0; i < program->program.declaration_count; i++) {
        // TODO: Implement string literal collection
    }

    // Generate string literal constants in header
    if (codegen->context->strings.count > 0) {
        codegen->context->output_file = codegen->header_file;
        arc_codegen_emit_line(codegen, "// String literals");

        for (size_t i = 0; i < codegen->context->strings.count; i++) {
            arc_codegen_emit_line(codegen, "extern const char* %s;",
                                  codegen->context->strings.names[i]);
        }
        arc_codegen_emit_line(codegen, "");

        // Generate string literal definitions in source
        codegen->context->output_file = codegen->source_file;
        arc_codegen_emit_line(codegen, "// String literal definitions");

        for (size_t i = 0; i < codegen->context->strings.count; i++) {
            arc_codegen_emit_line(codegen, "const char* %s = \"%s\";",
                                  codegen->context->strings.names[i],
                                  codegen->context->strings.literals[i]);
        }
        arc_codegen_emit_line(codegen, "");
    }
    // Second pass: generate function declarations in header
    codegen->context->output_file = codegen->header_file;
    arc_codegen_emit_line(codegen, "// Function declarations");

    for (size_t i = 0; i < program->program.declaration_count; i++) {
        ArcAstNode *decl = program->program.declarations[i];
        if (decl->type == AST_DECL_FUNCTION) {
            // Generate function declaration in header (prototype only)
            const char *return_type = "void";
            if (decl->function_decl.return_type) {
                return_type = arc_codegen_convert_type(codegen, decl->function_decl.return_type);
            }
            char *func_name_temp = MALLOC(decl->function_decl.name.length + 1);
            if (!func_name_temp) {
                arc_codegen_set_error(codegen, "Failed to allocate memory for function name");
                return false;
            }
            strncpy(func_name_temp, decl->function_decl.name.start,
                    decl->function_decl.name.length);
            func_name_temp[decl->function_decl.name.length] = '\0';

            char *func_name = arc_codegen_mangle_name(func_name_temp);
            FREE(func_name_temp);
            if (!func_name) {
                arc_codegen_set_error(codegen, "Failed to mangle function name");
                return false;
            }

            arc_codegen_emit(codegen, "%s %s(", return_type, func_name);

            // Generate parameters
            if (decl->function_decl.parameter_count == 0) {
                arc_codegen_emit(codegen, "void");
            } else {
                for (size_t j = 0; j < decl->function_decl.parameter_count; j++) {
                    if (j > 0)
                        arc_codegen_emit(codegen, ", ");
                    ArcAstNode *param = decl->function_decl.parameters[j];
                    const char *param_type =
                        arc_codegen_convert_type(codegen, param->parameter.type_annotation);

                    char *param_name_temp = MALLOC(param->parameter.name.length + 1);
                    if (!param_name_temp) {
                        FREE(func_name);
                        arc_codegen_set_error(codegen,
                                              "Failed to allocate memory for parameter name");
                        return false;
                    }
                    strncpy(param_name_temp, param->parameter.name.start,
                            param->parameter.name.length);
                    param_name_temp[param->parameter.name.length] = '\0';

                    char *param_name = arc_codegen_mangle_name(param_name_temp);
                    FREE(param_name_temp);
                    if (!param_name) {
                        FREE(func_name);
                        arc_codegen_set_error(codegen, "Failed to mangle parameter name");
                        return false;
                    }

                    arc_codegen_emit(codegen, "%s %s", param_type, param_name);

                    FREE(param_name);
                }
            }

            arc_codegen_emit(codegen, ");\n");
            FREE(func_name);
        }
    }
    arc_codegen_emit_line(codegen, "");

    // Third pass: generate function implementations in source
    codegen->context->output_file = codegen->source_file;
    arc_codegen_emit_line(codegen, "// Function implementations");

    bool has_main = false;
    for (size_t i = 0; i < program->program.declaration_count; i++) {
        ArcAstNode *decl = program->program.declarations[i];
        if (!arc_codegen_generate_declaration(codegen, decl)) {
            return false;
        }

        // Check if this is a main function
        if (decl->type == AST_DECL_FUNCTION) {
            const char *name = decl->function_decl.name.start;
            if (strncmp(name, "main", 4) == 0 && decl->function_decl.name.length == 4) {
                has_main = true;
            }
        }
    }

    // Generate main.c if there's a main function
    if (has_main) {
        codegen->context->output_file = codegen->main_file;

        arc_codegen_emit_line(codegen, "// Generated main.c");
        arc_codegen_emit_line(codegen, "#include \"%s.h\"", codegen->options.target_name);
        arc_codegen_emit_line(codegen, "");
        arc_codegen_emit_line(codegen, "int main(int argc, char* argv[]) {");
        arc_codegen_increase_indent(codegen);
        arc_codegen_emit_line(codegen, "return arc_main();");
        arc_codegen_decrease_indent(codegen);
        arc_codegen_emit_line(codegen, "}");
    }

    return true;
}

// --- Declaration Generation ---

bool arc_codegen_generate_declaration(ArcCodegen *codegen, ArcAstNode *declaration) {
    if (!codegen || !declaration) {
        arc_codegen_set_error(codegen, "Invalid declaration node");
        return false;
    }

    return codegen_generate_declaration_internal(codegen, declaration);
}

static bool codegen_generate_declaration_internal(ArcCodegen *codegen, ArcAstNode *decl) {
    switch (decl->type) {
        case AST_DECL_FUNCTION:
            return arc_codegen_generate_function(codegen, decl);

        case AST_DECL_EXTERN:
            return arc_codegen_generate_extern_function(codegen, decl);

        // --- START FIX ---
        // Teach the codegen that these statements are valid at the top level.
        case AST_STMT_VAR_DECL:
        case AST_STMT_CONST_DECL:
            // Delegate to the existing statement generator.
            return arc_codegen_generate_statement(codegen, decl);
            // --- END FIX ---

        case AST_DECL_USE:
            return true;  // No code gen needed

        case AST_DECL_MODULE:
            return true;  // No code gen needed

        default:
            arc_codegen_set_error(codegen, "Unsupported declaration type: %d", decl->type);
            return false;
    }
}

// --- Extern Function Generation ---

bool arc_codegen_generate_extern_function(ArcCodegen *codegen, ArcAstNode *extern_func) {
    if (!codegen || !extern_func || extern_func->type != AST_DECL_EXTERN) {
        arc_codegen_set_error(codegen, "Invalid extern function node");
        return false;
    }

    // Get return type (default to void if not specified)
    const char *return_type = "void";
    if (extern_func->extern_decl.return_type) {
        return_type = arc_codegen_convert_type(codegen, extern_func->extern_decl.return_type);
    }

    // Extract function name
    char *func_name_temp = MALLOC(extern_func->extern_decl.name.length + 1);
    if (!func_name_temp) {
        arc_codegen_set_error(codegen, "Failed to allocate memory for extern function name");
        return false;
    }
    strncpy(func_name_temp, extern_func->extern_decl.name.start,
            extern_func->extern_decl.name.length);
    func_name_temp[extern_func->extern_decl.name.length] = '\0';

    // For extern functions, use the original name (no mangling) unless c_name is specified
    char *func_name = NULL;
    if (extern_func->extern_decl.c_name && extern_func->extern_decl.c_name[0] != '\0') {
        // Use the specified C name
        size_t c_name_len = strlen(extern_func->extern_decl.c_name);
        func_name = MALLOC(c_name_len + 1);
        if (func_name) {
            strcpy(func_name, extern_func->extern_decl.c_name);
        }
    } else {
        // Use the Arc function name as-is for C linkage
        func_name = MALLOC(strlen(func_name_temp) + 1);
        if (func_name) {
            strcpy(func_name, func_name_temp);
        }
    }
    FREE(func_name_temp);
    if (!func_name) {
        arc_codegen_set_error(codegen, "Failed to allocate function name");
        return false;
    }

    // Check if this is a C standard library function - if so, skip generating the extern
    // declaration
    if (is_c_stdlib_function(func_name)) {
        // Skip generating extern declaration for C stdlib functions
        // They are already available through standard headers
        FREE(func_name);
        return true;
    }

    // Generate extern function declaration
    arc_codegen_emit(codegen, "extern %s %s(", return_type, func_name);

    // Generate parameters
    if (extern_func->extern_decl.parameter_count > 0) {
        for (size_t i = 0; i < extern_func->extern_decl.parameter_count; i++) {
            if (i > 0) {
                arc_codegen_emit(codegen, ", ");
            }

            ArcAstNode *param = extern_func->extern_decl.parameters[i];
            if (param->type == AST_PARAMETER) {
                const char *param_type = "int";  // Default type
                if (param->parameter.type_annotation) {
                    param_type =
                        arc_codegen_convert_type(codegen, param->parameter.type_annotation);
                }

                // Extract parameter name
                char *param_name_temp = MALLOC(param->parameter.name.length + 1);
                if (!param_name_temp) {
                    arc_codegen_set_error(codegen, "Failed to allocate memory for parameter name");
                    FREE(func_name);
                    return false;
                }
                strncpy(param_name_temp, param->parameter.name.start, param->parameter.name.length);
                param_name_temp[param->parameter.name.length] = '\0';

                arc_codegen_emit(codegen, "%s %s", param_type, param_name_temp);
                FREE(param_name_temp);
            }
        }
    } else {
        arc_codegen_emit(codegen, "void");
    }

    arc_codegen_emit_line(codegen, ");");
    arc_codegen_emit_line(codegen, "");

    FREE(func_name);
    return true;
}

// --- Function Generation ---

bool arc_codegen_generate_function(ArcCodegen *codegen, ArcAstNode *function) {
    if (!codegen || !function || function->type != AST_DECL_FUNCTION) {
        arc_codegen_set_error(codegen, "Invalid function node");
        return false;
    }

    const char *return_type = "void";
    if (function->function_decl.return_type) {
        return_type = arc_codegen_convert_type(codegen, function->function_decl.return_type);
    }
    char *func_name_temp = MALLOC(function->function_decl.name.length + 1);
    if (!func_name_temp) {
        arc_codegen_set_error(codegen, "Failed to allocate memory for function name");
        return false;
    }
    strncpy(func_name_temp, function->function_decl.name.start,
            function->function_decl.name.length);
    func_name_temp[function->function_decl.name.length] = '\0';

    char *func_name = arc_codegen_mangle_name(func_name_temp);
    FREE(func_name_temp);
    if (!func_name) {
        arc_codegen_set_error(codegen, "Failed to mangle function name");
        return false;
    }  // Set context
    codegen->context->in_function = true;
    codegen->context->current_function = func_name;  // Generate function signature
    arc_codegen_emit(codegen, "%s %s(", return_type, func_name);

    // Generate parameters
    if (function->function_decl.parameter_count == 0) {
        arc_codegen_emit(codegen, "void");
    } else {
        for (size_t i = 0; i < function->function_decl.parameter_count; i++) {
            if (i > 0)
                arc_codegen_emit(codegen, ", ");
            ArcAstNode *param = function->function_decl.parameters[i];
            const char *param_type =
                arc_codegen_convert_type(codegen, param->parameter.type_annotation);

            char *param_name_temp = MALLOC(param->parameter.name.length + 1);
            if (!param_name_temp) {
                FREE(func_name);
                arc_codegen_set_error(codegen, "Failed to allocate memory for parameter name");
                return false;
            }
            strncpy(param_name_temp, param->parameter.name.start, param->parameter.name.length);
            param_name_temp[param->parameter.name.length] = '\0';

            char *param_name = arc_codegen_mangle_name(param_name_temp);
            FREE(param_name_temp);
            if (!param_name) {
                FREE(func_name);
                arc_codegen_set_error(codegen, "Failed to mangle parameter name");
                return false;
            }

            arc_codegen_emit(codegen, "%s %s", param_type, param_name);

            FREE(param_name);
        }
    }

    arc_codegen_emit(codegen, ") {\n");
    arc_codegen_increase_indent(codegen);

    // Generate function body
    bool success = true;
    if (function->function_decl.body) {
        success = arc_codegen_generate_statement(codegen, function->function_decl.body);
    }

    arc_codegen_decrease_indent(codegen);
    arc_codegen_emit_line(codegen, "}");
    arc_codegen_emit_line(codegen, "");

    // Reset context
    codegen->context->in_function = false;
    codegen->context->current_function = NULL;

    FREE(func_name);
    return success;
}

// --- Statement Generation ---

bool arc_codegen_generate_statement(ArcCodegen *codegen, ArcAstNode *statement) {
    if (!codegen || !statement) {
        arc_codegen_set_error(codegen, "Invalid statement node");
        return false;
    }

    return codegen_generate_statement_internal(codegen, statement);
}

static bool codegen_generate_statement_internal(ArcCodegen *codegen, ArcAstNode *stmt) {
    if (codegen->options.emit_line_numbers && stmt->source_info.location.line > 0) {
        arc_codegen_emit_line(codegen, "// Line %zu", stmt->source_info.location.line);
    }

    switch (stmt->type) {
        case AST_STMT_BLOCK: {
            for (size_t i = 0; i < stmt->block_stmt.statement_count; i++) {
                if (!arc_codegen_generate_statement(codegen, stmt->block_stmt.statements[i])) {
                    return false;
                }
            }
            return true;
        }
        case AST_STMT_VAR_DECL: {
            // Generate variable declaration
            const char *var_type = "int";  // Default type
            if (stmt->var_decl_stmt.type_annotation) {
                var_type = arc_codegen_convert_type(codegen, stmt->var_decl_stmt.type_annotation);
            }
            char *var_name_temp = MALLOC(stmt->var_decl_stmt.name.length + 1);
            if (!var_name_temp) {
                arc_codegen_set_error(codegen, "Failed to allocate memory for variable name");
                return false;
            }
            strncpy(var_name_temp, stmt->var_decl_stmt.name.start, stmt->var_decl_stmt.name.length);
            var_name_temp[stmt->var_decl_stmt.name.length] = '\0';

            char *var_name = arc_codegen_mangle_name(var_name_temp);
            FREE(var_name_temp);
            if (!var_name) {
                arc_codegen_set_error(codegen, "Failed to mangle variable name");
                return false;
            }

            arc_codegen_emit(codegen, "%s %s", var_type, var_name);

            if (stmt->var_decl_stmt.initializer) {
                arc_codegen_emit(codegen, " = ");
                if (!arc_codegen_generate_expression(codegen, stmt->var_decl_stmt.initializer)) {
                    FREE(var_name);
                    return false;
                }
            }

            arc_codegen_emit(codegen, ";\n");
            FREE(var_name);
            return true;
        }

        case AST_STMT_CONST_DECL: {
            // --- START FIX ---
            char *const_name_temp = MALLOC(stmt->const_decl_stmt.name.length + 1);
            // ... (your existing code to copy the name)
            strncpy(const_name_temp, stmt->const_decl_stmt.name.start,
                    stmt->const_decl_stmt.name.length);
            const_name_temp[stmt->const_decl_stmt.name.length] = '\0';

            // Look up the symbol in the semantic analyzer to get its real type
            ArcSymbol *symbol = arc_scope_lookup_symbol_recursive(
                codegen->semantic_analyzer->current_scope, const_name_temp);

            const char *const_type = "int";  // Default
            if (symbol && symbol->type) {
                const_type = arc_codegen_convert_type_info(codegen, symbol->type);
            }

            char *const_name = arc_codegen_mangle_name(const_name_temp);
            FREE(const_name_temp);
            if (!const_name) {
                arc_codegen_set_error(codegen, "Failed to mangle constant name");
                return false;
            }
            // Emit the constant declaration
            arc_codegen_emit(codegen, "const %s %s", const_type, const_name);
            // --- END FIX ---

            if (stmt->const_decl_stmt.initializer) {
                arc_codegen_emit(codegen, " = ");
                if (!arc_codegen_generate_expression(codegen, stmt->const_decl_stmt.initializer)) {
                    FREE(const_name);
                    return false;
                }
            }

            arc_codegen_emit(codegen, ";\n");
            FREE(const_name);
            return true;
        }

        case AST_STMT_EXPRESSION: {
            if (!arc_codegen_generate_expression(codegen, stmt->expr_stmt.expression)) {
                return false;
            }
            arc_codegen_emit(codegen, ";\n");
            return true;
        }

        case AST_STMT_RETURN: {
            arc_codegen_emit(codegen, "return");
            if (stmt->return_stmt.value) {
                arc_codegen_emit(codegen, " ");
                if (!arc_codegen_generate_expression(codegen, stmt->return_stmt.value)) {
                    return false;
                }
            }
            arc_codegen_emit(codegen, ";\n");
            return true;
        }

        case AST_STMT_IF: {
            arc_codegen_emit(codegen, "if (");
            if (!arc_codegen_generate_expression(codegen, stmt->if_stmt.condition)) {
                return false;
            }
            arc_codegen_emit(codegen, ") {\n");
            arc_codegen_increase_indent(codegen);

            if (!arc_codegen_generate_statement(codegen, stmt->if_stmt.then_branch)) {
                return false;
            }

            arc_codegen_decrease_indent(codegen);
            arc_codegen_emit_line(codegen, "}");

            if (stmt->if_stmt.else_branch) {
                arc_codegen_emit_line(codegen, "else {");
                arc_codegen_increase_indent(codegen);

                if (!arc_codegen_generate_statement(codegen, stmt->if_stmt.else_branch)) {
                    return false;
                }

                arc_codegen_decrease_indent(codegen);
                arc_codegen_emit_line(codegen, "}");
            }

            return true;
        }

        case AST_STMT_WHILE: {
            codegen->context->in_loop = true;

            arc_codegen_emit(codegen, "while (");
            if (!arc_codegen_generate_expression(codegen, stmt->while_stmt.condition)) {
                codegen->context->in_loop = false;
                return false;
            }
            arc_codegen_emit(codegen, ") {\n");
            arc_codegen_increase_indent(codegen);

            if (!arc_codegen_generate_statement(codegen, stmt->while_stmt.body)) {
                codegen->context->in_loop = false;
                return false;
            }

            arc_codegen_decrease_indent(codegen);
            arc_codegen_emit_line(codegen, "}");

            codegen->context->in_loop = false;
            return true;
        }
        default:
            arc_codegen_set_error(codegen, "Unsupported statement type: %d", stmt->type);
            return false;
    }
}

// --- Expression Generation ---

bool arc_codegen_generate_expression(ArcCodegen *codegen, ArcAstNode *expression) {
    if (!codegen || !expression) {
        arc_codegen_set_error(codegen, "Invalid expression node");
        return false;
    }

    return codegen_generate_expression_internal(codegen, expression);
}

static bool codegen_generate_expression_internal(ArcCodegen *codegen, ArcAstNode *expr) {
    switch (expr->type) {
        case AST_LITERAL_INT:
            arc_codegen_emit(codegen, "%lld", expr->literal_int.value);
            return true;

        case AST_LITERAL_FLOAT:
            arc_codegen_emit(codegen, "%g", expr->literal_float.value);
            return true;

        case AST_LITERAL_STRING: {
            // For now, emit string literals directly
            // In a more complete implementation, we'd use the string constant system
            arc_codegen_emit(codegen, "\"%s\"", expr->literal_string.value);
            return true;
        }

        case AST_LITERAL_BOOL:
            arc_codegen_emit(codegen, expr->literal_bool.value ? "true" : "false");
            return true;
        case AST_IDENTIFIER: {
            char *name_temp = MALLOC(expr->identifier.token.length + 1);
            if (!name_temp) {
                arc_codegen_set_error(codegen, "Failed to allocate memory for identifier");
                return false;
            }
            strncpy(name_temp, expr->identifier.token.start, expr->identifier.token.length);
            name_temp[expr->identifier.token.length] = '\0';

            char *mangled_name = arc_codegen_mangle_name(name_temp);
            FREE(name_temp);
            if (!mangled_name) {
                arc_codegen_set_error(codegen, "Failed to mangle identifier");
                return false;
            }
            arc_codegen_emit(codegen, "%s", mangled_name);
            FREE(mangled_name);
            return true;
        }

        case AST_EXPR_BINARY: {
            const char *op_str = "";
            switch (expr->binary_expr.op_type) {
                case BINARY_OP_ADD:
                    op_str = "+";
                    break;
                case BINARY_OP_SUB:
                    op_str = "-";
                    break;
                case BINARY_OP_MUL:
                    op_str = "*";
                    break;
                case BINARY_OP_DIV:
                    op_str = "/";
                    break;
                case BINARY_OP_MOD:
                    op_str = "%";
                    break;
                case BINARY_OP_EQ:
                    op_str = "==";
                    break;
                case BINARY_OP_NE:
                    op_str = "!=";
                    break;
                case BINARY_OP_LT:
                    op_str = "<";
                    break;
                case BINARY_OP_LE:
                    op_str = "<=";
                    break;
                case BINARY_OP_GT:
                    op_str = ">";
                    break;
                case BINARY_OP_GE:
                    op_str = ">=";
                    break;
                case BINARY_OP_AND:
                    op_str = "&&";
                    break;
                case BINARY_OP_OR:
                    op_str = "||";
                    break;
                case BINARY_OP_ASSIGN:
                    op_str = "=";
                    break;
                case BINARY_OP_ADD_ASSIGN:
                    op_str = "+=";
                    break;
                case BINARY_OP_SUB_ASSIGN:
                    op_str = "-=";
                    break;
                case BINARY_OP_MUL_ASSIGN:
                    op_str = "*=";
                    break;
                case BINARY_OP_DIV_ASSIGN:
                    op_str = "/=";
                    break;
                default:
                    arc_codegen_set_error(codegen, "Unsupported binary operator: %d",
                                          expr->binary_expr.op_type);
                    return false;
            }

            arc_codegen_emit(codegen, "(");
            if (!arc_codegen_generate_expression(codegen, expr->binary_expr.left)) {
                return false;
            }
            arc_codegen_emit(codegen, " %s ", op_str);
            if (!arc_codegen_generate_expression(codegen, expr->binary_expr.right)) {
                return false;
            }
            arc_codegen_emit(codegen, ")");
            return true;
        }

        case AST_EXPR_UNARY: {
            const char *op_str = "";
            switch (expr->unary_expr.op_type) {
                case UNARY_OP_NOT:
                    op_str = "!";
                    break;
                case UNARY_OP_NEGATE:
                    op_str = "-";
                    break;
                default:
                    arc_codegen_set_error(codegen, "Unsupported unary operator: %d",
                                          expr->unary_expr.op_type);
                    return false;
            }

            arc_codegen_emit(codegen, "%s", op_str);
            if (!arc_codegen_generate_expression(codegen, expr->unary_expr.operand)) {
                return false;
            }
            return true;
        }

        case AST_EXPR_CALL: {
            if (!arc_codegen_generate_expression(codegen, expr->call_expr.function)) {
                return false;
            }
            arc_codegen_emit(codegen, "(");

            for (size_t i = 0; i < expr->call_expr.argument_count; i++) {
                if (i > 0)
                    arc_codegen_emit(codegen, ", ");
                if (!arc_codegen_generate_expression(codegen, expr->call_expr.arguments[i])) {
                    return false;
                }
            }

            arc_codegen_emit(codegen, ")");
            return true;
        }

        default:
            arc_codegen_set_error(codegen, "Unsupported expression type: %d", expr->type);
            return false;
    }
}
