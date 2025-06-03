// compiler/src/semantic.c
#include "arc/semantic.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Forward declarations
static void arc_semantic_analyzer_setup_builtins(ArcSemanticAnalyzer *analyzer);
static bool arc_analyze_assignment_target(ArcSemanticAnalyzer *analyzer, ArcAstNode *expr);
static bool arc_check_variable_initialization(ArcSemanticAnalyzer *analyzer, const char *var_name,
                                              ArcSourceInfo source_info);
static bool arc_analyze_return_type_compatibility(ArcSemanticAnalyzer *analyzer,
                                                  ArcTypeInfo *expr_type,
                                                  ArcSourceInfo source_info);
static void arc_check_unreachable_code(ArcSemanticAnalyzer *analyzer, ArcAstNode *stmt);
static bool arc_validate_function_signature(ArcSemanticAnalyzer *analyzer, ArcAstNode *func_decl);
static void arc_check_unused_variables(ArcSemanticAnalyzer *analyzer);
static void arc_check_unused_in_scope(ArcSemanticAnalyzer *analyzer, ArcScope *scope);
static bool arc_type_is_assignable(ArcTypeInfo *from, ArcTypeInfo *to);
static ArcTypeInfo *arc_infer_expression_type(ArcSemanticAnalyzer *analyzer, ArcAstNode *expr);

// Enhanced type system helpers
static const char *arc_type_to_string(ArcTypeInfo *type) {
    if (!type)
        return "unknown";

    switch (type->kind) {
        case ARC_TYPE_PRIMITIVE:
            switch (type->primitive.primitive_type) {
                case TOKEN_KEYWORD_I8:
                    return "i8";
                case TOKEN_KEYWORD_I16:
                    return "i16";
                case TOKEN_KEYWORD_I32:
                    return "i32";
                case TOKEN_KEYWORD_I64:
                    return "i64";
                case TOKEN_KEYWORD_U8:
                    return "u8";
                case TOKEN_KEYWORD_U16:
                    return "u16";
                case TOKEN_KEYWORD_U32:
                    return "u32";
                case TOKEN_KEYWORD_U64:
                    return "u64";
                case TOKEN_KEYWORD_F32:
                    return "f32";
                case TOKEN_KEYWORD_F64:
                    return "f64";
                case TOKEN_KEYWORD_BOOL:
                    return "bool";
                case TOKEN_KEYWORD_CHAR:
                    return "char";
                default:
                    return "primitive";
            }
        case ARC_TYPE_POINTER:
            return "pointer";
        case ARC_TYPE_ARRAY:
            return "array";
        case ARC_TYPE_SLICE:
            return "slice";
        case ARC_TYPE_OPTIONAL:
            return "optional";
        case ARC_TYPE_FUNCTION:
            return "function";
        case ARC_TYPE_VOID:
            return "void";
        case ARC_TYPE_ERROR:
            return "error";
        default:
            return "unknown";
    }
}

// === SEMANTIC ANALYZER LIFECYCLE ===

ArcSemanticAnalyzer *arc_semantic_analyzer_create(void) {
    ArcSemanticAnalyzer *analyzer = malloc(sizeof(ArcSemanticAnalyzer));
    if (!analyzer)
        return NULL;

    // Initialize arena for memory management
    analyzer->arena = arc_arena_create(1024 * 1024);  // 1MB arena
    if (!analyzer->arena) {
        free(analyzer);
        return NULL;
    }

    // Create global scope
    analyzer->global_scope = arc_scope_create(analyzer, ARC_SCOPE_GLOBAL, NULL);
    analyzer->current_scope = analyzer->global_scope;

    // Initialize builtin types
    analyzer->builtin_types = NULL;
    analyzer->builtin_type_count = 0;

    // Initialize diagnostics
    analyzer->diagnostics = NULL;
    analyzer->last_diagnostic = NULL;
    analyzer->error_count = 0;
    analyzer->warning_count = 0;

    // Initialize context
    analyzer->current_function = NULL;
    analyzer->in_loop = false;
    analyzer->has_return = false;

    // Setup builtin types
    arc_semantic_analyzer_setup_builtins(analyzer);

    return analyzer;
}

void arc_semantic_analyzer_destroy(ArcSemanticAnalyzer *analyzer) {
    if (!analyzer)
        return;

    // Diagnostics are arena-allocated, so they'll be freed with the arena
    arc_arena_destroy(analyzer->arena);
    free(analyzer);
}

// === SCOPE MANAGEMENT ===

ArcScope *arc_scope_create(ArcSemanticAnalyzer *analyzer, ArcScopeKind kind, ArcScope *parent) {
    ArcScope *scope = arc_arena_alloc(analyzer->arena, sizeof(ArcScope));
    if (!scope)
        return NULL;

    scope->kind = kind;
    scope->parent = parent;
    scope->symbol_capacity = 16;  // Start with 16 symbol slots
    scope->symbol_count = 0;
    scope->symbols = arc_arena_alloc(analyzer->arena, scope->symbol_capacity * sizeof(ArcSymbol *));

    // Initialize symbol table to NULL
    for (size_t i = 0; i < scope->symbol_capacity; i++) {
        scope->symbols[i] = NULL;
    }

    scope->function_symbol = NULL;
    scope->return_type = NULL;

    return scope;
}

void arc_scope_push(ArcSemanticAnalyzer *analyzer, ArcScope *scope) {
    analyzer->current_scope = scope;
}

ArcScope *arc_scope_pop(ArcSemanticAnalyzer *analyzer) {
    ArcScope *current = analyzer->current_scope;
    if (current && current->parent) {
        analyzer->current_scope = current->parent;
    }
    return current;
}

// === SYMBOL MANAGEMENT ===

// Simple hash function for symbol names
static size_t arc_hash_string(const char *str, size_t capacity) {
    size_t hash = 5381;
    while (*str) {
        hash = ((hash << 5) + hash) + *str++;
    }
    return hash % capacity;
}

ArcSymbol *arc_symbol_create(ArcSemanticAnalyzer *analyzer, ArcSymbolKind kind, const char *name) {
    ArcSymbol *symbol = arc_arena_alloc(analyzer->arena, sizeof(ArcSymbol));
    if (!symbol)
        return NULL;

    symbol->kind = kind;
    symbol->name = arc_arena_alloc(analyzer->arena, strlen(name) + 1);
    if (symbol->name) {
        strcpy(symbol->name, name);
    }

    symbol->type = NULL;
    symbol->declaration_node = NULL;
    symbol->scope = NULL;
    symbol->is_mutable = false;
    symbol->is_public = false;
    symbol->is_defined = false;
    symbol->parameters = NULL;
    symbol->parameter_count = 0;
    symbol->next = NULL;

    return symbol;
}

bool arc_scope_add_symbol(ArcScope *scope, ArcSymbol *symbol) {
    if (!scope || !symbol || !symbol->name)
        return false;

    size_t hash = arc_hash_string(symbol->name, scope->symbol_capacity);

    // Check for duplicate in same scope
    ArcSymbol *existing = scope->symbols[hash];
    while (existing) {
        if (strcmp(existing->name, symbol->name) == 0) {
            return false;  // Symbol already exists
        }
        existing = existing->next;
    }

    // Add symbol to hash table
    symbol->next = scope->symbols[hash];
    scope->symbols[hash] = symbol;
    symbol->scope = scope;
    scope->symbol_count++;

    return true;
}

ArcSymbol *arc_scope_lookup_symbol(ArcScope *scope, const char *name) {
    if (!scope || !name)
        return NULL;

    size_t hash = arc_hash_string(name, scope->symbol_capacity);
    ArcSymbol *symbol = scope->symbols[hash];

    while (symbol) {
        if (strcmp(symbol->name, name) == 0) {
            return symbol;
        }
        symbol = symbol->next;
    }

    return NULL;
}

ArcSymbol *arc_scope_lookup_symbol_recursive(ArcScope *scope, const char *name) {
    while (scope) {
        ArcSymbol *symbol = arc_scope_lookup_symbol(scope, name);
        if (symbol) {
            return symbol;
        }
        scope = scope->parent;
    }
    return NULL;
}

// === TYPE SYSTEM ===

ArcTypeInfo *arc_type_create(ArcSemanticAnalyzer *analyzer, ArcTypeKind kind) {
    ArcTypeInfo *type = arc_arena_alloc(analyzer->arena, sizeof(ArcTypeInfo));
    if (!type)
        return NULL;

    type->kind = kind;
    type->is_resolved = false;
    type->size = 0;
    type->alignment = 0;

    return type;
}

// Setup builtin types
static void arc_semantic_analyzer_setup_builtins(ArcSemanticAnalyzer *analyzer) {
    // We'll add builtin types like i32, bool, etc.
    // For now, we'll create them on-demand in arc_type_get_builtin
}

ArcTypeInfo *arc_type_get_builtin(ArcSemanticAnalyzer *analyzer, ArcTokenType primitive_type) {
    ArcTypeInfo *type = arc_type_create(analyzer, ARC_TYPE_PRIMITIVE);
    if (!type)
        return NULL;

    type->primitive.primitive_type = primitive_type;
    type->is_resolved = true;

    // Set size and alignment based on type
    switch (primitive_type) {
        case TOKEN_KEYWORD_I8:
        case TOKEN_KEYWORD_U8:
        case TOKEN_KEYWORD_BOOL:
        case TOKEN_KEYWORD_CHAR:
            type->size = 1;
            type->alignment = 1;
            break;
        case TOKEN_KEYWORD_I16:
        case TOKEN_KEYWORD_U16:
            type->size = 2;
            type->alignment = 2;
            break;
        case TOKEN_KEYWORD_I32:
        case TOKEN_KEYWORD_U32:
        case TOKEN_KEYWORD_F32:
            type->size = 4;
            type->alignment = 4;
            break;
        case TOKEN_KEYWORD_I64:
        case TOKEN_KEYWORD_U64:
        case TOKEN_KEYWORD_F64:
        case TOKEN_KEYWORD_ISIZE:
        case TOKEN_KEYWORD_USIZE:
            type->size = 8;
            type->alignment = 8;
            break;
        case TOKEN_KEYWORD_VOID:
            type->size = 0;
            type->alignment = 1;
            break;
        default:
            type->size = 0;
            type->alignment = 1;
            break;
    }

    return type;
}

// === DIAGNOSTICS ===

void arc_diagnostic_add(ArcSemanticAnalyzer *analyzer, ArcDiagnosticLevel level,
                        ArcSourceInfo source_info, const char *format, ...) {
    ArcDiagnostic *diag = arc_arena_alloc(analyzer->arena, sizeof(ArcDiagnostic));
    if (!diag)
        return;

    diag->level = level;
    diag->source_info = source_info;
    diag->next = NULL;

    // Format message
    va_list args;
    va_start(args, format);

    // Calculate required size
    va_list args_copy;
    va_copy(args_copy, args);
    int size = vsnprintf(NULL, 0, format, args_copy);
    va_end(args_copy);

    if (size > 0) {
        diag->message = arc_arena_alloc(analyzer->arena, size + 1);
        if (diag->message) {
            vsnprintf(diag->message, size + 1, format, args);
        }
    } else {
        diag->message = NULL;
    }

    va_end(args);

    // Add to diagnostic list
    if (!analyzer->diagnostics) {
        analyzer->diagnostics = diag;
        analyzer->last_diagnostic = diag;
    } else {
        analyzer->last_diagnostic->next = diag;
        analyzer->last_diagnostic = diag;
    }  // Update counters
    if (level == ARC_DIAGNOSTIC_ERROR) {
        analyzer->error_count++;
    } else if (level == ARC_DIAGNOSTIC_WARNING) {
        analyzer->warning_count++;
    }
}

void arc_diagnostic_print_all(ArcSemanticAnalyzer *analyzer) {
    ArcDiagnostic *diag = analyzer->diagnostics;

    while (diag) {
        const char *level_str;
        switch (diag->level) {
            case ARC_DIAGNOSTIC_ERROR:
                level_str = "error";
                break;
            case ARC_DIAGNOSTIC_WARNING:
                level_str = "warning";
                break;
            case ARC_DIAGNOSTIC_NOTE:
                level_str = "note";
                break;
            default:
                level_str = "unknown";
                break;
        }

        printf("%s:%zu:%zu: %s: %s\n",
               diag->source_info.filename ? diag->source_info.filename : "<unknown>",
               diag->source_info.location.line, diag->source_info.location.column, level_str,
               diag->message ? diag->message : "No message");

        diag = diag->next;
    }
}

bool arc_semantic_has_errors(ArcSemanticAnalyzer *analyzer) {
    return analyzer->error_count > 0;
}

// === TYPE ANALYSIS ===

ArcTypeInfo *arc_type_from_ast(ArcSemanticAnalyzer *analyzer, ArcAstNode *type_node) {
    if (!type_node)
        return NULL;

    switch (type_node->type) {
        case AST_TYPE_PRIMITIVE: {
            return arc_type_get_builtin(analyzer, type_node->type_primitive.primitive_type);
        }

        case AST_TYPE_POINTER: {
            ArcTypeInfo *pointed_type =
                arc_type_from_ast(analyzer, type_node->type_pointer.pointed_type);
            if (!pointed_type)
                return NULL;

            ArcTypeInfo *ptr_type = arc_type_create(analyzer, ARC_TYPE_POINTER);
            if (!ptr_type)
                return NULL;

            ptr_type->pointer.pointed_type = pointed_type;
            ptr_type->pointer.is_mutable =
                (type_node->type_pointer.caret_token.type == TOKEN_CARET);
            ptr_type->size = 8;  // Pointer size
            ptr_type->alignment = 8;
            ptr_type->is_resolved = true;

            return ptr_type;
        }

        case AST_TYPE_ARRAY: {
            ArcTypeInfo *element_type =
                arc_type_from_ast(analyzer, type_node->type_array.element_type);
            if (!element_type)
                return NULL;

            ArcTypeInfo *array_type = arc_type_create(analyzer, ARC_TYPE_ARRAY);
            if (!array_type)
                return NULL;

            array_type->array.element_type = element_type;
            // TODO: Evaluate size expression
            array_type->array.size = 10;  // Placeholder
            array_type->size = element_type->size * array_type->array.size;
            array_type->alignment = element_type->alignment;
            array_type->is_resolved = true;

            return array_type;
        }

        case AST_TYPE_SLICE: {
            ArcTypeInfo *element_type =
                arc_type_from_ast(analyzer, type_node->type_slice.element_type);
            if (!element_type)
                return NULL;

            ArcTypeInfo *slice_type = arc_type_create(analyzer, ARC_TYPE_SLICE);
            if (!slice_type)
                return NULL;

            slice_type->slice.element_type = element_type;
            slice_type->size = 16;  // ptr + length
            slice_type->alignment = 8;
            slice_type->is_resolved = true;

            return slice_type;
        }

        case AST_TYPE_OPTIONAL: {
            ArcTypeInfo *inner_type =
                arc_type_from_ast(analyzer, type_node->type_optional.inner_type);
            if (!inner_type)
                return NULL;

            ArcTypeInfo *opt_type = arc_type_create(analyzer, ARC_TYPE_OPTIONAL);
            if (!opt_type)
                return NULL;

            opt_type->optional.inner_type = inner_type;
            opt_type->size = inner_type->size + 1;  // value + has_value flag
            opt_type->alignment = inner_type->alignment;
            opt_type->is_resolved = true;

            return opt_type;
        }

        case AST_TYPE_FUNCTION: {
            ArcTypeInfo *func_type = arc_type_create(analyzer, ARC_TYPE_FUNCTION);
            if (!func_type)
                return NULL;

            // Process parameter types
            size_t param_count = type_node->type_function.parameter_count;
            if (param_count > 0) {
                func_type->function.parameter_types =
                    arc_arena_alloc(analyzer->arena, param_count * sizeof(ArcTypeInfo *));
                if (!func_type->function.parameter_types)
                    return NULL;

                for (size_t i = 0; i < param_count; i++) {
                    func_type->function.parameter_types[i] =
                        arc_type_from_ast(analyzer, type_node->type_function.parameter_types[i]);
                    if (!func_type->function.parameter_types[i])
                        return NULL;
                }
            }
            func_type->function.parameter_count = param_count;

            // Process return type
            if (type_node->type_function.return_type) {
                func_type->function.return_type =
                    arc_type_from_ast(analyzer, type_node->type_function.return_type);
            } else {
                func_type->function.return_type =
                    arc_type_get_builtin(analyzer, TOKEN_KEYWORD_VOID);
            }

            func_type->size = 8;  // Function pointer size
            func_type->alignment = 8;
            func_type->is_resolved = true;

            return func_type;
        }

        default:
            arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, type_node->source_info,
                               "Unknown type kind in semantic analysis");
            return NULL;
    }
}

bool arc_type_is_compatible(ArcTypeInfo *left, ArcTypeInfo *right) {
    if (!left || !right)
        return false;
    if (left == right)
        return true;

    if (left->kind != right->kind)
        return false;

    switch (left->kind) {
        case ARC_TYPE_PRIMITIVE:
            return left->primitive.primitive_type == right->primitive.primitive_type;

        case ARC_TYPE_POINTER:
            return arc_type_is_compatible(left->pointer.pointed_type,
                                          right->pointer.pointed_type) &&
                   left->pointer.is_mutable == right->pointer.is_mutable;

        case ARC_TYPE_ARRAY:
            return arc_type_is_compatible(left->array.element_type, right->array.element_type) &&
                   left->array.size == right->array.size;

        case ARC_TYPE_SLICE:
            return arc_type_is_compatible(left->slice.element_type, right->slice.element_type);

        case ARC_TYPE_OPTIONAL:
            return arc_type_is_compatible(left->optional.inner_type, right->optional.inner_type);

        case ARC_TYPE_FUNCTION: {
            if (left->function.parameter_count != right->function.parameter_count)
                return false;

            for (size_t i = 0; i < left->function.parameter_count; i++) {
                if (!arc_type_is_compatible(left->function.parameter_types[i],
                                            right->function.parameter_types[i])) {
                    return false;
                }
            }

            return arc_type_is_compatible(left->function.return_type, right->function.return_type);
        }

        default:
            return false;
    }
}

bool arc_type_is_assignable_to(ArcTypeInfo *from, ArcTypeInfo *to) {
    // For now, just use compatibility check
    // Later we can add implicit conversions (e.g., i32 -> i64)
    return arc_type_is_compatible(from, to);
}

// === TYPE ANALYSIS HELPERS ===

static bool arc_type_is_numeric(ArcTypeInfo *type) {
    if (!type || type->kind != ARC_TYPE_PRIMITIVE) {
        return false;
    }
    switch (type->primitive.primitive_type) {
        case TOKEN_KEYWORD_I8:
        case TOKEN_KEYWORD_I16:
        case TOKEN_KEYWORD_I32:
        case TOKEN_KEYWORD_I64:
        case TOKEN_KEYWORD_U8:
        case TOKEN_KEYWORD_U16:
        case TOKEN_KEYWORD_U32:
        case TOKEN_KEYWORD_U64:
        case TOKEN_KEYWORD_F32:
        case TOKEN_KEYWORD_F64:
            return true;
        default:
            return false;
    }
}

static bool arc_type_is_integer(ArcTypeInfo *type) {
    if (!type || type->kind != ARC_TYPE_PRIMITIVE) {
        return false;
    }
    switch (type->primitive.primitive_type) {
        case TOKEN_KEYWORD_I8:
        case TOKEN_KEYWORD_I16:
        case TOKEN_KEYWORD_I32:
        case TOKEN_KEYWORD_I64:
        case TOKEN_KEYWORD_U8:
        case TOKEN_KEYWORD_U16:
        case TOKEN_KEYWORD_U32:
        case TOKEN_KEYWORD_U64:
            return true;
        default:
            return false;
    }
}

static bool arc_type_is_boolean(ArcTypeInfo *type) {
    return type && type->kind == ARC_TYPE_PRIMITIVE &&
           type->primitive.primitive_type == TOKEN_KEYWORD_BOOL;
}

// Choose the "wider" type for arithmetic operations
static ArcTypeInfo *arc_type_get_common_arithmetic_type(ArcSemanticAnalyzer *analyzer,
                                                        ArcTypeInfo *left, ArcTypeInfo *right) {
    // For now, implement simple type promotion rules
    // Float types take precedence over integer types
    if (left->kind == ARC_TYPE_PRIMITIVE && right->kind == ARC_TYPE_PRIMITIVE) {
        if (left->primitive.primitive_type == TOKEN_KEYWORD_F64 ||
            right->primitive.primitive_type == TOKEN_KEYWORD_F64) {
            return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_F64);
        }
        if (left->primitive.primitive_type == TOKEN_KEYWORD_F32 ||
            right->primitive.primitive_type == TOKEN_KEYWORD_F32) {
            return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_F32);
        }
        // For integers, return the wider type (simplified)
        // TODO: Implement proper integer promotion rules
        return left;  // For now, just return left type
    }
    return left;
}

static ArcTypeInfo *arc_analyze_arithmetic_binary_op(ArcSemanticAnalyzer *analyzer,
                                                     ArcAstNode *expr, ArcTypeInfo *left_type,
                                                     ArcTypeInfo *right_type) {
    if (!arc_type_is_numeric(left_type)) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                           "Left operand of arithmetic operation must be numeric");
        return NULL;
    }
    if (!arc_type_is_numeric(right_type)) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                           "Right operand of arithmetic operation must be numeric");
        return NULL;
    }

    // Special case for modulo: requires integer operands
    if (expr->binary_expr.op_type == BINARY_OP_MOD) {
        if (!arc_type_is_integer(left_type) || !arc_type_is_integer(right_type)) {
            arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                               "Modulo operation requires integer operands");
            return NULL;
        }
    }

    return arc_type_get_common_arithmetic_type(analyzer, left_type, right_type);
}

static ArcTypeInfo *arc_analyze_comparison_binary_op(ArcSemanticAnalyzer *analyzer,
                                                     ArcAstNode *expr, ArcTypeInfo *left_type,
                                                     ArcTypeInfo *right_type) {
    // For now, allow comparison between same types or numeric types
    if (arc_type_is_numeric(left_type) && arc_type_is_numeric(right_type)) {
        return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_BOOL);
    }

    // TODO: Implement proper type compatibility checking
    // For now, assume compatibility and return bool
    return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_BOOL);
}

static ArcTypeInfo *arc_analyze_logical_binary_op(ArcSemanticAnalyzer *analyzer, ArcAstNode *expr,
                                                  ArcTypeInfo *left_type, ArcTypeInfo *right_type) {
    if (!arc_type_is_boolean(left_type)) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                           "Left operand of logical operation must be boolean");
        return NULL;
    }
    if (!arc_type_is_boolean(right_type)) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                           "Right operand of logical operation must be boolean");
        return NULL;
    }
    return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_BOOL);
}

static ArcTypeInfo *arc_analyze_bitwise_binary_op(ArcSemanticAnalyzer *analyzer, ArcAstNode *expr,
                                                  ArcTypeInfo *left_type, ArcTypeInfo *right_type) {
    if (!arc_type_is_integer(left_type)) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                           "Left operand of bitwise operation must be integer");
        return NULL;
    }
    if (!arc_type_is_integer(right_type)) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                           "Right operand of bitwise operation must be integer");
        return NULL;
    }
    return arc_type_get_common_arithmetic_type(analyzer, left_type, right_type);
}

static ArcTypeInfo *arc_analyze_assignment_binary_op(ArcSemanticAnalyzer *analyzer,
                                                     ArcAstNode *expr, ArcTypeInfo *left_type,
                                                     ArcTypeInfo *right_type) {
    // Enhanced assignment analysis with lvalue checking
    if (!arc_analyze_assignment_target(analyzer, expr->binary_expr.left)) {
        return NULL;
    }  // Enhanced type compatibility checking for assignment
    if (!arc_type_is_assignable_to(right_type, left_type)) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                           "Type mismatch in assignment: cannot assign %s to %s",
                           arc_type_to_string(right_type), arc_type_to_string(left_type));
        return NULL;
    }

    // For compound assignments, check the operation compatibility
    if (expr->binary_expr.op_type != BINARY_OP_ASSIGN) {
        switch (expr->binary_expr.op_type) {
            case BINARY_OP_ADD_ASSIGN:
            case BINARY_OP_SUB_ASSIGN:
            case BINARY_OP_MUL_ASSIGN:
            case BINARY_OP_DIV_ASSIGN:
                if (!arc_type_is_numeric(left_type) || !arc_type_is_numeric(right_type)) {
                    arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                                       "Arithmetic assignment requires numeric operands");
                    return NULL;
                }
                break;
            case BINARY_OP_MOD_ASSIGN:
                if (!arc_type_is_integer(left_type) || !arc_type_is_integer(right_type)) {
                    arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                                       "Modulo assignment requires integer operands");
                    return NULL;
                }
                break;
            default:
                break;
        }
    }

    // Mark the assigned variable as initialized if it's an identifier
    if (expr->binary_expr.left->type == AST_IDENTIFIER) {
        size_t name_len = expr->binary_expr.left->identifier.token.length;
        char *name = arc_arena_alloc(analyzer->arena, name_len + 1);
        if (name) {
            strncpy(name, expr->binary_expr.left->identifier.token.start, name_len);
            name[name_len] = '\0';

            ArcSymbol *symbol = arc_scope_lookup_symbol_recursive(analyzer->current_scope, name);
            if (symbol && symbol->kind == ARC_SYMBOL_VARIABLE) {
                symbol->is_defined = true;  // Mark as initialized
            }
        }
    }

    return left_type;  // Assignment expression has the type of the left operand
}

static ArcTypeInfo *arc_analyze_unary_op(ArcSemanticAnalyzer *analyzer, ArcAstNode *expr,
                                         ArcTypeInfo *operand_type) {
    // TODO: Implement unary operator analysis
    arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_WARNING, expr->source_info,
                       "Unary operator analysis not yet implemented");
    return operand_type;
}

static ArcTypeInfo *arc_analyze_function_call(ArcSemanticAnalyzer *analyzer, ArcAstNode *expr) {
    if (!expr || expr->type != AST_EXPR_CALL) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                           "Invalid function call expression");
        return NULL;
    }

    // Get the function identifier
    ArcAstNode *callee = expr->call_expr.function;
    if (!callee || callee->type != AST_IDENTIFIER) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                           "Function calls only support direct identifiers for now");
        return NULL;
    }

    // Extract function name from token
    ArcToken name_token = callee->identifier.token;
    if (name_token.length == 0 || !name_token.start) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                           "Invalid function name");
        return NULL;
    }

    char *func_name = arc_arena_alloc(analyzer->arena, name_token.length + 1);
    if (!func_name) {
        return NULL;
    }
    strncpy(func_name, name_token.start, name_token.length);
    func_name[name_token.length] = '\0';

    // Look up the function symbol
    ArcSymbol *func_symbol = arc_scope_lookup_symbol_recursive(analyzer->current_scope, func_name);
    if (!func_symbol) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                           "Undeclared function '%s'", func_name);
        return NULL;
    }

    if (func_symbol->kind != ARC_SYMBOL_FUNCTION) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                           "'%s' is not a function", func_name);
        return NULL;
    }

    // Check argument count
    size_t provided_args = expr->call_expr.argument_count;
    size_t expected_args = func_symbol->parameter_count;

    if (provided_args != expected_args) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                           "Function '%s' expects %zu arguments, but %zu were provided", func_name,
                           expected_args, provided_args);
        return NULL;
    }

    // Check argument types
    for (size_t i = 0; i < provided_args; i++) {
        ArcTypeInfo *arg_type = arc_analyze_expression_type(analyzer, expr->call_expr.arguments[i]);
        if (!arg_type) {
            arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                               "Invalid argument %zu in function call", i + 1);
            return NULL;
        }

        // For now, we don't have detailed parameter type info stored in function symbols
        // This would need to be enhanced when we fully implement function types
        // TODO: Check actual parameter types when function type system is complete
    }

    // Return the function's return type
    // For now, we don't have proper return type tracking in symbols
    // TODO: Implement proper return type tracking
    if (func_symbol->type && func_symbol->type->kind == ARC_TYPE_FUNCTION) {
        return func_symbol->type->function.return_type;
    }

    // Default to void for now
    return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_VOID);
}

// === MAIN ANALYSIS ENTRY POINT ===

bool arc_semantic_analyze(ArcSemanticAnalyzer *analyzer, ArcAstNode *ast) {
    if (!analyzer || !ast)
        return false;

    if (ast->type != AST_PROGRAM) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, ast->source_info,
                           "Expected program node at top level");
        return false;
    }  // Analyze all declarations in the program
    for (size_t i = 0; i < ast->program.declaration_count; i++) {
        if (!arc_analyze_declaration(analyzer, ast->program.declarations[i])) {
            // Continue analyzing other declarations even if one fails
        }
    }

    // After analyzing all declarations, check for unused variables
    arc_check_unused_variables(analyzer);

    return !arc_semantic_has_errors(analyzer);
}

bool arc_analyze_declaration(ArcSemanticAnalyzer *analyzer, ArcAstNode *decl) {
    if (!decl)
        return false;

    switch (decl->type) {
        case AST_DECL_FUNCTION: {
            // Get function name from token
            if (decl->function_decl.name.length == 0 || !decl->function_decl.name.start) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, decl->source_info,
                                   "Function declaration missing name");
                return false;
            }

            // Extract name from token (we need to make a null-terminated copy)
            size_t name_len = decl->function_decl.name.length;
            char *name = arc_arena_alloc(analyzer->arena, name_len + 1);
            if (!name) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, decl->source_info,
                                   "Failed to allocate memory for function name");
                return false;
            }
            strncpy(name, decl->function_decl.name.start, name_len);
            name[name_len] = '\0';

            // Check for duplicate declaration in current scope
            if (arc_scope_lookup_symbol(analyzer->current_scope, name)) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, decl->source_info,
                                   "Function '%s' already declared in this scope", name);
                return false;
            }

            // Create function symbol
            ArcSymbol *symbol = arc_symbol_create(analyzer, ARC_SYMBOL_FUNCTION, name);
            if (!symbol) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, decl->source_info,
                                   "Failed to create symbol for function '%s'", name);
                return false;
            }

            // Set function properties
            symbol->declaration_node = decl;
            symbol->is_public = true;  // For now, assume all functions are public
            symbol->is_defined = (decl->function_decl.body != NULL);

            // TODO: Create function type from parameters and return type
            // For now, just set a placeholder type
            symbol->type =
                arc_type_create(analyzer, ARC_TYPE_FUNCTION);  // Validate function signature
            if (!arc_validate_function_signature(analyzer, decl)) {
                return false;
            }

            // Add to current scope
            if (!arc_scope_add_symbol(analyzer->current_scope, symbol)) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, decl->source_info,
                                   "Failed to add function '%s' to scope", name);
                return false;
            }

            // If function has a body, analyze it
            if (decl->function_decl.body) {
                // Create new scope for function
                ArcScope *func_scope =
                    arc_scope_create(analyzer, ARC_SCOPE_FUNCTION, analyzer->current_scope);
                func_scope->function_symbol = symbol;

                // Add parameters to function scope
                for (size_t i = 0; i < decl->function_decl.parameter_count; i++) {
                    ArcAstNode *param = decl->function_decl.parameters[i];
                    if (param->type == AST_PARAMETER) {
                        // Extract parameter name from token
                        size_t param_name_len = param->parameter.name.length;
                        if (param_name_len > 0 && param->parameter.name.start) {
                            char *param_name = arc_arena_alloc(analyzer->arena, param_name_len + 1);
                            if (param_name) {
                                strncpy(param_name, param->parameter.name.start, param_name_len);
                                param_name[param_name_len] = '\0';

                                ArcSymbol *param_symbol =
                                    arc_symbol_create(analyzer, ARC_SYMBOL_PARAMETER, param_name);
                                if (param_symbol) {
                                    param_symbol->type = arc_type_from_ast(
                                        analyzer, param->parameter.type_annotation);
                                    param_symbol->declaration_node = param;
                                    arc_scope_add_symbol(func_scope, param_symbol);
                                }
                            }
                        }
                    }
                }

                // Push function scope and analyze body
                arc_scope_push(analyzer, func_scope);
                analyzer->current_function = symbol;

                bool body_ok = arc_analyze_statement(analyzer, decl->function_decl.body);

                // Pop function scope
                arc_scope_pop(analyzer);
                analyzer->current_function = NULL;

                if (!body_ok) {
                    return false;
                }
            }

            return true;
        }

        default:
            arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_WARNING, decl->source_info,
                               "Declaration type analysis not yet implemented");
            return true;
    }
}

ArcTypeInfo *arc_analyze_expression_type(ArcSemanticAnalyzer *analyzer, ArcAstNode *expr) {
    if (!expr)
        return NULL;

    switch (expr->type) {
        case AST_LITERAL_INT:
            return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_I32);

        case AST_LITERAL_FLOAT:
            return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_F64);

        case AST_LITERAL_BOOL:
            return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_BOOL);

        case AST_LITERAL_CHAR:
            return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_CHAR);

        case AST_LITERAL_NULL:
            // NULL literals are compatible with any pointer type
            // For now, return a generic pointer type
            // TODO: Implement proper null type that can be assigned to any pointer
            return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_VOID);
        case AST_IDENTIFIER: {
            // Extract identifier name from token
            size_t name_len = expr->identifier.token.length;
            if (name_len == 0 || !expr->identifier.token.start) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                                   "Invalid identifier");
                return NULL;
            }

            char *name = arc_arena_alloc(analyzer->arena, name_len + 1);
            if (!name)
                return NULL;
            strncpy(name, expr->identifier.token.start, name_len);
            name[name_len] = '\0';

            // Enhanced identifier resolution with initialization checking
            if (!arc_check_variable_initialization(analyzer, name, expr->source_info)) {
                return NULL;
            }

            ArcSymbol *symbol = arc_scope_lookup_symbol_recursive(analyzer->current_scope, name);
            // symbol should exist due to arc_check_variable_initialization, but double-check
            if (!symbol) {
                return NULL;
            }

            return symbol->type;
        }
        case AST_LITERAL_STRING:
            // TODO: Implement proper string type
            return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_CHAR);  // For now, treat as char

        case AST_EXPR_BINARY: {
            // Analyze left and right operands
            ArcTypeInfo *left_type = arc_analyze_expression_type(analyzer, expr->binary_expr.left);
            ArcTypeInfo *right_type =
                arc_analyze_expression_type(analyzer, expr->binary_expr.right);

            if (!left_type || !right_type) {
                return NULL;
            }

            // Type checking for binary operations
            switch (expr->binary_expr.op_type) {
                // Arithmetic operations
                case BINARY_OP_ADD:
                case BINARY_OP_SUB:
                case BINARY_OP_MUL:
                case BINARY_OP_DIV:
                case BINARY_OP_MOD:
                    return arc_analyze_arithmetic_binary_op(analyzer, expr, left_type, right_type);

                // Comparison operations
                case BINARY_OP_EQ:
                case BINARY_OP_NE:
                case BINARY_OP_LT:
                case BINARY_OP_LE:
                case BINARY_OP_GT:
                case BINARY_OP_GE:
                    return arc_analyze_comparison_binary_op(analyzer, expr, left_type, right_type);

                // Logical operations
                case BINARY_OP_AND:
                case BINARY_OP_OR:
                    return arc_analyze_logical_binary_op(analyzer, expr, left_type, right_type);

                // Bitwise operations
                case BINARY_OP_BIT_AND:
                case BINARY_OP_BIT_OR:
                case BINARY_OP_BIT_XOR:
                case BINARY_OP_BIT_SHL:
                case BINARY_OP_BIT_SHR:
                    return arc_analyze_bitwise_binary_op(analyzer, expr, left_type, right_type);

                // Assignment operations
                case BINARY_OP_ASSIGN:
                case BINARY_OP_ADD_ASSIGN:
                case BINARY_OP_SUB_ASSIGN:
                case BINARY_OP_MUL_ASSIGN:
                case BINARY_OP_DIV_ASSIGN:
                case BINARY_OP_MOD_ASSIGN:
                    return arc_analyze_assignment_binary_op(analyzer, expr, left_type, right_type);

                default:
                    arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                                       "Unknown binary operator");
                    return NULL;
            }
        }

        case AST_EXPR_UNARY: {
            ArcTypeInfo *operand_type =
                arc_analyze_expression_type(analyzer, expr->unary_expr.operand);
            if (!operand_type) {
                return NULL;
            }
            return arc_analyze_unary_op(analyzer, expr, operand_type);
        }

        case AST_EXPR_CALL: {
            return arc_analyze_function_call(analyzer, expr);
        }

        default:
            return arc_infer_expression_type(analyzer, expr);
    }
}

bool arc_analyze_statement(ArcSemanticAnalyzer *analyzer, ArcAstNode *stmt) {
    if (!stmt)
        return true;

    switch (stmt->type) {
        case AST_STMT_BLOCK: {
            // Create new block scope
            ArcScope *block_scope =
                arc_scope_create(analyzer, ARC_SCOPE_BLOCK, analyzer->current_scope);
            arc_scope_push(analyzer, block_scope);
            bool all_ok = true;
            // Analyze all statements in the block
            for (size_t i = 0; i < stmt->block_stmt.statement_count; i++) {
                // Check for unreachable code after return
                if (analyzer->has_return && i > 0) {
                    // Check if previous statement was a return
                    ArcAstNode *prev_stmt = stmt->block_stmt.statements[i - 1];
                    if (prev_stmt && prev_stmt->type == AST_STMT_RETURN) {
                        arc_check_unreachable_code(analyzer, stmt->block_stmt.statements[i]);
                    }
                }

                if (!arc_analyze_statement(analyzer, stmt->block_stmt.statements[i])) {
                    all_ok = false;
                }
            }

            // Pop block scope
            arc_scope_pop(analyzer);
            return all_ok;
        }
        case AST_STMT_VAR_DECL: {
            // Extract variable name from token
            size_t name_len = stmt->var_decl_stmt.name.length;
            if (name_len == 0 || !stmt->var_decl_stmt.name.start) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                                   "Variable declaration missing name");
                return false;
            }

            char *name = arc_arena_alloc(analyzer->arena, name_len + 1);
            if (!name)
                return false;
            strncpy(name, stmt->var_decl_stmt.name.start, name_len);
            name[name_len] = '\0';

            // Check for duplicate declaration in current scope
            if (arc_scope_lookup_symbol(analyzer->current_scope, name)) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                                   "Variable '%s' already declared in this scope", name);
                return false;
            }

            // Create symbol
            ArcSymbol *symbol = arc_symbol_create(analyzer, ARC_SYMBOL_VARIABLE, name);
            if (!symbol) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                                   "Failed to create symbol for variable '%s'", name);
                return false;
            }

            // Determine type
            if (stmt->var_decl_stmt.type_annotation) {
                symbol->type = arc_type_from_ast(analyzer, stmt->var_decl_stmt.type_annotation);
                if (!symbol->type) {
                    arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                                       "Invalid type annotation for variable '%s'", name);
                    return false;
                }
            }

            // Enhanced initializer analysis
            if (stmt->var_decl_stmt.initializer) {
                ArcTypeInfo *init_type =
                    arc_analyze_expression_type(analyzer, stmt->var_decl_stmt.initializer);
                if (!init_type) {
                    arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                                       "Invalid initializer for variable '%s'", name);
                    return false;
                }

                // If no explicit type, use enhanced type inference
                if (!symbol->type) {
                    symbol->type =
                        arc_infer_expression_type(analyzer, stmt->var_decl_stmt.initializer);
                    if (!symbol->type) {
                        symbol->type = init_type;  // Fallback to basic analysis
                    }
                } else {  // Enhanced type compatibility checking
                    if (!arc_type_is_assignable(init_type, symbol->type)) {
                        arc_diagnostic_add(
                            analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                            "Type mismatch in variable '%s' initialization: cannot assign %s to %s",
                            name, arc_type_to_string(init_type), arc_type_to_string(symbol->type));
                        return false;
                    }
                }

                // Mark as initialized
                symbol->is_defined = true;
            } else {
                // Variable declared without initializer
                if (!symbol->type) {
                    arc_diagnostic_add(
                        analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                        "Variable '%s' declared without type annotation or initializer", name);
                    return false;
                }
                // Mark as uninitialized
                symbol->is_defined = false;
            }

            // Variables must have a type by now
            if (!symbol->type) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                                   "Variable '%s' has no type annotation or initializer", name);
                return false;
            }

            // Set other symbol properties
            symbol->declaration_node = stmt;
            symbol->is_mutable = true;  // var declarations are mutable
            symbol->is_public = false;  // local variables are not public
            symbol->is_defined = true;

            // Add to current scope
            if (!arc_scope_add_symbol(analyzer->current_scope, symbol)) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                                   "Failed to add variable '%s' to scope", name);
                return false;
            }

            return true;
        }

        case AST_STMT_CONST_DECL: {
            // Similar to var declaration but immutable
            // TODO: Implement const declaration analysis
            arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_WARNING, stmt->source_info,
                               "Const declaration analysis not yet implemented");
            return true;
        }
        case AST_STMT_RETURN: {
            // Enhanced return statement analysis
            if (stmt->return_stmt.value) {
                ArcTypeInfo *return_type =
                    arc_analyze_expression_type(analyzer, stmt->return_stmt.value);
                if (!return_type) {
                    arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                                       "Invalid return expression");
                    return false;
                }

                // Enhanced return type compatibility checking
                if (!arc_analyze_return_type_compatibility(analyzer, return_type,
                                                           stmt->source_info)) {
                    return false;
                }
            } else {
                // Return without value - check if function expects void
                if (!arc_analyze_return_type_compatibility(analyzer, NULL, stmt->source_info)) {
                    return false;
                }
            }

            analyzer->has_return = true;
            return true;
        }

        case AST_STMT_EXPRESSION: {
            // Analyze the expression
            ArcTypeInfo *expr_type =
                arc_analyze_expression_type(analyzer, stmt->expr_stmt.expression);
            return expr_type != NULL;
        }

        default:
            // For unimplemented statement types, just emit a warning and continue
            arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_WARNING, stmt->source_info,
                               "Statement analysis not yet implemented for this statement type");
            return true;
    }
}

// === ENHANCED SEMANTIC ANALYSIS FEATURES ===

// Enhanced type checking utilities
static bool arc_type_can_be_null(ArcTypeInfo *type) {
    return type && (type->kind == ARC_TYPE_POINTER || type->kind == ARC_TYPE_OPTIONAL);
}

static bool arc_type_is_assignable(ArcTypeInfo *from, ArcTypeInfo *to) {
    if (!from || !to)
        return false;

    // Exact type match
    if (arc_type_is_compatible(from, to))
        return true;

    // Numeric conversions (widening)
    if (arc_type_is_numeric(from) && arc_type_is_numeric(to)) {
        // Allow i32 -> i64, f32 -> f64, etc.
        if (from->kind == ARC_TYPE_PRIMITIVE && to->kind == ARC_TYPE_PRIMITIVE) {
            // Simple widening rules
            if (from->primitive.primitive_type == TOKEN_KEYWORD_I32 &&
                to->primitive.primitive_type == TOKEN_KEYWORD_I64)
                return true;
            if (from->primitive.primitive_type == TOKEN_KEYWORD_F32 &&
                to->primitive.primitive_type == TOKEN_KEYWORD_F64)
                return true;
        }
    }

    // Null can be assigned to optional or pointer types
    if (from->kind == ARC_TYPE_VOID && arc_type_can_be_null(to))
        return true;

    return false;
}

static int arc_get_type_precedence(ArcTypeInfo *type) {
    if (!type || type->kind != ARC_TYPE_PRIMITIVE)
        return 0;

    switch (type->primitive.primitive_type) {
        case TOKEN_KEYWORD_BOOL:
            return 1;
        case TOKEN_KEYWORD_I8:
        case TOKEN_KEYWORD_U8:
            return 2;
        case TOKEN_KEYWORD_I16:
        case TOKEN_KEYWORD_U16:
            return 3;
        case TOKEN_KEYWORD_I32:
        case TOKEN_KEYWORD_U32:
            return 4;
        case TOKEN_KEYWORD_I64:
        case TOKEN_KEYWORD_U64:
            return 5;
        case TOKEN_KEYWORD_F32:
            return 6;
        case TOKEN_KEYWORD_F64:
            return 7;
        default:
            return 0;
    }
}

// Enhanced variable initialization tracking
static void arc_mark_variable_used(ArcSemanticAnalyzer *analyzer, const char *name) {
    ArcSymbol *symbol = arc_scope_lookup_symbol_recursive(analyzer->current_scope, name);
    if (symbol && symbol->kind == ARC_SYMBOL_VARIABLE) {
        // Mark as used (we could add a 'used' flag to ArcSymbol)
        // For now, just note that we found the symbol
    }
}

static bool arc_check_variable_initialization(ArcSemanticAnalyzer *analyzer, const char *var_name,
                                              ArcSourceInfo source_info) {
    ArcSymbol *symbol = arc_scope_lookup_symbol_recursive(analyzer->current_scope, var_name);
    if (!symbol) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, source_info, "Undeclared variable '%s'",
                           var_name);
        return false;
    }

    if (symbol->kind == ARC_SYMBOL_VARIABLE && !symbol->is_defined) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, source_info,
                           "Use of uninitialized variable '%s'", var_name);
        return false;
    }

    arc_mark_variable_used(analyzer, var_name);
    return true;
}

// Enhanced assignment analysis
static bool arc_analyze_assignment_target(ArcSemanticAnalyzer *analyzer, ArcAstNode *expr) {
    if (!expr)
        return false;

    switch (expr->type) {
        case AST_IDENTIFIER: {
            // Extract identifier name
            size_t name_len = expr->identifier.token.length;
            char *name = arc_arena_alloc(analyzer->arena, name_len + 1);
            if (!name)
                return false;
            strncpy(name, expr->identifier.token.start, name_len);
            name[name_len] = '\0';

            ArcSymbol *symbol = arc_scope_lookup_symbol_recursive(analyzer->current_scope, name);
            if (!symbol) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                                   "Cannot assign to undeclared variable '%s'", name);
                return false;
            }

            if (!symbol->is_mutable) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                                   "Cannot assign to immutable variable '%s'", name);
                return false;
            }

            return true;
        }

        case AST_EXPR_FIELD_ACCESS:
            // TODO: Implement field access assignment analysis
            return true;

        case AST_EXPR_INDEX:
            // TODO: Implement array index assignment analysis
            return true;

        default:
            arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                               "Invalid assignment target");
            return false;
    }
}

// Enhanced function analysis
static bool arc_validate_function_signature(ArcSemanticAnalyzer *analyzer, ArcAstNode *func_decl) {
    if (!func_decl || func_decl->type != AST_DECL_FUNCTION)
        return false;

    // Check for parameter name conflicts
    for (size_t i = 0; i < func_decl->function_decl.parameter_count; i++) {
        for (size_t j = i + 1; j < func_decl->function_decl.parameter_count; j++) {
            ArcAstNode *param1 = func_decl->function_decl.parameters[i];
            ArcAstNode *param2 = func_decl->function_decl.parameters[j];

            if (param1 && param2 &&
                param1->parameter.name.length == param2->parameter.name.length &&
                strncmp(param1->parameter.name.start, param2->parameter.name.start,
                        param1->parameter.name.length) == 0) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, param2->source_info,
                                   "Duplicate parameter name");
                return false;
            }
        }
    }

    return true;
}

static bool arc_analyze_return_type_compatibility(ArcSemanticAnalyzer *analyzer,
                                                  ArcTypeInfo *expr_type,
                                                  ArcSourceInfo source_info) {
    if (!analyzer->current_function) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, source_info,
                           "Return statement outside of function");
        return false;
    }

    // Get expected return type from current function
    ArcTypeInfo *expected_type =
        analyzer->current_function->type;  // This would need to be properly set

    if (!expected_type) {
        // Void function - no return value expected
        if (expr_type && expr_type->kind != ARC_TYPE_VOID) {
            arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_WARNING, source_info,
                               "Return value in void function will be ignored");
        }
        return true;
    }

    if (!expr_type) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, source_info,
                           "Function expects return value but none provided");
        return false;
    }

    if (!arc_type_is_assignable(expr_type, expected_type)) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, source_info, "Return type mismatch");
        return false;
    }

    return true;
}

// Enhanced control flow analysis
static void arc_check_unreachable_code(ArcSemanticAnalyzer *analyzer, ArcAstNode *stmt) {
    if (!stmt || !analyzer->has_return)
        return;

    arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_WARNING, stmt->source_info,
                       "Unreachable code after return statement");
}

// Enhanced expression analysis with better type inference
static ArcTypeInfo *arc_infer_expression_type(ArcSemanticAnalyzer *analyzer, ArcAstNode *expr) {
    if (!expr)
        return NULL;

    switch (expr->type) {
        case AST_LITERAL_INT: {
            // Try to infer the best integer type based on value
            // For now, default to i32
            return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_I32);
        }

        case AST_LITERAL_FLOAT: {
            // Try to infer f32 vs f64 based on precision needed
            // For now, default to f64
            return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_F64);
        }

        case AST_EXPR_BINARY: {
            ArcTypeInfo *left_type = arc_analyze_expression_type(analyzer, expr->binary_expr.left);
            ArcTypeInfo *right_type =
                arc_analyze_expression_type(analyzer, expr->binary_expr.right);

            if (!left_type || !right_type)
                return NULL;

            // Enhanced binary operation type inference
            switch (expr->binary_expr.op_type) {
                case BINARY_OP_ADD:
                case BINARY_OP_SUB:
                case BINARY_OP_MUL:
                case BINARY_OP_DIV: {
                    // Choose the type with higher precedence
                    int left_prec = arc_get_type_precedence(left_type);
                    int right_prec = arc_get_type_precedence(right_type);
                    return (left_prec >= right_prec) ? left_type : right_type;
                }

                case BINARY_OP_EQ:
                case BINARY_OP_NE:
                case BINARY_OP_LT:
                case BINARY_OP_LE:
                case BINARY_OP_GT:
                case BINARY_OP_GE:
                    return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_BOOL);

                default:
                    return left_type;
            }
        }

        default:
            return arc_analyze_expression_type(analyzer, expr);
    }
}

// Check for unused variables in all scopes
static void arc_check_unused_variables(ArcSemanticAnalyzer *analyzer) {
    if (!analyzer || !analyzer->global_scope)
        return;

    // For now, just check global scope symbols as we don't have child scope tracking yet
    // TODO: Implement proper scope traversal when child scope tracking is added
    arc_check_unused_in_scope(analyzer, analyzer->global_scope);
}

// Helper to check unused variables in a scope
static void arc_check_unused_in_scope(ArcSemanticAnalyzer *analyzer, ArcScope *scope) {
    if (!scope)
        return;

    // Check each symbol in this scope
    for (size_t i = 0; i < scope->symbol_count; i++) {
        ArcSymbol *symbol = scope->symbols[i];
        if (symbol && symbol->kind == ARC_SYMBOL_VARIABLE) {
            // For now, we'll add a basic check - in a full implementation,
            // we'd need to track variable usage throughout analysis
            // TODO: Add is_used field to ArcSymbol and track usage during analysis

            // Skip check for now since we don't have usage tracking implemented
            // This would warn about all variables which isn't useful yet
            (void)symbol;  // Suppress unused variable warning
        }
    }
}
