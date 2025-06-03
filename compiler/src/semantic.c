// compiler/src/semantic.c
#include "arc/semantic.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void arc_semantic_analyzer_setup_builtins(ArcSemanticAnalyzer *analyzer);

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
    // TODO: Check that left operand is an lvalue (assignable)
    // TODO: Check type compatibility for assignment

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
    // TODO: Implement function call analysis
    // 1. Resolve the function being called
    // 2. Check argument count and types
    // 3. Return the function's return type
    arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_WARNING, expr->source_info,
                       "Function call analysis not yet implemented");
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
    }

    // Analyze all declarations in the program
    for (size_t i = 0; i < ast->program.declaration_count; i++) {
        if (!arc_analyze_declaration(analyzer, ast->program.declarations[i])) {
            // Continue analyzing other declarations even if one fails
        }
    }

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
            symbol->type = arc_type_create(analyzer, ARC_TYPE_FUNCTION);

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

            ArcSymbol *symbol = arc_scope_lookup_symbol_recursive(analyzer->current_scope, name);
            if (!symbol) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                                   "Undeclared identifier '%s'", name);
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
            arc_diagnostic_add(
                analyzer, ARC_DIAGNOSTIC_WARNING, expr->source_info,
                "Expression type analysis not yet implemented for this expression type");
            return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_VOID);
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

            // Check initializer if present
            if (stmt->var_decl_stmt.initializer) {
                ArcTypeInfo *init_type =
                    arc_analyze_expression_type(analyzer, stmt->var_decl_stmt.initializer);
                if (!init_type) {
                    arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                                       "Invalid initializer for variable '%s'", name);
                    return false;
                }

                // If no explicit type, infer from initializer
                if (!symbol->type) {
                    symbol->type = init_type;
                } else {
                    // Check type compatibility
                    if (!arc_type_is_assignable_to(init_type, symbol->type)) {
                        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                                           "Type mismatch in variable '%s' initialization", name);
                        return false;
                    }
                }
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
            // TODO: Check if return type matches function return type
            if (stmt->return_stmt.value) {
                ArcTypeInfo *return_type =
                    arc_analyze_expression_type(analyzer, stmt->return_stmt.value);
                if (!return_type) {
                    arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                                       "Invalid return expression");
                    return false;
                }
                // TODO: Check compatibility with function return type
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
