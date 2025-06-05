// compiler/src/semantic.c
#include "arc/semantic.h"
#include "arc/lexer.h"
#include "arc/parser.h"
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
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
static bool arc_type_is_comparable(ArcTypeInfo *left, ArcTypeInfo *right);

// === MODULE RESOLUTION & IMPORT SYSTEM ===

// State of a module during the loading process to detect circular dependencies.
typedef enum {
    ARC_MODULE_STATE_UNLOADED,  // Not yet processed
    ARC_MODULE_STATE_LOADING,   // Currently being analyzed (for cycle detection)
    ARC_MODULE_STATE_LOADED,    // Successfully analyzed
    ARC_MODULE_STATE_FAILED,    // Analysis failed
} ArcModuleFileState;

typedef struct ArcModuleFile {
    char *filepath;
    char *content;
    ArcAstNode *ast;
    ArcScope *global_scope;  // The module's global scope containing its symbols
    ArcModuleFileState state;
    struct ArcModuleFile *next;
} ArcModuleFile;

// Module resolution context
typedef struct ArcModuleResolver {
    ArcModuleFile *loaded_modules;  // Cache of loaded modules
    char **search_paths;            // Array of search paths for modules
    size_t search_path_count;
} ArcModuleResolver;

// Module resolver global instance
static ArcModuleResolver *module_resolver = NULL;

// Module system function declarations
static void arc_module_resolver_init(void);
static void arc_module_resolver_cleanup(void);
static ArcModuleFile *arc_module_resolve(const char *module_name);
static char *arc_module_construct_path(const char *module_name);
static char *arc_module_load_content(const char *filepath);
static ArcModuleFile *arc_load_and_analyze_module(ArcSemanticAnalyzer *analyzer,
                                                  const char *module_name,
                                                  ArcSourceInfo import_site);

// Initialize module resolver
static void arc_module_resolver_init(void) {
    if (module_resolver)
        return;

    module_resolver = malloc(sizeof(ArcModuleResolver));
    if (module_resolver) {
        module_resolver->loaded_modules = NULL;
        module_resolver->search_paths = malloc(sizeof(char *) * 4);
        module_resolver->search_path_count = 0;

        // Add default search paths
        if (module_resolver->search_paths) {
            module_resolver->search_paths[module_resolver->search_path_count++] = ".";
            module_resolver->search_paths[module_resolver->search_path_count++] = "./src";
            module_resolver->search_paths[module_resolver->search_path_count++] = "./stdlib";
        }
    }
}

// Cleanup module resolver
static void arc_module_resolver_cleanup(void) {
    if (!module_resolver)
        return;

    ArcModuleFile *current = module_resolver->loaded_modules;
    while (current) {
        ArcModuleFile *next = current->next;
        free(current->filepath);
        free(current->content);
        // AST and scope are managed by arenas, which should be cleaned up
        // when their respective analyzers are destroyed.
        free(current);
        current = next;
    }

    free(module_resolver->search_paths);
    free(module_resolver);
    module_resolver = NULL;
}

// Construct module file path from module name
static char *arc_module_construct_path(const char *module_name) {
    if (!module_name || !module_resolver)
        return NULL;

    for (size_t i = 0; i < module_resolver->search_path_count; i++) {
        size_t path_len = strlen(module_resolver->search_paths[i]) + strlen(module_name) + 10;
        char *filepath = malloc(path_len);
        if (!filepath)
            continue;
#ifdef _WIN32
        snprintf(filepath, path_len, "%s\\%s.arc", module_resolver->search_paths[i], module_name);
#else
        snprintf(filepath, path_len, "%s/%s.arc", module_resolver->search_paths[i], module_name);
#endif

        FILE *file = fopen(filepath, "r");
        if (file) {
            fclose(file);
            return filepath;
        }
        free(filepath);
    }
    return NULL;
}

// Load module content from file
static char *arc_module_load_content(const char *filepath) {
    FILE *file = fopen(filepath, "rb");  // Use "rb" for cross-platform consistency
    if (!file)
        return NULL;

    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);

    if (size <= 0) {
        fclose(file);
        return size == 0 ? strdup("") : NULL;
    }

    char *content = malloc(size + 1);
    if (!content) {
        fclose(file);
        return NULL;
    }

    size_t read_size = fread(content, 1, size, file);
    content[read_size] = '\0';
    fclose(file);

    return content;
}

// Find or load a module by name
static ArcModuleFile *arc_module_resolve(const char *module_name) {
    if (!module_name || !module_resolver)
        return NULL;

    // Check cache first
    for (ArcModuleFile *mod = module_resolver->loaded_modules; mod; mod = mod->next) {
        // A more robust check would be to have a canonical module name/path
        if (strstr(mod->filepath, module_name)) {
            return mod;
        }
    }

    char *filepath = arc_module_construct_path(module_name);
    if (!filepath)
        return NULL;

    char *content = arc_module_load_content(filepath);
    if (!content) {
        free(filepath);
        return NULL;
    }

    ArcModuleFile *module_file = malloc(sizeof(ArcModuleFile));
    if (!module_file) {
        free(filepath);
        free(content);
        return NULL;
    }

    module_file->filepath = filepath;
    module_file->content = content;
    module_file->ast = NULL;
    module_file->global_scope = NULL;
    module_file->state = ARC_MODULE_STATE_UNLOADED;
    module_file->next = module_resolver->loaded_modules;
    module_resolver->loaded_modules = module_file;

    return module_file;
}

// The core function to load, parse, and analyze a module.
static ArcModuleFile *arc_load_and_analyze_module(ArcSemanticAnalyzer *analyzer,
                                                  const char *module_name,
                                                  ArcSourceInfo import_site) {
    ArcModuleFile *module_file = arc_module_resolve(module_name);
    if (!module_file) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, import_site, "Module '%s' not found.",
                           module_name);
        return NULL;
    }

    // Handle module state for caching and cycle detection
    if (module_file->state == ARC_MODULE_STATE_LOADING) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, import_site,
                           "Circular dependency detected: module '%s' is already being loaded.",
                           module_name);
        return NULL;
    }
    if (module_file->state == ARC_MODULE_STATE_LOADED) {
        return module_file;  // Already analyzed, return cached result
    }
    if (module_file->state == ARC_MODULE_STATE_FAILED) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_NOTE, import_site,
                           "Module '%s' failed to load in a previous attempt.", module_name);
        return NULL;
    }

    module_file->state = ARC_MODULE_STATE_LOADING;  // Parse the module content into an AST
    ArcLexer lexer;
    ArcParser parser;
    arc_lexer_init(&lexer, module_file->content, module_file->filepath);
    arc_parser_init_simple(&parser, &lexer);
    module_file->ast = arc_parser_parse_program(&parser);
    // TODO: Check for and handle parser errors
    arc_parser_cleanup(&parser);

    if (!module_file->ast) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, import_site,
                           "Failed to parse module '%s'.", module_name);
        module_file->state = ARC_MODULE_STATE_FAILED;
        return NULL;
    }

    ArcSemanticAnalyzer *module_analyzer = arc_semantic_analyzer_create_with_arena(analyzer->arena);
    bool success = arc_semantic_analyze(module_analyzer, module_file->ast);

    if (success) {
        module_file->state = ARC_MODULE_STATE_LOADED;
        module_file->global_scope = module_analyzer->global_scope;
        module_analyzer->global_scope = NULL;
    } else {
        module_file->state = ARC_MODULE_STATE_FAILED;
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, import_site,
                           "Errors found while analyzing module '%s'. See details below.",
                           module_name);
        // Propagate diagnostics from the module analyzer to the main one
        for (ArcDiagnostic *d = module_analyzer->diagnostics; d; d = d->next) {
            arc_diagnostic_add(analyzer, d->level, d->source_info, d->message);
        }
    }

    arc_semantic_analyzer_destroy(module_analyzer);
    return success ? module_file : NULL;
}

// === ENHANCED TYPE SYSTEM HELPERS ===

// Helper to create optional type
static ArcTypeInfo *arc_type_create_optional(ArcSemanticAnalyzer *analyzer, ArcTypeInfo *inner_type)
    __attribute__((unused));
static ArcTypeInfo *arc_type_create_optional(ArcSemanticAnalyzer *analyzer,
                                             ArcTypeInfo *inner_type) {
    if (!inner_type)
        return NULL;

    ArcTypeInfo *optional_type = arc_type_create(analyzer, ARC_TYPE_OPTIONAL);
    if (optional_type) {
        optional_type->optional.inner_type = inner_type;
        optional_type->is_resolved = inner_type->is_resolved;
        optional_type->size = inner_type->size + sizeof(bool);  // Inner type + bool flag
        optional_type->alignment = inner_type->alignment;
    }
    return optional_type;
}

// Helper to create array type
static ArcTypeInfo *arc_type_create_array(ArcSemanticAnalyzer *analyzer, ArcTypeInfo *element_type,
                                          long long size) {
    if (!element_type)
        return NULL;

    ArcTypeInfo *array_type = arc_type_create(analyzer, ARC_TYPE_ARRAY);
    if (array_type) {
        array_type->array.element_type = element_type;
        array_type->array.size = size;
        array_type->is_resolved = element_type->is_resolved;
        if (element_type->is_resolved && size > 0) {
            array_type->size = element_type->size * (size_t)size;
            array_type->alignment = element_type->alignment;
        }
    }
    return array_type;
}

// Helper to create slice type
static ArcTypeInfo *arc_type_create_slice(ArcSemanticAnalyzer *analyzer,
                                          ArcTypeInfo *element_type) {
    if (!element_type)
        return NULL;

    ArcTypeInfo *slice_type = arc_type_create(analyzer, ARC_TYPE_SLICE);
    if (slice_type) {
        slice_type->slice.element_type = element_type;
        slice_type->is_resolved = true;
        slice_type->size = sizeof(void *) + sizeof(size_t);  // Pointer + length
        slice_type->alignment = sizeof(void *);
    }
    return slice_type;
}

// Helper to create function type
static ArcTypeInfo *arc_type_create_function(ArcSemanticAnalyzer *analyzer,
                                             ArcTypeInfo **param_types, size_t param_count,
                                             ArcTypeInfo *return_type) {
    ArcTypeInfo *func_type = arc_type_create(analyzer, ARC_TYPE_FUNCTION);
    if (func_type) {
        func_type->function.parameter_types = param_types;
        func_type->function.parameter_count = param_count;
        func_type->function.return_type = return_type;
        func_type->is_resolved = true;
        func_type->size = sizeof(void *);  // Function pointer size
        func_type->alignment = sizeof(void *);
    }
    return func_type;
}

// Enhanced type checking for Arc's distinctive features
static bool arc_type_supports_pipeline(ArcTypeInfo *target_type) {
    return target_type && target_type->kind == ARC_TYPE_FUNCTION;
}

static bool arc_type_supports_null_coalescing(ArcTypeInfo *type) {
    return type && type->kind == ARC_TYPE_OPTIONAL;
}

static bool arc_type_supports_force_unwrap(ArcTypeInfo *type) {
    return type && type->kind == ARC_TYPE_OPTIONAL;
}

// Enhanced type compatibility checking
static bool arc_types_are_equivalent(ArcTypeInfo *a, ArcTypeInfo *b) {
    if (a == b)
        return true;
    if (!a || !b)
        return false;
    if (a->kind != b->kind)
        return false;

    switch (a->kind) {
        case ARC_TYPE_PRIMITIVE:
            return a->primitive.primitive_type == b->primitive.primitive_type;

        case ARC_TYPE_POINTER:
            return a->pointer.is_mutable == b->pointer.is_mutable &&
                   arc_types_are_equivalent(a->pointer.pointed_type, b->pointer.pointed_type);

        case ARC_TYPE_ARRAY:
            return a->array.size == b->array.size &&
                   arc_types_are_equivalent(a->array.element_type, b->array.element_type);

        case ARC_TYPE_SLICE:
            return arc_types_are_equivalent(a->slice.element_type, b->slice.element_type);

        case ARC_TYPE_OPTIONAL:
            return arc_types_are_equivalent(a->optional.inner_type, b->optional.inner_type);

        case ARC_TYPE_FUNCTION:
            if (a->function.parameter_count != b->function.parameter_count)
                return false;
            if (!arc_types_are_equivalent(a->function.return_type, b->function.return_type))
                return false;

            for (size_t i = 0; i < a->function.parameter_count; i++) {
                if (!arc_types_are_equivalent(a->function.parameter_types[i],
                                              b->function.parameter_types[i])) {
                    return false;
                }
            }
            return true;

        case ARC_TYPE_VOID:
        case ARC_TYPE_ERROR:
            return true;

        default:
            return false;
    }
}

// Enhanced variable usage tracking
typedef struct {
    char *name;
    bool is_used;
    bool is_initialized;
    ArcSourceInfo declaration_site;
} ArcVariableUsage;

// Context tracking for semantic analysis
typedef struct {
    bool in_async_context;
    bool in_unsafe_context;
    bool in_comptime_context;
    int loop_depth;
    int defer_count;
} ArcSemanticContext;

// Add context to semantic analyzer (would need to be added to the struct definition)
static void arc_semantic_push_context(ArcSemanticAnalyzer *analyzer) {
    // TODO: Implement context stack when struct is expanded
}

static void arc_semantic_pop_context(ArcSemanticAnalyzer *analyzer) {
    // TODO: Implement context stack when struct is expanded
}

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
                case TOKEN_STRING_LITERAL:
                    return "string";
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

// Enhanced expression type inference
static ArcTypeInfo *arc_infer_literal_type(ArcSemanticAnalyzer *analyzer, ArcAstNode *expr) {
    switch (expr->type) {
        case AST_LITERAL_INT:
            // Enhanced integer literal inference based on value
            if (expr->literal_int.value >= (-2147483647 - 1) &&
                expr->literal_int.value <= 2147483647) {
                return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_I32);
            } else {
                return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_I64);
            }

        case AST_LITERAL_FLOAT:
            // Default to f64 for floating point literals
            return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_F64);

        case AST_LITERAL_STRING:
            // TODO: Return proper string type when implemented
            return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_CHAR);

        case AST_LITERAL_BOOL:
            return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_BOOL);

        case AST_LITERAL_CHAR:
            return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_CHAR);

        case AST_LITERAL_NULL:
            // TODO: Return proper null type
            return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_VOID);

        default:
            return NULL;
    }
}

// === SEMANTIC ANALYZER LIFECYCLE ===
ArcSemanticAnalyzer *arc_semantic_analyzer_create_with_arena(ArcArena *arena) {
    ArcSemanticAnalyzer *analyzer = malloc(sizeof(ArcSemanticAnalyzer));
    if (!analyzer)
        return NULL;

    // If an arena is provided, use it. Otherwise, create a new one.
    if (arena) {
        analyzer->arena = arena;
    } else {
        analyzer->arena = arc_arena_create(1024 * 1024);
        if (!analyzer->arena) {
            free(analyzer);
            return NULL;
        }
    }

    analyzer->global_scope = arc_scope_create(analyzer, ARC_SCOPE_GLOBAL, NULL);
    analyzer->current_scope = analyzer->global_scope;
    analyzer->builtin_types = NULL;
    analyzer->builtin_type_count = 0;
    analyzer->diagnostics = NULL;
    analyzer->last_diagnostic = NULL;
    analyzer->error_count = 0;
    analyzer->warning_count = 0;
    analyzer->current_function = NULL;
    analyzer->in_loop = false;
    analyzer->has_return = false;

    arc_semantic_analyzer_setup_builtins(analyzer);

    // The module resolver is global, so just point to it.
    analyzer->module_resolver = module_resolver;

    return analyzer;
}

ArcSemanticAnalyzer *arc_semantic_analyzer_create(void) {
    // This is the main entry point, so it creates the primary arena.
    return arc_semantic_analyzer_create_with_arena(NULL);
}

void arc_semantic_analyzer_destroy(ArcSemanticAnalyzer *analyzer) {
    if (!analyzer)
        return;

    // The analyzer is only responsible for its own arena IF it created it.
    // In our new model, only the main analyzer creates its arena, and the
    // temporary ones borrow it. The main analyzer's destruction will be
    // handled by the CLI. For temporary analyzers, we just free the struct.
    // A better approach is to track ownership, but this is simpler for now.
    // Let's assume the CLI will destroy the main arena.
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
    // Check if types are comparable
    if (arc_type_is_numeric(left_type) && arc_type_is_numeric(right_type)) {
        return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_BOOL);
    }

    // Same primitive types can be compared
    if (left_type->kind == ARC_TYPE_PRIMITIVE && right_type->kind == ARC_TYPE_PRIMITIVE &&
        left_type->primitive.primitive_type == right_type->primitive.primitive_type) {
        return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_BOOL);
    }

    // Otherwise, incompatible comparison
    arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                       "Cannot compare types '%s' and '%s'", arc_type_to_string(left_type),
                       arc_type_to_string(right_type));
    return NULL;
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
    if (!expr || expr->type != AST_EXPR_UNARY) {
        return operand_type;
    }

    ArcUnaryOperator op = expr->unary_expr.op_type;

    switch (op) {
        case UNARY_OP_NOT:
        case UNARY_OP_LOGICAL_NOT:
            // Logical NOT: operand must be boolean, result is boolean
            if (!arc_type_is_boolean(operand_type)) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                                   "Logical NOT operator requires boolean operand, got '%s'",
                                   arc_type_to_string(operand_type));
                return arc_type_create(analyzer, ARC_TYPE_ERROR);
            }
            return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_BOOL);

        case UNARY_OP_NEGATE:
            // Arithmetic negation: operand must be numeric, result is same type
            if (!arc_type_is_numeric(operand_type)) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                                   "Arithmetic negation requires numeric operand, got '%s'",
                                   arc_type_to_string(operand_type));
                return arc_type_create(analyzer, ARC_TYPE_ERROR);
            }
            return operand_type;

        case UNARY_OP_ADDRESS:
            // Address-of operator: creates pointer to operand type
            {
                ArcTypeInfo *pointer_type = arc_type_create(analyzer, ARC_TYPE_POINTER);
                if (pointer_type) {
                    pointer_type->pointer.pointed_type = operand_type;
                    pointer_type->pointer.is_mutable = false;  // Default to immutable
                    pointer_type->is_resolved = true;
                }
                return pointer_type;
            }

        case UNARY_OP_DEREFERENCE:
            // Dereference operator: operand must be pointer, result is pointed type
            if (operand_type->kind != ARC_TYPE_POINTER) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                                   "Dereference operator requires pointer operand, got '%s'",
                                   arc_type_to_string(operand_type));
                return arc_type_create(analyzer, ARC_TYPE_ERROR);
            }
            return operand_type->pointer.pointed_type;

        case UNARY_OP_FORCE_UNWRAP:
            // Force unwrap operator: operand must be optional, result is inner type
            if (operand_type->kind != ARC_TYPE_OPTIONAL) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                                   "Force unwrap operator requires optional operand, got '%s'",
                                   arc_type_to_string(operand_type));
                return arc_type_create(analyzer, ARC_TYPE_ERROR);
            }
            return operand_type->optional.inner_type;

        default:
            arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_WARNING, expr->source_info,
                               "Unknown unary operator");
            return operand_type;
    }
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

// Enhanced binary operator analysis for Arc's distinctive operators
static ArcTypeInfo *arc_analyze_pipeline_binary_op(ArcSemanticAnalyzer *analyzer, ArcAstNode *expr,
                                                   ArcTypeInfo *left_type,
                                                   ArcTypeInfo *right_type) {
    // Pipeline operators: |>, ~>, <|
    // Left operand should be a value, right operand should be a function

    if (right_type->kind != ARC_TYPE_FUNCTION) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                           "Right operand of pipeline operator must be a function, got '%s'",
                           arc_type_to_string(right_type));
        return arc_type_create(analyzer, ARC_TYPE_ERROR);
    }

    // Check if left type is compatible with first parameter of function
    if (right_type->function.parameter_count == 0) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                           "Pipeline target function must have at least one parameter");
        return arc_type_create(analyzer, ARC_TYPE_ERROR);
    }

    ArcTypeInfo *first_param_type = right_type->function.parameter_types[0];
    if (!arc_type_is_assignable_to(left_type, first_param_type)) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                           "Cannot pipe value of type '%s' to function expecting '%s'",
                           arc_type_to_string(left_type), arc_type_to_string(first_param_type));
        return arc_type_create(analyzer, ARC_TYPE_ERROR);
    }

    // Result type is the return type of the function
    return right_type->function.return_type;
}

static ArcTypeInfo *arc_analyze_null_coalescing_binary_op(ArcSemanticAnalyzer *analyzer,
                                                          ArcAstNode *expr, ArcTypeInfo *left_type,
                                                          ArcTypeInfo *right_type) {
    // Null coalescing operator: ??
    // Left operand should be optional, right operand should be same type as inner type

    if (left_type->kind != ARC_TYPE_OPTIONAL) {
        arc_diagnostic_add(
            analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
            "Left operand of null coalescing operator must be optional type, got '%s'",
            arc_type_to_string(left_type));
        return arc_type_create(analyzer, ARC_TYPE_ERROR);
    }

    ArcTypeInfo *inner_type = left_type->optional.inner_type;
    if (!arc_type_is_assignable_to(right_type, inner_type)) {
        arc_diagnostic_add(
            analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
            "Right operand type '%s' is not compatible with optional inner type '%s'",
            arc_type_to_string(right_type), arc_type_to_string(inner_type));
        return arc_type_create(analyzer, ARC_TYPE_ERROR);
    }

    // Result type is the non-optional inner type
    return inner_type;
}

static ArcTypeInfo *arc_analyze_scope_resolution_binary_op(ArcSemanticAnalyzer *analyzer,
                                                           ArcAstNode *expr, ArcTypeInfo *left_type,
                                                           ArcTypeInfo *right_type) {
    // Scope resolution operator: ::
    // This is handled differently - it's more about symbol resolution than type analysis
    // For now, we'll defer to the expression analysis that handles the actual symbol lookup

    // The type of a scope resolution expression is the type of the resolved symbol
    // This should be handled in the primary expression analysis
    return right_type;  // Right side determines the type
}

static ArcTypeInfo *arc_analyze_spaceship_binary_op(ArcSemanticAnalyzer *analyzer, ArcAstNode *expr,
                                                    ArcTypeInfo *left_type,
                                                    ArcTypeInfo *right_type) {
    // Spaceship operator: <=> (three-way comparison)
    // Returns an ordering type (similar to comparison but with three states)

    if (!arc_type_is_comparable(left_type, right_type)) {
        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                           "Cannot compare types '%s' and '%s' with spaceship operator",
                           arc_type_to_string(left_type), arc_type_to_string(right_type));
        return arc_type_create(analyzer, ARC_TYPE_ERROR);
    }

    // TODO: Return proper Ordering enum type when implemented
    // For now, return i32 (-1, 0, 1)
    return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_I32);
}

// Helper function to check if types are comparable
static bool arc_type_is_comparable(ArcTypeInfo *left, ArcTypeInfo *right) {
    // Both must be numeric, or both must be the same type
    if (arc_type_is_numeric(left) && arc_type_is_numeric(right)) {
        return true;
    }

    if (left->kind == right->kind) {
        switch (left->kind) {
            case ARC_TYPE_PRIMITIVE:
                return left->primitive.primitive_type == right->primitive.primitive_type;
            case ARC_TYPE_POINTER:
                return arc_type_is_compatible(left->pointer.pointed_type,
                                              right->pointer.pointed_type);
            default:
                return false;
        }
    }

    return false;
}

// Enhanced special binary operator analysis for Arc's distinctive operators
static ArcTypeInfo *arc_analyze_special_binary_op(ArcSemanticAnalyzer *analyzer, ArcAstNode *expr,
                                                  ArcTypeInfo *left_type, ArcTypeInfo *right_type) {
    ArcBinaryOperator op = expr->binary_expr.op_type;

    switch (op) {
        case BINARY_OP_PIPELINE:
        case BINARY_OP_REVERSE_PIPELINE:
            return arc_analyze_pipeline_binary_op(analyzer, expr, left_type, right_type);

        case BINARY_OP_ASYNC_PIPELINE:
            // Async pipeline: should return a Future or Task type
            // For now, return the same as regular pipeline
            return arc_analyze_pipeline_binary_op(analyzer, expr, left_type, right_type);

        case BINARY_OP_NULL_COALESCING:
            return arc_analyze_null_coalescing_binary_op(analyzer, expr, left_type, right_type);

        case BINARY_OP_SPACESHIP:
            return arc_analyze_spaceship_binary_op(analyzer, expr, left_type, right_type);

        case BINARY_OP_SCOPE_RESOLUTION:
            return arc_analyze_scope_resolution_binary_op(analyzer, expr, left_type, right_type);

        default:
            arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                               "Unknown special binary operator");
            return arc_type_create(analyzer, ARC_TYPE_ERROR);
    }
}

static ArcTypeInfo *arc_analyze_special_binary_op_type(ArcSemanticAnalyzer *analyzer,
                                                       ArcAstNode *expr, ArcTypeInfo *left_type,
                                                       ArcTypeInfo *right_type) {
    ArcBinaryOperator op = expr->binary_expr.op_type;

    switch (op) {
        case BINARY_OP_PIPELINE:
        case BINARY_OP_ASYNC_PIPELINE:
        case BINARY_OP_REVERSE_PIPELINE:
            // Pipeline operations return the return type of the target function
            if (right_type->kind == ARC_TYPE_FUNCTION) {
                return right_type->function.return_type;
            }
            return arc_type_create(analyzer, ARC_TYPE_ERROR);

        case BINARY_OP_NULL_COALESCING:
            // Null coalescing returns the non-optional inner type
            if (left_type->kind == ARC_TYPE_OPTIONAL) {
                return left_type->optional.inner_type;
            }
            return right_type;  // Fallback

        case BINARY_OP_SPACESHIP:
            // Spaceship operator returns comparison result (i32 for now)
            return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_I32);

        case BINARY_OP_SCOPE_RESOLUTION:
            return right_type;  // Right side determines the type

        default:
            return left_type;  // Default fallback
    }
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
    for (size_t i = 0; i < ast->program.declaration_count; i++) {
        arc_analyze_declaration(analyzer, ast->program.declarations[i]);
    }

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
            }  // Create function symbol
            ArcSymbol *symbol = arc_symbol_create(analyzer, ARC_SYMBOL_FUNCTION, name);
            if (!symbol) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, decl->source_info,
                                   "Failed to create symbol for function '%s'", name);
                return false;
            }  // Set function properties
            symbol->declaration_node = decl;
            symbol->is_public = true;  // For now, assume all functions are public
            symbol->is_defined = (decl->function_decl.body != NULL);
            symbol->parameter_count = decl->function_decl.parameter_count;  // Set parameter count

            // Create function type with return type
            symbol->type = arc_type_create(analyzer, ARC_TYPE_FUNCTION);
            if (symbol->type) {
                // Set return type from declaration
                if (decl->function_decl.return_type) {
                    symbol->type->function.return_type =
                        arc_type_from_ast(analyzer, decl->function_decl.return_type);
                } else {
                    // Default to void if no return type specified
                    symbol->type->function.return_type =
                        arc_type_get_builtin(analyzer, TOKEN_KEYWORD_VOID);
                }
            }
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

        case AST_DECL_EXTERN: {
            // Get extern function name from token
            if (decl->extern_decl.name.length == 0 || !decl->extern_decl.name.start) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, decl->source_info,
                                   "Extern function declaration missing name");
                return false;
            }

            // Extract name from token (we need to make a null-terminated copy)
            size_t name_len = decl->extern_decl.name.length;
            char *name = arc_arena_alloc(analyzer->arena, name_len + 1);
            if (!name) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, decl->source_info,
                                   "Failed to allocate memory for extern function name");
                return false;
            }
            strncpy(name, decl->extern_decl.name.start, name_len);
            name[name_len] = '\0';

            // Check for duplicate declaration in current scope
            if (arc_scope_lookup_symbol(analyzer->current_scope, name)) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, decl->source_info,
                                   "Extern function '%s' already declared in this scope", name);
                return false;
            }

            // Create extern function symbol
            ArcSymbol *symbol = arc_symbol_create(analyzer, ARC_SYMBOL_FUNCTION, name);
            if (!symbol) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, decl->source_info,
                                   "Failed to create symbol for extern function '%s'", name);
                return false;
            }

            // Set extern function properties
            symbol->declaration_node = decl;
            symbol->is_public = true;    // Extern functions are publicly accessible
            symbol->is_defined = false;  // Extern functions are declared but not defined in Arc
            symbol->is_extern = true;    // Mark as extern function
            symbol->parameter_count = decl->extern_decl.parameter_count;

            // Create function type with return type
            symbol->type = arc_type_create(analyzer, ARC_TYPE_FUNCTION);
            if (symbol->type) {
                // Set return type from declaration
                if (decl->extern_decl.return_type) {
                    ArcTypeInfo *return_type =
                        arc_type_from_ast(analyzer, decl->extern_decl.return_type);
                    if (!return_type) {
                        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, decl->source_info,
                                           "Failed to resolve return type for extern function '%s'",
                                           name);
                        return false;
                    }
                    symbol->type->function.return_type = return_type;
                } else {
                    // Default to void if no return type specified
                    symbol->type->function.return_type =
                        arc_type_get_builtin(analyzer, TOKEN_KEYWORD_VOID);
                }

                // Analyze parameter types
                if (decl->extern_decl.parameter_count > 0) {
                    symbol->type->function.parameter_types = arc_arena_alloc(
                        analyzer->arena, sizeof(ArcTypeInfo *) * decl->extern_decl.parameter_count);
                    if (!symbol->type->function.parameter_types) {
                        arc_diagnostic_add(
                            analyzer, ARC_DIAGNOSTIC_ERROR, decl->source_info,
                            "Failed to allocate parameter types for extern function '%s'", name);
                        return false;
                    }

                    for (size_t i = 0; i < decl->extern_decl.parameter_count; i++) {
                        ArcAstNode *param = decl->extern_decl.parameters[i];
                        if (param->type != AST_PARAMETER) {
                            arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, param->source_info,
                                               "Invalid parameter node in extern function '%s'",
                                               name);
                            return false;
                        }

                        ArcTypeInfo *param_type =
                            arc_type_from_ast(analyzer, param->parameter.type_annotation);
                        if (!param_type) {
                            arc_diagnostic_add(
                                analyzer, ARC_DIAGNOSTIC_ERROR, param->source_info,
                                "Failed to resolve parameter type in extern function '%s'", name);
                            return false;
                        }
                        symbol->type->function.parameter_types[i] = param_type;
                    }
                }
                symbol->type->function.parameter_count = decl->extern_decl.parameter_count;
            }

            // Add to current scope
            if (!arc_scope_add_symbol(analyzer->current_scope, symbol)) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, decl->source_info,
                                   "Failed to add extern function '%s' to scope", name);
                return false;
            }

            return true;
        }

        case AST_DECL_STRUCT: {
            // Struct declaration analysis
            // TODO: Implement when struct type system is ready
            arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_WARNING, decl->source_info,
                               "Struct declaration analysis not yet implemented");
            return true;
        }

        case AST_DECL_ENUM: {
            // Enum declaration analysis
            // TODO: Implement when enum type system is ready
            arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_WARNING, decl->source_info,
                               "Enum declaration analysis not yet implemented");
            return true;
        }

        case AST_DECL_TYPE_ALIAS: {
            // Type alias declaration analysis
            size_t name_len = decl->type_alias_decl.name.length;
            if (name_len == 0 || !decl->type_alias_decl.name.start) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, decl->source_info,
                                   "Type alias declaration missing name");
                return false;
            }

            char *name = arc_arena_alloc(analyzer->arena, name_len + 1);
            if (!name)
                return false;
            strncpy(name, decl->type_alias_decl.name.start, name_len);
            name[name_len] = '\0';

            // Check for duplicate declaration
            if (arc_scope_lookup_symbol(analyzer->current_scope, name)) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, decl->source_info,
                                   "Type '%s' already declared in this scope", name);
                return false;
            }

            // Create type symbol
            ArcSymbol *symbol = arc_symbol_create(analyzer, ARC_SYMBOL_TYPE, name);
            if (!symbol)
                return false;

            // Analyze the target type
            symbol->type = arc_type_from_ast(analyzer, decl->type_alias_decl.target_type);
            if (!symbol->type) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, decl->source_info,
                                   "Invalid target type for type alias '%s'", name);
                return false;
            }

            symbol->declaration_node = decl;
            symbol->is_public = true;  // Type aliases are typically public
            symbol->is_defined = true;

            return arc_scope_add_symbol(analyzer->current_scope, symbol);
        }

        case AST_DECL_INTERFACE: {
            // Interface declaration analysis
            // TODO: Implement when interface type system is ready
            arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_WARNING, decl->source_info,
                               "Interface declaration analysis not yet implemented");
            return true;
        }

        case AST_DECL_IMPL: {
            // Implementation declaration analysis
            // TODO: Implement when interface/impl system is ready
            arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_WARNING, decl->source_info,
                               "Implementation declaration analysis not yet implemented");
            return true;
        }
        case AST_DECL_MODULE: {
            // Module declaration analysis
            size_t name_len = decl->module_decl.name.length;
            if (name_len == 0 || !decl->module_decl.name.start) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, decl->source_info,
                                   "Module declaration missing name");
                return false;
            }

            char *name = arc_arena_alloc(analyzer->arena, name_len + 1);
            if (!name)
                return false;
            strncpy(name, decl->module_decl.name.start, name_len);
            name[name_len] = '\0';

            // Create module symbol
            ArcSymbol *symbol = arc_symbol_create(analyzer, ARC_SYMBOL_MODULE, name);
            if (!symbol)
                return false;

            symbol->declaration_node = decl;
            symbol->is_public = true;  // Modules are public by default
            symbol->is_defined = true;

            // For inline modules, create a new scope and analyze the body
            if (!decl->module_decl.is_external && decl->module_decl.body) {  // Create module scope
                ArcScope *module_scope =
                    arc_scope_create(analyzer, ARC_SCOPE_MODULE, analyzer->current_scope);
                if (!module_scope) {
                    return false;
                }
                module_scope->parent = analyzer->current_scope;

                // Store the module scope in the symbol for later reference
                symbol->scope = module_scope;

                // Enter module scope
                ArcScope *previous_scope = analyzer->current_scope;
                analyzer->current_scope = module_scope;

                // Analyze module body
                bool success = true;
                if (decl->module_decl.body->type == AST_STMT_BLOCK) {
                    for (size_t i = 0; i < decl->module_decl.body->block_stmt.statement_count;
                         i++) {
                        ArcAstNode *stmt = decl->module_decl.body->block_stmt.statements[i];
                        if (!arc_analyze_declaration(analyzer, stmt)) {
                            success = false;
                            // Continue analyzing other declarations for better error reporting
                        }
                    }
                }

                // Exit module scope
                analyzer->current_scope = previous_scope;

                if (!success) {
                    return false;
                }
            }

            return arc_scope_add_symbol(analyzer->current_scope, symbol);
        }
        case AST_DECL_USE: {
            // --- START: REPLACEMENT OF TEMPORARY FIX ---
            if (!decl->use_decl.path) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, decl->source_info,
                                   "Use declaration missing path");
                return false;
            }

            // For now, we only support simple `use <module_name>` paths.
            if (decl->use_decl.path->type != AST_IDENTIFIER) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, decl->source_info,
                                   "Complex module paths are not yet supported.");
                return false;
            }

            size_t name_len = decl->use_decl.path->identifier.token.length;
            char *module_name = arc_arena_alloc(analyzer->arena, name_len + 1);
            strncpy(module_name, decl->use_decl.path->identifier.token.start, name_len);
            module_name[name_len] = '\0';

            ArcModuleFile *module_file =
                arc_load_and_analyze_module(analyzer, module_name, decl->source_info);
            if (!module_file) {
                return false;  // Diagnostics are added in the load function
            }

            ArcScope *module_scope = module_file->global_scope;

            if (decl->use_decl.is_glob_import) {
                // `use module::*;`
                for (size_t i = 0; i < module_scope->symbol_capacity; i++) {
                    for (ArcSymbol *sym = module_scope->symbols[i]; sym; sym = sym->next) {
                        if (sym->is_public) {
                            if (arc_scope_lookup_symbol(analyzer->current_scope, sym->name)) {
                                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_NOTE, decl->source_info,
                                                   "Symbol '%s' from module '%s' conflicts with an "
                                                   "existing symbol and was not imported.",
                                                   sym->name, module_name);
                                continue;
                            }
                            // NOTE: This creates a shallow copy. A real compiler might
                            // need a deep copy or a more complex reference.
                            arc_scope_add_symbol(analyzer->current_scope, sym);
                        }
                    }
                }
            } else if (decl->use_decl.imports && decl->use_decl.import_count > 0) {
                // `use module::{item1, item2};`
                for (size_t i = 0; i < decl->use_decl.import_count; i++) {
                    ArcAstNode *import_node = decl->use_decl.imports[i];
                    // TODO: Handle aliasing `item as alias`
                    if (import_node->type != AST_IDENTIFIER) {
                        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_WARNING,
                                           import_node->source_info,
                                           "Import aliasing not yet implemented.");
                        continue;
                    }

                    size_t item_len = import_node->identifier.token.length;
                    char *item_name = arc_arena_alloc(analyzer->arena, item_len + 1);
                    strncpy(item_name, import_node->identifier.token.start, item_len);
                    item_name[item_len] = '\0';

                    ArcSymbol *imported_sym = arc_scope_lookup_symbol(module_scope, item_name);
                    if (!imported_sym) {
                        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, import_node->source_info,
                                           "Symbol '%s' not found in module '%s'.", item_name,
                                           module_name);
                        continue;
                    }
                    if (!imported_sym->is_public) {
                        arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, import_node->source_info,
                                           "Cannot import private symbol '%s' from module '%s'.",
                                           item_name, module_name);
                        continue;
                    }
                    if (arc_scope_lookup_symbol(analyzer->current_scope, item_name)) {
                        arc_diagnostic_add(
                            analyzer, ARC_DIAGNOSTIC_NOTE, import_node->source_info,
                            "Imported symbol '%s' conflicts with an existing symbol.", item_name);
                        continue;
                    }
                    arc_scope_add_symbol(analyzer->current_scope, imported_sym);
                }
            } else {
                // `use module;`
                if (arc_scope_lookup_symbol(analyzer->current_scope, module_name)) {
                    arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, decl->source_info,
                                       "Module name '%s' conflicts with an existing symbol.",
                                       module_name);
                    return false;
                }
                ArcSymbol *module_symbol =
                    arc_symbol_create(analyzer, ARC_SYMBOL_MODULE, module_name);
                module_symbol->scope = module_scope;  // Link to the module's global scope
                module_symbol->is_defined = true;
                arc_scope_add_symbol(analyzer->current_scope, module_symbol);
            }
            return true;
            // --- END: REPLACEMENT ---
        }

        // Handle variable/const statements that reached declaration analyzer
        case AST_STMT_VAR_DECL:
        case AST_STMT_CONST_DECL:
            return arc_analyze_statement(analyzer, decl);

        default:
            arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_WARNING, decl->source_info,
                               "Declaration type analysis not yet implemented (type: %d)",
                               decl->type);
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
            // NULL literals are compatible with any pointer or optional type.
            // We return a special 'void' type that assignment checks can handle.
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
            if (!symbol) {
                // This should have been caught by the initialization check, but as a safeguard:
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                                   "Undeclared identifier '%s'", name);
                return NULL;
            }

            return symbol->type;
        }
        case AST_LITERAL_STRING: {
            // For now, a string literal is treated as a pointer to a constant char.
            // In a more advanced system, this would be a `string` or `&const str` type.
            ArcTypeInfo *char_ptr_type = arc_type_create(analyzer, ARC_TYPE_POINTER);
            if (char_ptr_type) {
                char_ptr_type->pointer.pointed_type =
                    arc_type_get_builtin(analyzer, TOKEN_KEYWORD_CHAR);
                char_ptr_type->pointer.is_mutable = false;  // String literals are immutable
                char_ptr_type->is_resolved = true;
                char_ptr_type->size = sizeof(void *);
                char_ptr_type->alignment = sizeof(void *);
            }
            return char_ptr_type;
        }

        case AST_EXPR_BINARY: {
            // The scope resolution operator is special and must be handled before
            // analyzing left/right, as the left side is a module, not an expression.
            if (expr->binary_expr.op_type == BINARY_OP_SCOPE_RESOLUTION) {
                if (expr->binary_expr.left->type != AST_IDENTIFIER) {
                    arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR,
                                       expr->binary_expr.left->source_info,
                                       "Expected module name on the left side of '::'.");
                    return NULL;
                }

                ArcToken mod_token = expr->binary_expr.left->identifier.token;
                char *mod_name = arc_arena_alloc(analyzer->arena, mod_token.length + 1);
                strncpy(mod_name, mod_token.start, mod_token.length);
                mod_name[mod_token.length] = '\0';

                ArcSymbol *mod_sym =
                    arc_scope_lookup_symbol_recursive(analyzer->current_scope, mod_name);
                if (!mod_sym || mod_sym->kind != ARC_SYMBOL_MODULE) {
                    arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR,
                                       expr->binary_expr.left->source_info,
                                       "'%s' is not an imported module.", mod_name);
                    return NULL;
                }

                if (expr->binary_expr.right->type != AST_IDENTIFIER) {
                    arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR,
                                       expr->binary_expr.right->source_info,
                                       "Expected identifier on the right side of '::'.");
                    return NULL;
                }

                ArcToken item_token = expr->binary_expr.right->identifier.token;
                char *item_name = arc_arena_alloc(analyzer->arena, item_token.length + 1);
                strncpy(item_name, item_token.start, item_token.length);
                item_name[item_token.length] = '\0';

                ArcSymbol *target_sym = arc_scope_lookup_symbol(mod_sym->scope, item_name);

                if (!target_sym) {
                    arc_diagnostic_add(
                        analyzer, ARC_DIAGNOSTIC_ERROR, expr->binary_expr.right->source_info,
                        "Symbol '%s' not found in module '%s'.", item_name, mod_name);
                    return NULL;
                }

                // *** VISIBILITY CHECK ***
                if (!target_sym->is_public) {
                    arc_diagnostic_add(
                        analyzer, ARC_DIAGNOSTIC_ERROR, expr->binary_expr.right->source_info,
                        "Cannot access private symbol '%s' in module '%s'.", item_name, mod_name);
                    return NULL;
                }

                return target_sym->type;
            }

            // For all other binary operators, analyze left and right operands first.
            ArcTypeInfo *left_type = arc_analyze_expression_type(analyzer, expr->binary_expr.left);
            ArcTypeInfo *right_type =
                arc_analyze_expression_type(analyzer, expr->binary_expr.right);

            if (!left_type || !right_type) {
                return NULL;
            }

            switch (expr->binary_expr.op_type) {
                case BINARY_OP_ADD:
                case BINARY_OP_SUB:
                case BINARY_OP_MUL:
                case BINARY_OP_DIV:
                case BINARY_OP_MOD:
                    return arc_analyze_arithmetic_binary_op(analyzer, expr, left_type, right_type);

                case BINARY_OP_EQ:
                case BINARY_OP_NE:
                case BINARY_OP_LT:
                case BINARY_OP_LE:
                case BINARY_OP_GT:
                case BINARY_OP_GE:
                    return arc_analyze_comparison_binary_op(analyzer, expr, left_type, right_type);

                case BINARY_OP_AND:
                case BINARY_OP_OR:
                    return arc_analyze_logical_binary_op(analyzer, expr, left_type, right_type);

                case BINARY_OP_BIT_AND:
                case BINARY_OP_BIT_OR:
                case BINARY_OP_BIT_XOR:
                case BINARY_OP_BIT_SHL:
                case BINARY_OP_BIT_SHR:
                    return arc_analyze_bitwise_binary_op(analyzer, expr, left_type, right_type);

                case BINARY_OP_ASSIGN:
                case BINARY_OP_ADD_ASSIGN:
                case BINARY_OP_SUB_ASSIGN:
                case BINARY_OP_MUL_ASSIGN:
                case BINARY_OP_DIV_ASSIGN:
                case BINARY_OP_MOD_ASSIGN:
                    return arc_analyze_assignment_binary_op(analyzer, expr, left_type, right_type);
                case BINARY_OP_PIPELINE:
                case BINARY_OP_ASYNC_PIPELINE:
                case BINARY_OP_REVERSE_PIPELINE:
                case BINARY_OP_NULL_COALESCING:
                case BINARY_OP_SPACESHIP:
                    return arc_analyze_special_binary_op(analyzer, expr, left_type, right_type);

                default:
                    arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                                       "Unknown or unhandled binary operator");
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

        case AST_EXPR_INDEX: {
            ArcTypeInfo *object_type =
                arc_analyze_expression_type(analyzer, expr->index_expr.object);
            ArcTypeInfo *index_type = arc_analyze_expression_type(analyzer, expr->index_expr.index);

            if (!object_type || !index_type) {
                return NULL;
            }

            if (!arc_type_is_integer(index_type)) {
                arc_diagnostic_add(
                    analyzer, ARC_DIAGNOSTIC_ERROR, expr->index_expr.index->source_info,
                    "Array index must be an integer, but got '%s'", arc_type_to_string(index_type));
                return arc_type_create(analyzer, ARC_TYPE_ERROR);
            }

            switch (object_type->kind) {
                case ARC_TYPE_ARRAY:
                    return object_type->array.element_type;
                case ARC_TYPE_SLICE:
                    return object_type->slice.element_type;
                case ARC_TYPE_POINTER:
                    return object_type->pointer.pointed_type;
                default:
                    arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, expr->source_info,
                                       "Cannot index into type '%s'",
                                       arc_type_to_string(object_type));
                    return arc_type_create(analyzer, ARC_TYPE_ERROR);
            }
        }

        case AST_EXPR_FIELD_ACCESS: {
            ArcTypeInfo *object_type =
                arc_analyze_expression_type(analyzer, expr->field_access_expr.object);
            if (!object_type) {
                return NULL;
            }

            // TODO: Implement struct/union field access when those types are fully supported.
            arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_WARNING, expr->source_info,
                               "Field access not fully implemented - struct support pending");
            return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_VOID);
        }

        case AST_EXPR_CAST: {
            ArcTypeInfo *source_type =
                arc_analyze_expression_type(analyzer, expr->cast_expr.expression);
            ArcTypeInfo *target_type = arc_type_from_ast(analyzer, expr->cast_expr.target_type);

            if (!source_type || !target_type) {
                return NULL;
            }

            // TODO: Implement detailed cast validity checking (e.g., numeric to numeric,
            // pointer to int, etc.).
            arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_WARNING, expr->source_info,
                               "Cast validation not fully implemented");
            return target_type;
        }

        case AST_EXPR_PIPELINE: {
            ArcTypeInfo *left_type =
                arc_analyze_expression_type(analyzer, expr->pipeline_expr.left);
            ArcTypeInfo *right_type =
                arc_analyze_expression_type(analyzer, expr->pipeline_expr.right);

            if (!left_type || !right_type) {
                return NULL;
            }

            return arc_analyze_pipeline_binary_op(analyzer, expr, left_type, right_type);
        }
        case AST_EXPR_RANGE: {
            if (expr->range_expr.start) {
                arc_analyze_expression_type(analyzer, expr->range_expr.start);
            }
            if (expr->range_expr.end) {
                arc_analyze_expression_type(analyzer, expr->range_expr.end);
            }

            // TODO: Create a proper Range<T> type.
            arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_WARNING, expr->source_info,
                               "Range type analysis not fully implemented.");
            return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_VOID);
        }

        case AST_EXPR_ARRAY_LITERAL: {
            if (expr->array_literal_expr.element_count == 0) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_WARNING, expr->source_info,
                                   "Empty array literal requires type inference from context. This "
                                   "is not yet supported.");
                return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_VOID);
            }

            ArcTypeInfo *element_type =
                arc_analyze_expression_type(analyzer, expr->array_literal_expr.elements[0]);
            if (!element_type) {
                return NULL;
            }

            for (size_t i = 1; i < expr->array_literal_expr.element_count; i++) {
                ArcTypeInfo *current_element_type =
                    arc_analyze_expression_type(analyzer, expr->array_literal_expr.elements[i]);
                if (!current_element_type ||
                    !arc_type_is_assignable_to(current_element_type, element_type)) {
                    arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR,
                                       expr->array_literal_expr.elements[i]->source_info,
                                       "Array element has type '%s', but expected '%s' based on "
                                       "the first element.",
                                       arc_type_to_string(current_element_type),
                                       arc_type_to_string(element_type));
                    return arc_type_create(analyzer, ARC_TYPE_ERROR);
                }
            }

            return arc_type_create_array(analyzer, element_type,
                                         (long long)expr->array_literal_expr.element_count);
        }

        case AST_EXPR_STRUCT_LITERAL: {
            arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_WARNING, expr->source_info,
                               "Struct literal analysis not implemented - struct support pending");
            return arc_type_get_builtin(analyzer, TOKEN_KEYWORD_VOID);
        }

        default:
            // Fallback to the inference helper, though most cases should be covered above.
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
            }  // Set other symbol properties
            symbol->declaration_node = stmt;

            // Determine mutability based on declaration keyword
            if (stmt->var_decl_stmt.var_token.type == TOKEN_KEYWORD_LET) {
                symbol->is_mutable = false;  // let declarations are immutable
            } else if (stmt->var_decl_stmt.var_token.type == TOKEN_KEYWORD_MUT) {
                symbol->is_mutable = true;  // mut declarations are mutable
            } else {
                // Fallback for any other cases (shouldn't happen with current parser)
                symbol->is_mutable = false;
            }

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
            // Constant declaration analysis
            size_t name_len = stmt->const_decl_stmt.name.length;
            if (name_len == 0 || !stmt->const_decl_stmt.name.start) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                                   "Constant declaration missing name");
                return false;
            }

            char *name = arc_arena_alloc(analyzer->arena, name_len + 1);
            if (!name)
                return false;
            strncpy(name, stmt->const_decl_stmt.name.start, name_len);
            name[name_len] = '\0';

            // Check for duplicate declaration
            if (arc_scope_lookup_symbol(analyzer->current_scope, name)) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                                   "Constant '%s' already declared in this scope", name);
                return false;
            }

            // Constants must have initializers
            if (!stmt->const_decl_stmt.initializer) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                                   "Constant '%s' must have an initializer", name);
                return false;
            }

            // Create symbol
            ArcSymbol *symbol = arc_symbol_create(analyzer, ARC_SYMBOL_VARIABLE, name);
            if (!symbol)
                return false;

            // Analyze initializer
            ArcTypeInfo *init_type =
                arc_analyze_expression_type(analyzer, stmt->const_decl_stmt.initializer);
            if (!init_type) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                                   "Invalid initializer for constant '%s'", name);
                return false;
            }

            // Determine type
            if (stmt->const_decl_stmt.type_annotation) {
                symbol->type = arc_type_from_ast(analyzer, stmt->const_decl_stmt.type_annotation);
                if (!symbol->type)
                    return false;

                if (!arc_type_is_assignable_to(init_type, symbol->type)) {
                    arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                                       "Type mismatch in constant '%s' initialization", name);
                    return false;
                }
            } else {
                symbol->type = init_type;
            }

            symbol->declaration_node = stmt;
            symbol->is_mutable = false;  // Constants are immutable
            symbol->is_defined = true;

            return arc_scope_add_symbol(analyzer->current_scope, symbol);
        }

        case AST_STMT_ASSIGNMENT: {
            // Assignment statement analysis
            if (!arc_analyze_assignment_target(analyzer, stmt->assignment_stmt.target)) {
                return false;
            }

            ArcTypeInfo *target_type =
                arc_analyze_expression_type(analyzer, stmt->assignment_stmt.target);
            ArcTypeInfo *value_type =
                arc_analyze_expression_type(analyzer, stmt->assignment_stmt.value);

            if (!target_type || !value_type) {
                return false;
            }

            // Check type compatibility
            if (!arc_type_is_assignable_to(value_type, target_type)) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                                   "Cannot assign value of type '%s' to target of type '%s'",
                                   arc_type_to_string(value_type), arc_type_to_string(target_type));
                return false;
            }

            return true;
        }

        case AST_STMT_IF: {
            // If statement analysis
            ArcTypeInfo *condition_type =
                arc_analyze_expression_type(analyzer, stmt->if_stmt.condition);
            if (!condition_type) {
                return false;
            }

            // Condition must be boolean
            if (!arc_type_is_boolean(condition_type)) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                                   "If condition must be boolean, got '%s'",
                                   arc_type_to_string(condition_type));
                return false;
            }

            // Analyze then branch
            bool then_ok = arc_analyze_statement(analyzer, stmt->if_stmt.then_branch);

            // Analyze else branch if present
            bool else_ok = true;
            if (stmt->if_stmt.else_branch) {
                else_ok = arc_analyze_statement(analyzer, stmt->if_stmt.else_branch);
            }

            return then_ok && else_ok;
        }

        case AST_STMT_WHILE: {
            // While loop analysis
            ArcTypeInfo *condition_type =
                arc_analyze_expression_type(analyzer, stmt->while_stmt.condition);
            if (!condition_type) {
                return false;
            }

            // Condition must be boolean
            if (!arc_type_is_boolean(condition_type)) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                                   "While condition must be boolean, got '%s'",
                                   arc_type_to_string(condition_type));
                return false;
            }

            // Set loop context and analyze body
            bool prev_in_loop = analyzer->in_loop;
            analyzer->in_loop = true;
            bool body_ok = arc_analyze_statement(analyzer, stmt->while_stmt.body);
            analyzer->in_loop = prev_in_loop;

            return body_ok;
        }

        case AST_STMT_FOR: {
            // For loop analysis
            // TODO: Implement full for loop analysis when iterators are supported
            arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_WARNING, stmt->source_info,
                               "For loop analysis partially implemented");

            bool prev_in_loop = analyzer->in_loop;
            analyzer->in_loop = true;

            // Basic analysis - just check the body for now
            bool body_ok = true;
            if (stmt->for_stmt.body) {
                body_ok = arc_analyze_statement(analyzer, stmt->for_stmt.body);
            }

            analyzer->in_loop = prev_in_loop;
            return body_ok;
        }

        case AST_STMT_MATCH: {
            // Match statement analysis
            // TODO: Implement pattern matching analysis
            arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_WARNING, stmt->source_info,
                               "Match statement analysis not yet implemented");
            return true;
        }

        case AST_STMT_BREAK: {
            // Break statement analysis
            if (!analyzer->in_loop) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                                   "Break statement outside of loop");
                return false;
            }
            return true;
        }

        case AST_STMT_CONTINUE: {
            // Continue statement analysis
            if (!analyzer->in_loop) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                                   "Continue statement outside of loop");
                return false;
            }
            return true;
        }
        case AST_STMT_DEFER: {
            // Defer statement analysis
            // Defer statements execute at the end of the current scope
            // For now, just analyze the deferred statement
            return arc_analyze_statement(analyzer, stmt->defer_stmt.statement);
        }
        case AST_STMT_EXPRESSION: {
            // Expression statement analysis
            ArcTypeInfo *expr_type =
                arc_analyze_expression_type(analyzer, stmt->expr_stmt.expression);
            return expr_type != NULL;
        }

        case AST_STMT_RETURN: {
            // Return statement analysis
            if (!analyzer->current_function) {
                arc_diagnostic_add(analyzer, ARC_DIAGNOSTIC_ERROR, stmt->source_info,
                                   "Return statement outside of function");
                return false;
            }

            // Analyze return value if present
            if (stmt->return_stmt.value) {
                ArcTypeInfo *return_type =
                    arc_analyze_expression_type(analyzer, stmt->return_stmt.value);
                if (!return_type) {
                    return false;
                }

                // Check return type compatibility
                if (!arc_analyze_return_type_compatibility(analyzer, return_type,
                                                           stmt->source_info)) {
                    return false;
                }
            } else {
                // Void return - check if function expects void
                // TODO: Implement proper return type checking when function types are complete
            }

            analyzer->has_return = true;
            return true;
        }

        default:
            // For unimplemented statement types, just emit a warning and continue
            arc_diagnostic_add(
                analyzer, ARC_DIAGNOSTIC_WARNING, stmt->source_info,
                "Statement analysis not yet implemented for this statement type (type: %d)",
                stmt->type);
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
    }  // Get expected return type from current function
    ArcTypeInfo *expected_type = NULL;
    if (analyzer->current_function && analyzer->current_function->declaration_node) {
        ArcAstNode *return_type_node =
            analyzer->current_function->declaration_node->function_decl.return_type;
        if (return_type_node) {
            expected_type = arc_type_from_ast(analyzer, return_type_node);
        }
    }

    if (!expected_type || expected_type->kind == ARC_TYPE_VOID) {
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
                    return arc_type_get_builtin(analyzer,
                                                TOKEN_KEYWORD_BOOL);  // Arc's distinctive operators
                case BINARY_OP_PIPELINE:
                case BINARY_OP_ASYNC_PIPELINE:
                case BINARY_OP_REVERSE_PIPELINE:
                case BINARY_OP_NULL_COALESCING:
                case BINARY_OP_SPACESHIP:
                    return arc_analyze_special_binary_op_type(analyzer, expr, left_type,
                                                              right_type);

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
