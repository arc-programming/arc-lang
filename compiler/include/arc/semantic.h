// compiler/include/arc/semantic.h
#ifndef ARC_SEMANTIC_H
#define ARC_SEMANTIC_H

#include "arc/common.h"
#include "arc/parser.h"

// Forward declarations
typedef struct ArcSemanticAnalyzer ArcSemanticAnalyzer;
typedef struct ArcSymbol ArcSymbol;
typedef struct ArcScope ArcScope;
typedef struct ArcTypeInfo ArcTypeInfo;
typedef struct ArcModuleResolver ArcModuleResolver;

// === SYMBOL SYSTEM ===

typedef enum {
    ARC_SYMBOL_VARIABLE,
    ARC_SYMBOL_FUNCTION,
    ARC_SYMBOL_TYPE,
    ARC_SYMBOL_MODULE,
    ARC_SYMBOL_PARAMETER
} ArcSymbolKind;

typedef struct ArcSymbol {
    ArcSymbolKind kind;
    char *name;
    ArcTypeInfo *type;
    ArcAstNode *declaration_node;
    ArcScope *scope;
    bool is_mutable;
    bool is_public;
    bool is_defined;  // For function declarations vs definitions
    bool is_extern;   // For extern function declarations

    // For function symbols
    struct ArcSymbol **parameters;
    size_t parameter_count;

    struct ArcSymbol *next;  // For hash table chaining
} ArcSymbol;

// === SCOPE SYSTEM ===

typedef enum {
    ARC_SCOPE_GLOBAL,
    ARC_SCOPE_MODULE,
    ARC_SCOPE_FUNCTION,
    ARC_SCOPE_BLOCK
} ArcScopeKind;

typedef struct ArcScope {
    ArcScopeKind kind;
    struct ArcScope *parent;
    ArcSymbol **symbols;  // Hash table of symbols
    size_t symbol_capacity;
    size_t symbol_count;

    // For function scopes
    ArcSymbol *function_symbol;
    ArcTypeInfo *return_type;
} ArcScope;

// === TYPE SYSTEM ===

typedef enum {
    ARC_TYPE_PRIMITIVE,
    ARC_TYPE_POINTER,
    ARC_TYPE_ARRAY,
    ARC_TYPE_SLICE,
    ARC_TYPE_OPTIONAL,
    ARC_TYPE_FUNCTION,
    ARC_TYPE_VOID,
    ARC_TYPE_ERROR  // For error recovery
} ArcTypeKind;

typedef struct ArcTypeInfo {
    ArcTypeKind kind;

    union {
        // Primitive type
        struct {
            ArcTokenType primitive_type;
        } primitive;

        // Pointer type
        struct {
            struct ArcTypeInfo *pointed_type;
            bool is_mutable;  // ^ vs *const
        } pointer;

        // Array type
        struct {
            struct ArcTypeInfo *element_type;
            long long size;  // -1 for dynamic arrays
        } array;

        // Slice type
        struct {
            struct ArcTypeInfo *element_type;
        } slice;

        // Optional type
        struct {
            struct ArcTypeInfo *inner_type;
        } optional;

        // Function type
        struct {
            struct ArcTypeInfo **parameter_types;
            size_t parameter_count;
            struct ArcTypeInfo *return_type;
        } function;
    };

    // Type metadata
    bool is_resolved;
    size_t size;       // Size in bytes (0 if not calculated)
    size_t alignment;  // Alignment requirement
} ArcTypeInfo;

// === DIAGNOSTIC SYSTEM ===
// Note: ArcDiagnosticLevel and ArcDiagnostic are defined in parser.h

// === SEMANTIC ANALYZER ===

typedef struct ArcSemanticAnalyzer {
    // Scope management
    ArcScope *global_scope;
    ArcScope *current_scope;

    // Type system
    ArcTypeInfo **builtin_types;
    size_t builtin_type_count;

    // Diagnostics
    ArcDiagnostic *diagnostics;
    ArcDiagnostic *last_diagnostic;
    size_t error_count;
    size_t warning_count;
    ArcModuleResolver *module_resolver;

    // Memory management
    struct ArcArena *arena;

    // Current analysis context
    ArcSymbol *current_function;
    bool in_loop;
    bool has_return;
} ArcSemanticAnalyzer;

// === PUBLIC API ===

// Analyzer lifecycle
ArcSemanticAnalyzer *arc_semantic_analyzer_create_with_arena(ArcArena *arena);
ArcSemanticAnalyzer *arc_semantic_analyzer_create(void);
void arc_semantic_analyzer_destroy(ArcSemanticAnalyzer *analyzer);

// Main analysis entry point
bool arc_semantic_analyze(ArcSemanticAnalyzer *analyzer, ArcAstNode *ast);

// Scope management
ArcScope *arc_scope_create(ArcSemanticAnalyzer *analyzer, ArcScopeKind kind, ArcScope *parent);
void arc_scope_push(ArcSemanticAnalyzer *analyzer, ArcScope *scope);
ArcScope *arc_scope_pop(ArcSemanticAnalyzer *analyzer);

// Symbol management
ArcSymbol *arc_symbol_create(ArcSemanticAnalyzer *analyzer, ArcSymbolKind kind, const char *name);
bool arc_scope_add_symbol(ArcScope *scope, ArcSymbol *symbol);
ArcSymbol *arc_scope_lookup_symbol(ArcScope *scope, const char *name);
ArcSymbol *arc_scope_lookup_symbol_recursive(ArcScope *scope, const char *name);

// Type system
ArcTypeInfo *arc_type_create(ArcSemanticAnalyzer *analyzer, ArcTypeKind kind);
ArcTypeInfo *arc_type_from_ast(ArcSemanticAnalyzer *analyzer, ArcAstNode *type_node);
bool arc_type_is_compatible(ArcTypeInfo *left, ArcTypeInfo *right);
bool arc_type_is_assignable_to(ArcTypeInfo *from, ArcTypeInfo *to);
ArcTypeInfo *arc_type_get_builtin(ArcSemanticAnalyzer *analyzer, ArcTokenType primitive_type);

// Diagnostics
void arc_diagnostic_add(ArcSemanticAnalyzer *analyzer, ArcDiagnosticLevel level,
                        ArcSourceInfo source_info, const char *format, ...);
void arc_diagnostic_print_all(ArcSemanticAnalyzer *analyzer);
bool arc_semantic_has_errors(ArcSemanticAnalyzer *analyzer);

// Analysis helpers
ArcTypeInfo *arc_analyze_expression_type(ArcSemanticAnalyzer *analyzer, ArcAstNode *expr);
bool arc_analyze_declaration(ArcSemanticAnalyzer *analyzer, ArcAstNode *decl);
bool arc_analyze_statement(ArcSemanticAnalyzer *analyzer, ArcAstNode *stmt);

void arc_module_resolver_init(void);
void arc_module_resolver_cleanup(void);

#endif  // ARC_SEMANTIC_H
