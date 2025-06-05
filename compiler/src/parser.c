#include "arc/parser.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Default arena block size (1MB)
#define DEFAULT_ARENA_BLOCK_SIZE (1024 * 1024)

// Initial capacity for node tracking array
#define INITIAL_NODE_CAPACITY 256

// Forward declarations for parsing functions
static ArcAstNode *parse_declaration(ArcParser *parser);
static ArcAstNode *parse_extern_declaration(ArcParser *parser);
static ArcAstNode *parse_statement(ArcParser *parser);
static ArcAstNode *parse_expression(ArcParser *parser);
static ArcAstNode *parse_primary(ArcParser *parser);
static ArcAstNode *parse_type(ArcParser *parser);
static ArcAstNode *parse_type_postfix(ArcParser *parser);
static ArcAstNode *parse_variable_declaration(ArcParser *parser);
static ArcAstNode *parse_module_declaration(ArcParser *parser);
static ArcAstNode *parse_use_declaration(ArcParser *parser);
static ArcAstNode *parse_module_path(ArcParser *parser);
static ArcAstNode *parse_scope_resolution(ArcParser *parser, ArcAstNode *left);
static ArcAstNode *parse_match_statement(ArcParser *parser);
static ArcAstNode *parse_defer_statement(ArcParser *parser);
static ArcAstNode *parse_scope_resolution(ArcParser *parser, ArcAstNode *left);

// Utility functions
static void advance(ArcParser *parser);
static bool check(const ArcParser *parser, ArcTokenType type);
static bool match(ArcParser *parser, ArcTokenType type);
static ArcToken consume(ArcParser *parser, ArcTokenType type, const char *message);
static void consume_statement_terminator(ArcParser *parser);
static bool is_statement_terminator(const ArcParser *parser);
// static void skip_to_sync_point(ArcParser *parser);

// === ARENA ALLOCATOR IMPLEMENTATION ===

ArcArena *arc_arena_create(size_t default_block_size) {
    if (default_block_size == 0) {
        default_block_size = DEFAULT_ARENA_BLOCK_SIZE;
    }

    ArcArena *arena = malloc(sizeof(ArcArena));
    if (!arena)
        return NULL;

    arena->default_block_size = default_block_size;
    arena->total_allocated = 0;
    arena->first_block = NULL;
    arena->current_block = NULL;

    return arena;
}

void arc_arena_destroy(ArcArena *arena) {
    if (!arena)
        return;

    ArcArenaBlock *block = arena->first_block;
    while (block) {
        ArcArenaBlock *next = block->next;
        free(block->memory);
        free(block);
        block = next;
    }

    free(arena);
}

static ArcArenaBlock *create_arena_block(size_t size) {
    ArcArenaBlock *block = malloc(sizeof(ArcArenaBlock));
    if (!block)
        return NULL;

    block->memory = malloc(size);
    if (!block->memory) {
        free(block);
        return NULL;
    }

    block->size = size;
    block->used = 0;
    block->next = NULL;

    return block;
}

void *arc_arena_alloc(ArcArena *arena, size_t size) {
    if (!arena || size == 0)
        return NULL;

    // Align size to 8-byte boundary
    size = (size + 7) & ~7;

    // Check if current block has enough space
    if (arena->current_block && arena->current_block->used + size <= arena->current_block->size) {
        void *ptr = (char *)arena->current_block->memory + arena->current_block->used;
        arena->current_block->used += size;
        return ptr;
    }

    // Need a new block
    size_t block_size = size > arena->default_block_size ? size : arena->default_block_size;
    ArcArenaBlock *new_block = create_arena_block(block_size);
    if (!new_block)
        return NULL;

    // Link the new block
    if (arena->current_block) {
        arena->current_block->next = new_block;
    } else {
        arena->first_block = new_block;
    }
    arena->current_block = new_block;
    arena->total_allocated += block_size;

    // Allocate from the new block
    void *ptr = new_block->memory;
    new_block->used = size;
    return ptr;
}

void arc_arena_reset(ArcArena *arena) {
    if (!arena)
        return;

    // Keep first block, free others
    if (arena->first_block) {
        ArcArenaBlock *block = arena->first_block->next;
        while (block) {
            ArcArenaBlock *next = block->next;
            free(block->memory);
            free(block);
            arena->total_allocated -= block->size;
            block = next;
        }

        arena->first_block->next = NULL;
        arena->first_block->used = 0;
        arena->current_block = arena->first_block;
    }
}

size_t arc_arena_total_allocated(const ArcArena *arena) {
    return arena ? arena->total_allocated : 0;
}

// === PARSER IMPLEMENTATION ===

void arc_parser_init(ArcParser *parser, ArcLexer *lexer, ArcArena *arena) {
    parser->lexer = lexer;
    parser->ast_arena = arena;
    parser->use_arena = (arena != NULL);

    // Initialize tracking array for non-arena mode
    if (!parser->use_arena) {
        parser->allocated_nodes = malloc(sizeof(ArcAstNode *) * INITIAL_NODE_CAPACITY);
        parser->allocated_capacity = parser->allocated_nodes ? INITIAL_NODE_CAPACITY : 0;
    } else {
        parser->allocated_nodes = NULL;
        parser->allocated_capacity = 0;
    }
    parser->allocated_count = 0;

    // Initialize error state
    parser->had_error = false;
    parser->panic_mode = false;
    parser->synchronizing = false;
    parser->max_errors = 0;  // Unlimited by default

    // Initialize diagnostics
    parser->diagnostics = NULL;
    parser->last_diagnostic = NULL;
    parser->diagnostic_count = 0;

    // Get first token
    parser->current_token = arc_lexer_next_token(lexer);
    parser->previous_token = parser->current_token;  // Initialize previous
}

void arc_parser_init_simple(ArcParser *parser, ArcLexer *lexer) {
    arc_parser_init(parser, lexer, NULL);
}

void arc_parser_cleanup(ArcParser *parser) {
    if (!parser)
        return;

    // Clean up AST nodes
    if (parser->use_arena) {
        // Arena cleanup - just reset the arena
        if (parser->ast_arena) {
            arc_arena_reset(parser->ast_arena);
        }
    } else {
        // Traditional cleanup - destroy each node
        for (size_t i = 0; i < parser->allocated_count; i++) {
            arc_ast_node_destroy(parser->allocated_nodes[i]);
        }
        free(parser->allocated_nodes);
    }

    // Clean up diagnostics
    arc_parser_clear_diagnostics(parser);

    // Reset state
    parser->allocated_nodes = NULL;
    parser->allocated_count = 0;
    parser->allocated_capacity = 0;
    parser->had_error = false;
    parser->panic_mode = false;
    parser->synchronizing = false;
}

// === DIAGNOSTIC FUNCTIONS ===

static ArcDiagnostic *create_diagnostic(ArcDiagnosticLevel level, ArcSourceInfo source_info,
                                        const char *message) {
    ArcDiagnostic *diag = malloc(sizeof(ArcDiagnostic));
    if (!diag)
        return NULL;

    diag->level = level;
    diag->source_info = source_info;
    diag->message = arc_strdup(message);
    diag->next = NULL;

    return diag;
}

void arc_parser_add_diagnostic(ArcParser *parser, ArcDiagnosticLevel level,
                               ArcSourceInfo source_info, const char *format, ...) {
    va_list args;
    va_start(args, format);  // Create a formatted message using portable vsnprintf
    char *message = NULL;
    va_list args_copy;
    va_copy(args_copy, args);

    // First call to get required size
    int size = vsnprintf(NULL, 0, format, args);
    if (size < 0) {
        va_end(args);
        va_end(args_copy);
        return;  // Failed to format message
    }

    // Allocate buffer and format message
    message = malloc(size + 1);
    if (!message) {
        va_end(args);
        va_end(args_copy);
        return;  // Out of memory
    }

    vsnprintf(message, size + 1, format, args_copy);
    va_end(args);
    va_end(args_copy);

    ArcDiagnostic *diag = create_diagnostic(level, source_info, message);
    free(message);

    if (!diag)
        return;

    // Add to linked list
    if (parser->last_diagnostic) {
        parser->last_diagnostic->next = diag;
    } else {
        parser->diagnostics = diag;
    }
    parser->last_diagnostic = diag;
    parser->diagnostic_count++;

    if (level == ARC_DIAGNOSTIC_ERROR) {
        parser->had_error = true;
    }
}

void arc_parser_error(ArcParser *parser, const char *format, ...) {
    if (parser->panic_mode)
        return;  // Suppress cascading errors

    va_list args;
    va_start(args, format);

    char *message = arc_string_vformat(format, args);
    va_end(args);

    if (!message)
        return;

    ArcSourceInfo source_info = arc_parser_current_source_info(parser);
    arc_parser_add_diagnostic(parser, ARC_DIAGNOSTIC_ERROR, source_info, "%s", message);
    free(message);

    parser->panic_mode = true;
}

void arc_parser_error_at(ArcParser *parser, ArcSourceInfo source_info, const char *format, ...) {
    if (parser->panic_mode)
        return;

    va_list args;
    va_start(args, format);

    char *message = arc_string_vformat(format, args);
    va_end(args);

    if (!message)
        return;

    arc_parser_add_diagnostic(parser, ARC_DIAGNOSTIC_ERROR, source_info, "%s", message);
    free(message);

    parser->panic_mode = true;
}

void arc_parser_warning(ArcParser *parser, ArcSourceInfo source_info, const char *format, ...) {
    va_list args;
    va_start(args, format);

    char *message = arc_string_vformat(format, args);
    va_end(args);

    if (!message)
        return;

    arc_parser_add_diagnostic(parser, ARC_DIAGNOSTIC_WARNING, source_info, "%s", message);
    free(message);
}

bool arc_parser_had_error(const ArcParser *parser) {
    return parser->had_error;
}

size_t arc_parser_diagnostic_count(const ArcParser *parser) {
    return parser->diagnostic_count;
}

const ArcDiagnostic *arc_parser_get_diagnostics(const ArcParser *parser) {
    return parser->diagnostics;
}

void arc_parser_reset_error(ArcParser *parser) {
    parser->had_error = false;
    parser->panic_mode = false;
    parser->synchronizing = false;
}

void arc_parser_clear_diagnostics(ArcParser *parser) {
    ArcDiagnostic *current = parser->diagnostics;
    while (current) {
        ArcDiagnostic *next = current->next;
        free(current->message);
        free(current);
        current = next;
    }

    parser->diagnostics = NULL;
    parser->last_diagnostic = NULL;
    parser->diagnostic_count = 0;
    arc_parser_reset_error(parser);
}

ArcSourceInfo arc_parser_current_source_info(const ArcParser *parser) {
    return arc_source_info_from_token(&parser->current_token, NULL);
}

ArcSourceInfo arc_source_info_from_token(const ArcToken *token, const char *filename) {
    ArcSourceInfo info;
    info.location = token->loc;
    info.filename = filename;
    info.source_text = token->start;
    return info;
}

// === AST NODE FUNCTIONS ===

ArcAstNode *arc_ast_node_create(ArcParser *parser, ArcAstNodeType type, ArcSourceInfo source_info) {
    ArcAstNode *node;

    if (parser->use_arena) {
        node = arc_arena_alloc(parser->ast_arena, sizeof(ArcAstNode));
    } else {
        node = malloc(sizeof(ArcAstNode));
    }

    if (!node)
        return NULL;

    // Initialize node
    memset(node, 0, sizeof(ArcAstNode));
    node->type = type;
    node->source_info = source_info;

    // Track node for traditional allocation
    if (!parser->use_arena) {
        if (parser->allocated_count >= parser->allocated_capacity) {
            size_t new_capacity = parser->allocated_capacity * 2;
            if (new_capacity == 0)
                new_capacity = INITIAL_NODE_CAPACITY;

            ArcAstNode **new_array =
                realloc(parser->allocated_nodes, sizeof(ArcAstNode *) * new_capacity);
            if (!new_array) {
                free(node);
                return NULL;
            }

            parser->allocated_nodes = new_array;
            parser->allocated_capacity = new_capacity;
        }

        parser->allocated_nodes[parser->allocated_count++] = node;
    }

    return node;
}

ArcAstNode *arc_ast_node_create_tracked(ArcParser *parser, ArcAstNodeType type,
                                        ArcSourceLocation location) {
    ArcSourceInfo source_info;
    source_info.location = location;
    source_info.filename = NULL;
    source_info.source_text = NULL;

    return arc_ast_node_create(parser, type, source_info);
}

// Helper to allocate arrays from arena or heap
static void **alloc_array(ArcParser *parser, size_t count, size_t element_size) {
    if (count == 0)
        return NULL;

    size_t total_size = count * element_size;

    if (parser->use_arena) {
        return arc_arena_alloc(parser->ast_arena, total_size);
    } else {
        return malloc(total_size);
    }
}

// Helper to duplicate string in arena or heap
static char *dup_string(ArcParser *parser, const char *str, size_t length) {
    if (!str)
        return NULL;

    char *result;
    if (parser->use_arena) {
        result = arc_arena_alloc(parser->ast_arena, length + 1);
    } else {
        result = malloc(length + 1);
    }

    if (result) {
        memcpy(result, str, length);
        result[length] = '\0';
    }

    return result;
}

void arc_ast_node_destroy(ArcAstNode *node) {
    if (!node)
        return;

    // Note: This function is only called in non-arena mode
    // In arena mode, all memory is freed when arena is destroyed

    switch (node->type) {
        case AST_LITERAL_STRING:
            free(node->literal_string.value);
            break;

        case AST_TYPE_FUNCTION:
            free(node->type_function.parameter_types);
            if (node->type_function.return_type) {
                arc_ast_node_destroy(node->type_function.return_type);
            }
            for (size_t i = 0; i < node->type_function.parameter_count; i++) {
                arc_ast_node_destroy(node->type_function.parameter_types[i]);
            }
            break;

        case AST_EXPR_CALL:
            arc_ast_node_destroy(node->call_expr.function);
            for (size_t i = 0; i < node->call_expr.argument_count; i++) {
                arc_ast_node_destroy(node->call_expr.arguments[i]);
            }
            free(node->call_expr.arguments);
            break;

        case AST_EXPR_ARRAY_LITERAL:
            for (size_t i = 0; i < node->array_literal_expr.element_count; i++) {
                arc_ast_node_destroy(node->array_literal_expr.elements[i]);
            }
            free(node->array_literal_expr.elements);
            break;

        case AST_STMT_BLOCK:
            for (size_t i = 0; i < node->block_stmt.statement_count; i++) {
                arc_ast_node_destroy(node->block_stmt.statements[i]);
            }
            free(node->block_stmt.statements);
            break;

        case AST_DECL_FUNCTION:
            arc_ast_node_destroy(node->function_decl.body);
            if (node->function_decl.return_type) {
                arc_ast_node_destroy(node->function_decl.return_type);
            }
            for (size_t i = 0; i < node->function_decl.parameter_count; i++) {
                arc_ast_node_destroy(node->function_decl.parameters[i]);
            }
            free(node->function_decl.parameters);
            break;

        case AST_PROGRAM:
            for (size_t i = 0; i < node->program.declaration_count; i++) {
                arc_ast_node_destroy(node->program.declarations[i]);
            }
            free(node->program.declarations);
            break;

        // Add more cases as needed for other node types with dynamic children
        default:
            // Many node types only have simple fields or single child nodes
            // Handle single child nodes generically
            break;
    }

    free(node);
}

// === PARSING UTILITY FUNCTIONS ===

static void advance(ArcParser *parser) {
    parser->previous_token = parser->current_token;

    for (;;) {
        parser->current_token = arc_lexer_next_token(parser->lexer);

        if (parser->current_token.type != TOKEN_ERROR)
            break;

        // Handle lexer errors
        ArcSourceInfo source_info = arc_source_info_from_token(&parser->current_token, NULL);
        arc_parser_error_at(parser, source_info, "Lexer error: %.*s",
                            (int)parser->current_token.length, parser->current_token.start);
    }
}

static bool check(const ArcParser *parser, ArcTokenType type) {
    return parser->current_token.type == type;
}

static bool match(ArcParser *parser, ArcTokenType type) {
    if (!check(parser, type))
        return false;
    advance(parser);
    return true;
}

static ArcToken consume(ArcParser *parser, ArcTokenType type, const char *message) {
    if (parser->current_token.type == type) {
        ArcToken token = parser->current_token;
        advance(parser);
        return token;
    }

    arc_parser_error(parser, "%s", message);
    return parser->current_token;  // Return current token even on error
}

void arc_parser_synchronize(ArcParser *parser) {
    parser->panic_mode = false;
    parser->synchronizing = true;

    // Always advance at least once to avoid infinite loops
    if (parser->current_token.type != TOKEN_EOF) {
        advance(parser);
    }
    while (parser->current_token.type != TOKEN_EOF) {
        if (parser->previous_token.type == TOKEN_SEMICOLON ||
            parser->previous_token.type == TOKEN_NEWLINE) {
            parser->synchronizing = false;
            return;
        }
        switch (parser->current_token.type) {
            case TOKEN_KEYWORD_FUNC:
            case TOKEN_KEYWORD_STRUCT:
            case TOKEN_KEYWORD_ENUM:
            case TOKEN_KEYWORD_TYPE:
            case TOKEN_KEYWORD_INTERFACE:
            case TOKEN_KEYWORD_IMPL:
            case TOKEN_KEYWORD_LET:
            case TOKEN_KEYWORD_MUT:
            case TOKEN_KEYWORD_CONST:
            case TOKEN_KEYWORD_IF:
            case TOKEN_KEYWORD_WHILE:
            case TOKEN_KEYWORD_FOR:
            case TOKEN_KEYWORD_MATCH:
            case TOKEN_KEYWORD_RETURN:
            case TOKEN_KEYWORD_BREAK:
            case TOKEN_KEYWORD_CONTINUE:
                parser->synchronizing = false;
                return;
            default:
                break;
        }

        advance(parser);
    }

    parser->synchronizing = false;
}

// Helper functions for semicolon-less statement termination
static bool is_statement_terminator(const ArcParser *parser) {
    // Statement can be terminated by:
    // 1. Newline
    // 2. End of block (})
    // 3. End of file
    // 4. Semicolon (still supported for compatibility)
    return check(parser, TOKEN_NEWLINE) || check(parser, TOKEN_RBRACE) ||
           check(parser, TOKEN_EOF) || check(parser, TOKEN_SEMICOLON);
}

static void consume_statement_terminator(ArcParser *parser) {
    // For semicolon-less syntax, we accept newlines, end of block, or semicolons
    if (match(parser, TOKEN_SEMICOLON)) {
        // Still support semicolons for compatibility
        return;
    } else if (match(parser, TOKEN_NEWLINE)) {
        // Consume any additional newlines
        while (match(parser, TOKEN_NEWLINE)) {
            // Continue consuming newlines
        }
        return;
    } else if (check(parser, TOKEN_RBRACE) || check(parser, TOKEN_EOF)) {
        // End of block or file - don't consume these tokens
        return;
    }
    // If we reach here and it's not a valid terminator, it's an error
    // but we'll let the calling code handle it
}

// === PARSING FUNCTIONS ===

// Parse binary operator precedence
typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,       // =, +=, -=, etc., :=
    PREC_PIPELINE,         // |>, ~>, <|
    PREC_NULL_COALESCING,  // ??
    PREC_OR,               // ||, or
    PREC_AND,              // &&, and
    PREC_EQUALITY,         // ==, !=
    PREC_COMPARISON,       // <, >, <=, >=, <=>
    PREC_BITWISE_OR,       // |
    PREC_BITWISE_XOR,      // ^
    PREC_BITWISE_AND,      // &
    PREC_SHIFT,            // <<, >>
    PREC_TERM,             // +, -
    PREC_FACTOR,           // *, /, %
    PREC_POWER,            // **
    PREC_UNARY,            // !, -, &, *, not, !!
    PREC_CALL,             // (), [], ., ::
    PREC_PRIMARY
} Precedence;

static Precedence get_token_precedence(ArcTokenType type) {
    switch (type) {
        case TOKEN_EQUAL:
        case TOKEN_WALRUS:  // :=
        case TOKEN_PLUS_EQUAL:
        case TOKEN_MINUS_EQUAL:
        case TOKEN_ASTERISK_EQUAL:
        case TOKEN_SLASH_EQUAL:
        case TOKEN_PERCENT_EQUAL:
        case TOKEN_POWER_EQUAL:
        case TOKEN_AMPERSAND_EQUAL:
        case TOKEN_PIPE_EQUAL:
        case TOKEN_CARET_EQUAL:
        case TOKEN_LEFT_SHIFT_EQUAL:
        case TOKEN_RIGHT_SHIFT_EQUAL:
            return PREC_ASSIGNMENT;
        case TOKEN_PIPELINE:          // |>
        case TOKEN_ASYNC_PIPELINE:    // ~>
        case TOKEN_REVERSE_PIPELINE:  // <|
            return PREC_PIPELINE;
        case TOKEN_NULL_COALESCING:  // ??
            return PREC_NULL_COALESCING;
        case TOKEN_PIPE_PIPE:
        case TOKEN_KEYWORD_OR:
            return PREC_OR;
        case TOKEN_AMPERSAND_AMPERSAND:
        case TOKEN_KEYWORD_AND:
            return PREC_AND;
        case TOKEN_EQUAL_EQUAL:
        case TOKEN_BANG_EQUAL:
            return PREC_EQUALITY;
        case TOKEN_LESS:
        case TOKEN_LESS_EQUAL:
        case TOKEN_GREATER:
        case TOKEN_GREATER_EQUAL:
        case TOKEN_SPACESHIP:  // <=>
            return PREC_COMPARISON;
        case TOKEN_PIPE:
            return PREC_BITWISE_OR;
        case TOKEN_CARET:
            return PREC_BITWISE_XOR;
        case TOKEN_AMPERSAND:
            return PREC_BITWISE_AND;
        case TOKEN_LEFT_SHIFT:
        case TOKEN_RIGHT_SHIFT:
            return PREC_SHIFT;
        case TOKEN_PLUS:
        case TOKEN_MINUS:
            return PREC_TERM;
        case TOKEN_ASTERISK:
        case TOKEN_SLASH:
        case TOKEN_PERCENT:
            return PREC_FACTOR;
        case TOKEN_POWER:  // **
            return PREC_POWER;
        case TOKEN_LPAREN:
        case TOKEN_LBRACKET:
        case TOKEN_DOT:
        case TOKEN_DOUBLE_COLON:  // ::
            return PREC_CALL;
        default:
            return PREC_NONE;
    }
}

static ArcBinaryOperator token_to_binary_op(ArcTokenType type) {
    switch (type) {
        case TOKEN_PLUS:
            return BINARY_OP_ADD;
        case TOKEN_MINUS:
            return BINARY_OP_SUB;
        case TOKEN_ASTERISK:
            return BINARY_OP_MUL;
        case TOKEN_SLASH:
            return BINARY_OP_DIV;
        case TOKEN_PERCENT:
            return BINARY_OP_MOD;
        case TOKEN_POWER:
            return BINARY_OP_POW;
        case TOKEN_EQUAL_EQUAL:
            return BINARY_OP_EQ;
        case TOKEN_BANG_EQUAL:
            return BINARY_OP_NE;
        case TOKEN_LESS:
            return BINARY_OP_LT;
        case TOKEN_LESS_EQUAL:
            return BINARY_OP_LE;
        case TOKEN_GREATER:
            return BINARY_OP_GT;
        case TOKEN_GREATER_EQUAL:
            return BINARY_OP_GE;
        case TOKEN_SPACESHIP:
            return BINARY_OP_SPACESHIP;
        case TOKEN_AMPERSAND_AMPERSAND:
        case TOKEN_KEYWORD_AND:
            return BINARY_OP_AND;
        case TOKEN_PIPE_PIPE:
        case TOKEN_KEYWORD_OR:
            return BINARY_OP_OR;
        case TOKEN_AMPERSAND:
            return BINARY_OP_BIT_AND;
        case TOKEN_PIPE:
            return BINARY_OP_BIT_OR;
        case TOKEN_CARET:
            return BINARY_OP_BIT_XOR;
        case TOKEN_LEFT_SHIFT:
            return BINARY_OP_BIT_SHL;
        case TOKEN_RIGHT_SHIFT:
            return BINARY_OP_BIT_SHR;
        case TOKEN_PIPELINE:
            return BINARY_OP_PIPELINE;
        case TOKEN_ASYNC_PIPELINE:
            return BINARY_OP_ASYNC_PIPELINE;
        case TOKEN_REVERSE_PIPELINE:
            return BINARY_OP_REVERSE_PIPELINE;
        case TOKEN_NULL_COALESCING:
            return BINARY_OP_NULL_COALESCING;
        case TOKEN_DOUBLE_COLON:
            return BINARY_OP_SCOPE_RESOLUTION;
        case TOKEN_EQUAL:
            return BINARY_OP_ASSIGN;
        case TOKEN_WALRUS:
            return BINARY_OP_WALRUS;
        case TOKEN_PLUS_EQUAL:
            return BINARY_OP_ADD_ASSIGN;
        case TOKEN_MINUS_EQUAL:
            return BINARY_OP_SUB_ASSIGN;
        case TOKEN_ASTERISK_EQUAL:
            return BINARY_OP_MUL_ASSIGN;
        case TOKEN_SLASH_EQUAL:
            return BINARY_OP_DIV_ASSIGN;
        case TOKEN_PERCENT_EQUAL:
            return BINARY_OP_MOD_ASSIGN;
        case TOKEN_POWER_EQUAL:
            return BINARY_OP_POW_ASSIGN;
        case TOKEN_AMPERSAND_EQUAL:
            return BINARY_OP_AND_ASSIGN;
        case TOKEN_PIPE_EQUAL:
            return BINARY_OP_OR_ASSIGN;
        case TOKEN_CARET_EQUAL:
            return BINARY_OP_XOR_ASSIGN;
        case TOKEN_LEFT_SHIFT_EQUAL:
            return BINARY_OP_SHL_ASSIGN;
        case TOKEN_RIGHT_SHIFT_EQUAL:
            return BINARY_OP_SHR_ASSIGN;
        default:
            return BINARY_OP_ADD;  // Fallback - should not happen
    }
}

static ArcUnaryOperator token_to_unary_op(ArcTokenType type) {
    switch (type) {
        case TOKEN_BANG:
            return UNARY_OP_NOT;
        case TOKEN_MINUS:
            return UNARY_OP_NEGATE;
        case TOKEN_AMPERSAND:
            return UNARY_OP_ADDRESS;
        case TOKEN_ASTERISK:
            return UNARY_OP_DEREFERENCE;
        case TOKEN_KEYWORD_NOT:
            return UNARY_OP_LOGICAL_NOT;
        case TOKEN_FORCE_UNWRAP:
            return UNARY_OP_FORCE_UNWRAP;
        default:
            return UNARY_OP_NOT;  // Fallback - should not happen
    }
}

// Forward declaration for expression parsing
static ArcAstNode *parse_precedence(ArcParser *parser, Precedence precedence);

static ArcAstNode *parse_primary(ArcParser *parser) {
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);

    switch (parser->current_token.type) {
        case TOKEN_NUMBER_INT: {
            ArcToken token = parser->current_token;
            advance(parser);

            ArcAstNode *node = arc_ast_node_create(parser, AST_LITERAL_INT, source_info);
            if (node) {
                node->literal_int.value = token.value.int_val;
                node->literal_int.token = token;
            }
            return node;
        }

        case TOKEN_NUMBER_FLOAT: {
            ArcToken token = parser->current_token;
            advance(parser);

            ArcAstNode *node = arc_ast_node_create(parser, AST_LITERAL_FLOAT, source_info);
            if (node) {
                node->literal_float.value = token.value.float_val;
                node->literal_float.token = token;
            }
            return node;
        }

        case TOKEN_STRING_LITERAL: {
            ArcToken token = parser->current_token;
            advance(parser);

            ArcAstNode *node = arc_ast_node_create(parser, AST_LITERAL_STRING, source_info);
            if (node) {
                // Duplicate the string content
                node->literal_string.value = dup_string(parser, token.start + 1, token.length - 2);
                node->literal_string.length = token.length - 2;
                node->literal_string.token = token;
            }
            return node;
        }

        case TOKEN_CHAR_LITERAL: {
            ArcToken token = parser->current_token;
            advance(parser);

            ArcAstNode *node = arc_ast_node_create(parser, AST_LITERAL_CHAR, source_info);
            if (node) {
                node->literal_char.codepoint = token.value.char_val;
                node->literal_char.token = token;
            }
            return node;
        }

        case TOKEN_KEYWORD_TRUE:
        case TOKEN_KEYWORD_FALSE: {
            ArcToken token = parser->current_token;
            advance(parser);

            ArcAstNode *node = arc_ast_node_create(parser, AST_LITERAL_BOOL, source_info);
            if (node) {
                node->literal_bool.value = (token.type == TOKEN_KEYWORD_TRUE);
                node->literal_bool.token = token;
            }
            return node;
        }
        case TOKEN_KEYWORD_NIL: {
            ArcToken token = parser->current_token;
            advance(parser);

            ArcAstNode *node = arc_ast_node_create(parser, AST_LITERAL_NULL, source_info);
            if (node) {
                node->literal_null.token = token;
            }
            return node;
        }

        case TOKEN_IDENTIFIER: {
            ArcToken token = parser->current_token;
            advance(parser);

            ArcAstNode *node = arc_ast_node_create(parser, AST_IDENTIFIER, source_info);
            if (node) {
                node->identifier.token = token;
            }
            return node;
        }
        case TOKEN_LPAREN: {
            advance(parser);  // consume '('
            ArcAstNode *expr = parse_expression(parser);
            consume(parser, TOKEN_RPAREN, "Expected ')' after expression");
            return expr;
        }

        case TOKEN_LBRACKET: {
            // Array literal: [expr, expr, ...]
            ArcToken lbracket_token = parser->current_token;
            advance(parser);  // consume '['

            ArcAstNode *node = arc_ast_node_create(parser, AST_EXPR_ARRAY_LITERAL, source_info);
            if (!node)
                return NULL;

            node->array_literal_expr.lbracket_token = lbracket_token;

            // Parse elements using simple realloc approach
            ArcAstNode **elements = NULL;
            size_t element_count = 0;
            size_t element_capacity = 0;

            if (!check(parser, TOKEN_RBRACKET)) {
                do {
                    ArcAstNode *element = parse_expression(parser);
                    if (element) {
                        // Grow array if needed
                        if (element_count >= element_capacity) {
                            size_t new_capacity = element_capacity == 0 ? 4 : element_capacity * 2;
                            ArcAstNode **new_elements =
                                realloc(elements, new_capacity * sizeof(ArcAstNode *));
                            if (new_elements) {
                                elements = new_elements;
                                element_capacity = new_capacity;
                            }
                        }

                        if (element_count < element_capacity) {
                            elements[element_count++] = element;
                        }
                    }
                } while (match(parser, TOKEN_COMMA));
            }

            consume(parser, TOKEN_RBRACKET,
                    "Expected ']' after array elements");  // Convert to arena/heap allocated array
            node->array_literal_expr.element_count = element_count;
            if (element_count > 0) {
                node->array_literal_expr.elements =
                    (ArcAstNode **)alloc_array(parser, element_count, sizeof(ArcAstNode *));
                if (node->array_literal_expr.elements) {
                    memcpy(node->array_literal_expr.elements, elements,
                           element_count * sizeof(ArcAstNode *));
                }
            } else {
                node->array_literal_expr.elements = NULL;
            }

            free(elements);  // Free temporary array
            return node;
        }

        default:
            arc_parser_error(parser, "Unexpected token '%.*s'", (int)parser->current_token.length,
                             parser->current_token.start);
            return NULL;
    }
}

static ArcAstNode *parse_call(ArcParser *parser, ArcAstNode *function) {
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);
    ArcToken lparen_token = parser->previous_token;  // The '(' was already consumed by match()
    // Don't advance here - the '(' has already been consumed

    ArcAstNode *node = arc_ast_node_create(parser, AST_EXPR_CALL, source_info);
    if (!node)
        return NULL;
    node->call_expr.function = function;
    node->call_expr.lparen_token = lparen_token;

    // Parse arguments
    ArcAstNode **arguments = NULL;
    size_t argument_count = 0;
    size_t argument_capacity = 0;
    if (!check(parser, TOKEN_RPAREN)) {
        do {
            ArcAstNode *arg = parse_expression(parser);
            if (arg) {
                // Grow array if needed
                if (argument_count >= argument_capacity) {
                    size_t new_capacity = argument_capacity == 0 ? 4 : argument_capacity * 2;
                    ArcAstNode **new_args = realloc(arguments, new_capacity * sizeof(ArcAstNode *));
                    if (!new_args) {
                        free(arguments);
                        arc_parser_error(parser, "Out of memory");
                        return node;
                    }
                    arguments = new_args;
                    argument_capacity = new_capacity;
                }
                arguments[argument_count++] = arg;
            } else {
                // If expression parsing failed, we might be in an error state
                // Try to recover by consuming tokens until we find a comma or closing paren
                if (parser->panic_mode) {
                    while (!check(parser, TOKEN_COMMA) && !check(parser, TOKEN_RPAREN) &&
                           !check(parser, TOKEN_EOF)) {
                        advance(parser);
                    }
                }
            }
        } while (match(parser, TOKEN_COMMA));
    }
    consume(parser, TOKEN_RPAREN, "Expected ')' after function arguments");

    // Convert to arena/heap allocated array
    node->call_expr.argument_count = argument_count;
    if (argument_count > 0) {
        node->call_expr.arguments =
            (ArcAstNode **)alloc_array(parser, argument_count, sizeof(ArcAstNode *));
        if (node->call_expr.arguments) {
            memcpy(node->call_expr.arguments, arguments, sizeof(ArcAstNode *) * argument_count);
        }
    } else {
        node->call_expr.arguments = NULL;
    }

    free(arguments);
    return node;
}

static ArcAstNode *parse_index(ArcParser *parser, ArcAstNode *object) {
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);
    ArcToken lbracket_token = parser->current_token;
    advance(parser);  // consume '['

    ArcAstNode *index = parse_expression(parser);
    consume(parser, TOKEN_RBRACKET, "Expected ']' after index expression");

    ArcAstNode *node = arc_ast_node_create(parser, AST_EXPR_INDEX, source_info);
    if (node) {
        node->index_expr.object = object;
        node->index_expr.index = index;
        node->index_expr.lbracket_token = lbracket_token;
    }
    return node;
}

static ArcAstNode *parse_field_access(ArcParser *parser, ArcAstNode *object) {
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);
    ArcToken dot_token = parser->previous_token;  // '.' was already consumed

    ArcToken field_name = consume(parser, TOKEN_IDENTIFIER, "Expected field name after '.'");

    ArcAstNode *node = arc_ast_node_create(parser, AST_EXPR_FIELD_ACCESS, source_info);
    if (node) {
        node->field_access_expr.object = object;
        node->field_access_expr.field_name = field_name;
        node->field_access_expr.dot_token = dot_token;
    }
    return node;
}

static ArcAstNode *parse_unary(ArcParser *parser) {
    if (match(parser, TOKEN_BANG) || match(parser, TOKEN_MINUS) || match(parser, TOKEN_AMPERSAND) ||
        match(parser, TOKEN_ASTERISK) || match(parser, TOKEN_KEYWORD_NOT) ||
        match(parser, TOKEN_FORCE_UNWRAP)) {

        ArcToken operator_token = parser->previous_token;
        ArcSourceInfo source_info = arc_source_info_from_token(&operator_token, NULL);

        ArcAstNode *operand = parse_precedence(parser, PREC_UNARY);

        ArcAstNode *node = arc_ast_node_create(parser, AST_EXPR_UNARY, source_info);
        if (node) {
            node->unary_expr.op_type = token_to_unary_op(operator_token.type);
            node->unary_expr.operator_token = operator_token;
            node->unary_expr.operand = operand;
        }
        return node;
    }

    ArcAstNode *primary = parse_primary(
        parser);  // Handle postfix operators (call, index, field access, scope resolution)
    while (true) {
        if (match(parser, TOKEN_LPAREN)) {
            primary = parse_call(parser, primary);
        } else if (match(parser, TOKEN_LBRACKET)) {
            primary = parse_index(parser, primary);
        } else if (match(parser, TOKEN_DOT)) {
            primary = parse_field_access(parser, primary);
        } else if (match(parser, TOKEN_DOUBLE_COLON)) {
            primary = parse_scope_resolution(parser, primary);
        } else {
            break;
        }
    }

    return primary;
}

static ArcAstNode *parse_binary(ArcParser *parser, ArcAstNode *left) {
    ArcToken operator_token = parser->current_token;
    ArcSourceInfo source_info = arc_source_info_from_token(&operator_token, NULL);

    Precedence precedence = get_token_precedence(operator_token.type);
    advance(parser);  // consume operator

    ArcAstNode *right = parse_precedence(parser, (Precedence)(precedence + 1));

    ArcAstNode *node = arc_ast_node_create(parser, AST_EXPR_BINARY, source_info);
    if (node) {
        node->binary_expr.op_type = token_to_binary_op(operator_token.type);
        node->binary_expr.operator_token = operator_token;
        node->binary_expr.left = left;
        node->binary_expr.right = right;
    }
    return node;
}

static ArcAstNode *parse_precedence(ArcParser *parser, Precedence precedence) {
    ArcAstNode *left = parse_unary(parser);

    while (precedence <= get_token_precedence(parser->current_token.type)) {
        left = parse_binary(parser, left);
    }

    return left;
}

static ArcAstNode *parse_expression(ArcParser *parser) {
    return parse_precedence(parser, PREC_ASSIGNMENT);
}

// Parse type annotations
static ArcAstNode *parse_type(ArcParser *parser) {
    return parse_type_postfix(parser);
}

// Helper function to parse base types
static ArcAstNode *parse_base_type(ArcParser *parser) {
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);

    switch (parser->current_token.type) {
        // Primitive types
        case TOKEN_KEYWORD_I8:
        case TOKEN_KEYWORD_I16:
        case TOKEN_KEYWORD_I32:
        case TOKEN_KEYWORD_I64:
        case TOKEN_KEYWORD_ISIZE:
        case TOKEN_KEYWORD_U8:
        case TOKEN_KEYWORD_U16:
        case TOKEN_KEYWORD_U32:
        case TOKEN_KEYWORD_U64:
        case TOKEN_KEYWORD_USIZE:
        case TOKEN_KEYWORD_F32:
        case TOKEN_KEYWORD_F64:
        case TOKEN_KEYWORD_BOOL:
        case TOKEN_KEYWORD_CHAR:
        case TOKEN_KEYWORD_VOID: {
            ArcToken token = parser->current_token;
            advance(parser);

            ArcAstNode *node = arc_ast_node_create(parser, AST_TYPE_PRIMITIVE, source_info);
            if (node) {
                node->type_primitive.primitive_type = token.type;
                node->type_primitive.token = token;
            }
            return node;
        }

        case TOKEN_IDENTIFIER: {
            // User-defined types and named types
            ArcToken token = parser->current_token;
            advance(parser);

            // For now, treat as primitive type with identifier
            ArcAstNode *node = arc_ast_node_create(parser, AST_TYPE_PRIMITIVE, source_info);
            if (node) {
                node->type_primitive.primitive_type = TOKEN_IDENTIFIER;
                node->type_primitive.token = token;
            }
            return node;
        }

        case TOKEN_CARET: {
            // Mutable pointer type: ^Type
            ArcToken caret_token = parser->current_token;
            advance(parser);

            ArcAstNode *pointed_type = parse_base_type(parser);

            ArcAstNode *node = arc_ast_node_create(parser, AST_TYPE_POINTER, source_info);
            if (node) {
                node->type_pointer.pointed_type = pointed_type;
                node->type_pointer.caret_token = caret_token;
            }
            return node;
        }

        case TOKEN_ASTERISK: {
            // Immutable pointer type: *const Type (simplified for now as *Type)
            ArcToken asterisk_token = parser->current_token;
            advance(parser);

            // For now, ignore 'const' keyword and just parse as immutable pointer
            if (match(parser, TOKEN_KEYWORD_CONST)) {
                // Skip 'const'
            }

            ArcAstNode *pointed_type = parse_base_type(parser);

            ArcAstNode *node = arc_ast_node_create(parser, AST_TYPE_POINTER, source_info);
            if (node) {
                node->type_pointer.pointed_type = pointed_type;
                node->type_pointer.caret_token = asterisk_token;  // Reuse for immutable pointers
            }
            return node;
        }

        case TOKEN_LBRACKET: {
            // Array or slice type: [Type; size] or []Type
            ArcToken lbracket_token = parser->current_token;
            advance(parser);

            if (check(parser, TOKEN_RBRACKET)) {
                // Slice type: []Type
                advance(parser);  // consume ']'
                ArcAstNode *element_type = parse_type(parser);

                ArcAstNode *node = arc_ast_node_create(parser, AST_TYPE_SLICE, source_info);
                if (node) {
                    node->type_slice.element_type = element_type;
                    node->type_slice.lbracket_token = lbracket_token;
                }
                return node;
            } else {
                // Array type: [Type; size]
                ArcAstNode *element_type = parse_type(parser);
                consume(parser, TOKEN_SEMICOLON, "Expected ';' in array type");
                ArcAstNode *size_expr = parse_expression(parser);
                consume(parser, TOKEN_RBRACKET, "Expected ']' after array type");

                ArcAstNode *node = arc_ast_node_create(parser, AST_TYPE_ARRAY, source_info);
                if (node) {
                    node->type_array.element_type = element_type;
                    node->type_array.size_expr = size_expr;
                    node->type_array.lbracket_token = lbracket_token;
                }
                return node;
            }
        }
        case TOKEN_KEYWORD_FUNC: {
            // Function type: func(params) -> ReturnType
            ArcToken func_token = parser->current_token;
            advance(parser);

            consume(parser, TOKEN_LPAREN, "Expected '(' after 'func'");

            // Parse parameter types
            ArcAstNode **parameter_types = NULL;
            size_t parameter_count = 0;
            size_t parameter_capacity = 0;

            if (!check(parser, TOKEN_RPAREN)) {
                do {
                    if (parameter_count >= parameter_capacity) {
                        parameter_capacity = parameter_capacity == 0 ? 4 : parameter_capacity * 2;
                        parameter_types =
                            realloc(parameter_types, parameter_capacity * sizeof(ArcAstNode *));
                        if (!parameter_types) {
                            arc_parser_error(parser,
                                             "Out of memory parsing function type parameters");
                            return NULL;
                        }
                    }

                    parameter_types[parameter_count++] = parse_type(parser);
                } while (match(parser, TOKEN_COMMA));
            }

            consume(parser, TOKEN_RPAREN, "Expected ')' after function parameters");

            ArcAstNode *return_type = NULL;
            if (match(parser, TOKEN_ARROW)) {
                return_type = parse_type(parser);
            }

            ArcAstNode *node = arc_ast_node_create(parser, AST_TYPE_FUNCTION, source_info);
            if (node) {
                // Allocate final parameter array in parser's memory system
                if (parameter_count > 0) {
                    node->type_function.parameter_types =
                        (ArcAstNode **)alloc_array(parser, parameter_count, sizeof(ArcAstNode *));
                    if (node->type_function.parameter_types) {
                        memcpy(node->type_function.parameter_types, parameter_types,
                               parameter_count * sizeof(ArcAstNode *));
                    }
                } else {
                    node->type_function.parameter_types = NULL;
                }
                node->type_function.parameter_count = parameter_count;
                node->type_function.return_type = return_type;
                node->type_function.fn_token = func_token;
            }

            free(parameter_types);
            return node;
        }

        case TOKEN_LPAREN: {
            // Parenthesized type: (Type)
            advance(parser);  // consume '('
            ArcAstNode *inner_type = parse_type(parser);
            consume(parser, TOKEN_RPAREN, "Expected ')' after parenthesized type");
            return inner_type;
        }

        default:
            arc_parser_error(parser, "Expected type annotation");
            return NULL;
    }
}

// Helper function to parse type with optional postfix (for optional types)
static ArcAstNode *parse_type_postfix(ArcParser *parser) {
    ArcAstNode *base_type = parse_base_type(parser);
    if (!base_type)
        return NULL;

    // Handle optional type postfix: Type?
    if (match(parser, TOKEN_QUESTION)) {
        ArcSourceInfo source_info = arc_parser_current_source_info(parser);
        ArcToken question_token = parser->previous_token;

        ArcAstNode *node = arc_ast_node_create(parser, AST_TYPE_OPTIONAL, source_info);
        if (node) {
            node->type_optional.inner_type = base_type;
            node->type_optional.question_token = question_token;
        }
        return node;
    }

    return base_type;
}

// === STATEMENT PARSING ===

static ArcAstNode *parse_expression_statement(ArcParser *parser) {
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);

    // Check if we're trying to parse something that's not an expression
    if (check(parser, TOKEN_RBRACE) || check(parser, TOKEN_EOF)) {
        arc_parser_error(parser, "Unexpected token in expression statement");
        return NULL;
    }

    ArcAstNode *expr = parse_expression(parser);  // If expression parsing failed, try to recover
    if (!expr) {
        // Skip tokens until we find a statement terminator
        while (!is_statement_terminator(parser)) {
            advance(parser);
        }
    }

    consume_statement_terminator(parser);

    ArcAstNode *node = arc_ast_node_create(parser, AST_STMT_EXPRESSION, source_info);
    if (node) {
        node->expr_stmt.expression = expr;
    }
    return node;
}

// Forward declaration needed for parse_block_statement
static ArcAstNode *parse_statement(ArcParser *parser);

static ArcAstNode *parse_block_statement(ArcParser *parser) {
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);
    ArcToken lbrace_token = consume(parser, TOKEN_LBRACE, "Expected '{'");

    ArcAstNode *node = arc_ast_node_create(parser, AST_STMT_BLOCK, source_info);
    if (!node)
        return NULL;

    node->block_stmt.lbrace_token = lbrace_token;

    // Parse statements using dynamic array
    ArcAstNode **statements = NULL;
    size_t statement_count = 0;
    size_t statement_capacity = 0;

    while (!check(parser, TOKEN_RBRACE) && !check(parser, TOKEN_EOF)) {
        printf("parse_block_statement: parsing statement, current token = %s\n",
               arc_token_type_to_string(parser->current_token.type));

        ArcAstNode *stmt = parse_statement(parser);
        if (stmt) {
            // Grow array if needed
            if (statement_count >= statement_capacity) {
                size_t new_capacity = statement_capacity == 0 ? 4 : statement_capacity * 2;
                ArcAstNode **new_statements =
                    realloc(statements, sizeof(ArcAstNode *) * new_capacity);
                if (!new_statements) {
                    free(statements);
                    return node;
                }
                statements = new_statements;
                statement_capacity = new_capacity;
            }
            statements[statement_count++] = stmt;
        } else {
            // If parse_statement returns NULL, we might be at the end of the block
            // or encountered an error. Check if we're at a valid stopping point.
            if (check(parser, TOKEN_RBRACE)) {
                printf("parse_block_statement: reached end of block\n");
                break;
            }

            // Skip the problematic token to avoid infinite loop
            printf("parse_block_statement: skipping problematic token %s\n",
                   arc_token_type_to_string(parser->current_token.type));
            advance(parser);
        }

        if (parser->panic_mode) {
            arc_parser_synchronize(parser);
        }
    }

    consume(parser, TOKEN_RBRACE, "Expected '}' after block");

    // Convert to arena/heap allocated array
    node->block_stmt.statement_count = statement_count;
    if (statement_count > 0) {
        node->block_stmt.statements =
            (ArcAstNode **)alloc_array(parser, statement_count, sizeof(ArcAstNode *));
        if (node->block_stmt.statements) {
            memcpy(node->block_stmt.statements, statements, sizeof(ArcAstNode *) * statement_count);
        }
    } else {
        node->block_stmt.statements = NULL;
    }

    free(statements);
    return node;
}

static ArcAstNode *parse_if_statement(ArcParser *parser) {
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);
    ArcToken if_token = consume(parser, TOKEN_KEYWORD_IF, "Expected 'if'");

    ArcAstNode *condition = parse_expression(parser);
    ArcAstNode *then_stmt = parse_statement(parser);
    ArcAstNode *else_stmt = NULL;  // Handle elif and else
    if (check(parser, TOKEN_KEYWORD_ELIF)) {
        advance(parser);  // consume 'elif'
        ArcAstNode *elif_condition = parse_expression(parser);
        ArcAstNode *elif_then_stmt = parse_statement(parser);

        // Create nested if statement for elif
        ArcSourceInfo elif_source_info = arc_parser_current_source_info(parser);
        ArcAstNode *elif_node = arc_ast_node_create(parser, AST_STMT_IF, elif_source_info);
        if (elif_node) {
            elif_node->if_stmt.if_token = if_token;  // Reuse token for simplicity
            elif_node->if_stmt.condition = elif_condition;
            elif_node->if_stmt.then_branch = elif_then_stmt;
            elif_node->if_stmt.else_branch = NULL;

            // Handle else after elif
            if (match(parser, TOKEN_KEYWORD_ELSE)) {
                elif_node->if_stmt.else_branch = parse_statement(parser);
            }
        }
        else_stmt = elif_node;
    } else if (match(parser, TOKEN_KEYWORD_ELSE)) {
        else_stmt = parse_statement(parser);
    }

    ArcAstNode *node = arc_ast_node_create(parser, AST_STMT_IF, source_info);
    if (node) {
        node->if_stmt.if_token = if_token;
        node->if_stmt.condition = condition;
        node->if_stmt.then_branch = then_stmt;
        node->if_stmt.else_branch = else_stmt;
    }
    return node;
}

static ArcAstNode *parse_while_statement(ArcParser *parser) {
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);
    ArcToken while_token = consume(parser, TOKEN_KEYWORD_WHILE, "Expected 'while'");

    ArcAstNode *condition = parse_expression(parser);
    ArcAstNode *body = parse_statement(parser);

    ArcAstNode *node = arc_ast_node_create(parser, AST_STMT_WHILE, source_info);
    if (node) {
        node->while_stmt.while_token = while_token;
        node->while_stmt.condition = condition;
        node->while_stmt.body = body;
    }
    return node;
}

static ArcAstNode *parse_for_statement(ArcParser *parser) {
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);
    ArcToken for_token = consume(parser, TOKEN_KEYWORD_FOR, "Expected 'for'");

    ArcToken iterator = consume(parser, TOKEN_IDENTIFIER, "Expected iterator variable name");
    ArcToken in_token = consume(parser, TOKEN_KEYWORD_IN, "Expected 'in' after iterator variable");
    ArcAstNode *iterable = parse_expression(parser);
    ArcAstNode *body = parse_statement(parser);

    ArcAstNode *node = arc_ast_node_create(parser, AST_STMT_FOR, source_info);
    if (node) {
        node->for_stmt.for_token = for_token;
        node->for_stmt.iterator = iterator;
        node->for_stmt.in_token = in_token;
        node->for_stmt.iterable = iterable;
        node->for_stmt.body = body;
    }
    return node;
}

static ArcAstNode *parse_return_statement(ArcParser *parser) {
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);
    ArcToken return_token = consume(parser, TOKEN_KEYWORD_RETURN, "Expected 'return'");

    ArcAstNode *value = NULL;
    if (!check(parser, TOKEN_SEMICOLON)) {
        value = parse_expression(parser);
    }

    consume_statement_terminator(parser);

    ArcAstNode *node = arc_ast_node_create(parser, AST_STMT_RETURN, source_info);
    if (node) {
        node->return_stmt.return_token = return_token;
        node->return_stmt.value = value;
    }
    return node;
}

static ArcAstNode *parse_break_statement(ArcParser *parser) {
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);
    ArcToken break_token = consume(parser, TOKEN_KEYWORD_BREAK, "Expected 'break'");

    consume_statement_terminator(parser);

    ArcAstNode *node = arc_ast_node_create(parser, AST_STMT_BREAK, source_info);
    if (node) {
        node->break_stmt.break_token = break_token;
    }
    return node;
}

static ArcAstNode *parse_continue_statement(ArcParser *parser) {
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);
    ArcToken continue_token = consume(parser, TOKEN_KEYWORD_CONTINUE, "Expected 'continue'");

    consume_statement_terminator(parser);

    ArcAstNode *node = arc_ast_node_create(parser, AST_STMT_CONTINUE, source_info);
    if (node) {
        node->continue_stmt.continue_token = continue_token;
    }
    return node;
}

static ArcAstNode *parse_match_statement(ArcParser *parser) {
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);
    ArcToken match_token = consume(parser, TOKEN_KEYWORD_MATCH, "Expected 'match'");

    ArcAstNode *value = parse_expression(parser);
    consume(parser, TOKEN_LBRACE, "Expected '{' after match expression");

    // For now, create a simple AST node - full match implementation would need more work
    ArcAstNode *node = arc_ast_node_create(parser, AST_STMT_EXPRESSION, source_info);
    if (node) {
        // Store match information in expression statement for now
        node->expr_stmt.expression = value;
        // Use the match_token to avoid unused variable warning
        (void)match_token;
    }

    // Skip to closing brace (simplified implementation)
    while (!check(parser, TOKEN_RBRACE) && !check(parser, TOKEN_EOF)) {
        advance(parser);
    }
    consume(parser, TOKEN_RBRACE, "Expected '}' after match body");

    return node;
}

static ArcAstNode *parse_defer_statement(ArcParser *parser) {
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);
    ArcToken defer_token = consume(parser, TOKEN_KEYWORD_DEFER, "Expected 'defer'");

    ArcAstNode *stmt = parse_statement(parser);

    // For now, wrap in expression statement
    ArcAstNode *node = arc_ast_node_create(parser, AST_STMT_EXPRESSION, source_info);
    if (node) {
        node->expr_stmt.expression = stmt;
        // Use the defer_token to avoid unused variable warning
        (void)defer_token;
    }

    return node;
}

static ArcAstNode *parse_statement(ArcParser *parser) {
    printf("parse_statement: current token = %s\n",
           arc_token_type_to_string(parser->current_token.type));

    // Skip newlines at statement level - but don't recurse infinitely
    while (match(parser, TOKEN_NEWLINE)) {
        printf("parse_statement: skipping newline\n");
        // Continue the loop to consume all consecutive newlines
    }

    switch (parser->current_token.type) {
        case TOKEN_KEYWORD_IF:
            return parse_if_statement(parser);
        case TOKEN_KEYWORD_WHILE:
            return parse_while_statement(parser);
        case TOKEN_KEYWORD_FOR:
            return parse_for_statement(parser);
        case TOKEN_KEYWORD_RETURN:
            return parse_return_statement(parser);
        case TOKEN_KEYWORD_BREAK:
            return parse_break_statement(parser);
        case TOKEN_KEYWORD_CONTINUE:
            return parse_continue_statement(parser);
        case TOKEN_KEYWORD_MATCH:
            return parse_match_statement(parser);
        case TOKEN_KEYWORD_DEFER:
            return parse_defer_statement(parser);
        case TOKEN_LBRACE:
            return parse_block_statement(parser);
        case TOKEN_KEYWORD_LET:
        case TOKEN_KEYWORD_MUT:
        case TOKEN_KEYWORD_CONST:
            return parse_variable_declaration(parser);

        // Handle block terminators
        case TOKEN_RBRACE:
        case TOKEN_EOF:
            printf("parse_statement: reached end of block/file\n");
            return NULL;  // Let the calling function handle this

        default:
            printf("parse_statement: defaulting to expression statement\n");
            return parse_expression_statement(parser);
    }
}

// === DECLARATION PARSING ===

static ArcAstNode *parse_variable_declaration(ArcParser *parser) {
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);

    // Handle let, mut, or const keywords
    ArcToken decl_token;
    if (check(parser, TOKEN_KEYWORD_LET)) {
        decl_token = consume(parser, TOKEN_KEYWORD_LET, "Expected 'let'");
    } else if (check(parser, TOKEN_KEYWORD_MUT)) {
        decl_token = consume(parser, TOKEN_KEYWORD_MUT, "Expected 'mut'");
    } else if (check(parser, TOKEN_KEYWORD_CONST)) {
        decl_token = consume(parser, TOKEN_KEYWORD_CONST, "Expected 'const'");
    } else {
        // Error - should not reach here given the switch cases
        arc_parser_error(parser, "Expected variable declaration keyword");
        return NULL;
    }

    ArcToken name = consume(parser, TOKEN_IDENTIFIER, "Expected variable name");

    ArcAstNode *type_annotation = NULL;
    if (match(parser, TOKEN_COLON)) {
        type_annotation = parse_type(parser);
    }

    ArcAstNode *initializer = NULL;
    if (match(parser, TOKEN_EQUAL)) {
        initializer = parse_expression(parser);
    }

    consume_statement_terminator(parser);

    ArcAstNode *node = arc_ast_node_create(parser, AST_STMT_VAR_DECL, source_info);
    if (node) {
        node->var_decl_stmt.var_token = decl_token;
        node->var_decl_stmt.name = name;
        node->var_decl_stmt.type_annotation = type_annotation;
        node->var_decl_stmt.initializer = initializer;
    }
    return node;
}

static ArcAstNode *parse_function_declaration(ArcParser *parser) {
    printf("parse_function_declaration: starting\n");
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);
    ArcToken func_token = consume(parser, TOKEN_KEYWORD_FUNC, "Expected 'func'");
    printf("parse_function_declaration: consumed 'func'\n");

    ArcToken name = consume(parser, TOKEN_IDENTIFIER, "Expected function name");
    printf("parse_function_declaration: consumed function name '%.*s'\n", (int)name.length,
           name.start);

    consume(parser, TOKEN_LPAREN, "Expected '(' after function name");
    printf("parse_function_declaration: consumed '('\n");

    // Parse parameters
    ArcAstNode **parameters = NULL;
    size_t parameter_count = 0;
    size_t parameter_capacity = 0;

    if (!check(parser, TOKEN_RPAREN)) {
        printf("parse_function_declaration: parsing parameters\n");
        do {
            // Parse parameter: name: type
            ArcSourceInfo param_source = arc_parser_current_source_info(parser);
            ArcToken param_name = consume(parser, TOKEN_IDENTIFIER, "Expected parameter name");

            consume(parser, TOKEN_COLON, "Expected ':' after parameter name");

            // Parse parameter type
            ArcAstNode *param_type = parse_type(parser);

            // Create parameter node
            ArcAstNode *param = arc_ast_node_create(parser, AST_PARAMETER, param_source);
            if (param) {
                param->parameter.name = param_name;
                param->parameter.type_annotation = param_type;
            }

            // Add to parameters array
            if (parameter_count >= parameter_capacity) {
                size_t new_capacity = parameter_capacity == 0 ? 4 : parameter_capacity * 2;
                ArcAstNode **new_parameters =
                    realloc(parameters, sizeof(ArcAstNode *) * new_capacity);
                if (!new_parameters) {
                    free(parameters);
                    return NULL;
                }
                parameters = new_parameters;
                parameter_capacity = new_capacity;
            }
            parameters[parameter_count++] = param;

        } while (match(parser, TOKEN_COMMA));
    }

    printf("parse_function_declaration: consuming ')'\n");
    consume(parser, TOKEN_RPAREN, "Expected ')' after parameters");

    // Parse return type
    ArcAstNode *return_type = NULL;
    if (match(parser, TOKEN_ARROW)) {
        printf("parse_function_declaration: parsing return type\n");
        return_type = parse_type(parser);
    }

    // Parse body
    printf("parse_function_declaration: parsing body\n");
    ArcAstNode *body = parse_block_statement(parser);
    printf("parse_function_declaration: finished parsing body\n");

    ArcAstNode *node = arc_ast_node_create(parser, AST_DECL_FUNCTION, source_info);
    if (node) {
        node->function_decl.fn_token = func_token;
        node->function_decl.name = name;
        node->function_decl.parameter_count = parameter_count;
        node->function_decl.return_type = return_type;
        node->function_decl.body = body;

        // Convert parameters to arena/heap allocated array
        if (parameter_count > 0) {
            node->function_decl.parameters =
                (ArcAstNode **)alloc_array(parser, parameter_count, sizeof(ArcAstNode *));
            if (node->function_decl.parameters) {
                memcpy(node->function_decl.parameters, parameters,
                       sizeof(ArcAstNode *) * parameter_count);
            }
        } else {
            node->function_decl.parameters = NULL;
        }
    }

    free(parameters);
    return node;
}

static ArcAstNode *parse_module_declaration(ArcParser *parser) {
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);
    ArcToken mod_token = consume(parser, TOKEN_KEYWORD_MOD, "Expected 'mod'");
    ArcToken name = consume(parser, TOKEN_IDENTIFIER, "Expected module name");

    ArcAstNode *node = arc_ast_node_create(parser, AST_DECL_MODULE, source_info);
    if (!node)
        return NULL;

    node->module_decl.mod_token = mod_token;
    node->module_decl.name = name;
    node->module_decl.body = NULL;  // Initialize as external module
    node->module_decl.is_external = true;

    // Check if this is a module with a body or just a declaration
    if (match(parser, TOKEN_LBRACE)) {
        // Module with body: mod name { ... }
        node->module_decl.is_external = false;

        // Parse module body as a list of declarations
        ArcAstNode **declarations = NULL;
        size_t declaration_count = 0;
        size_t declaration_capacity = 0;

        while (!check(parser, TOKEN_RBRACE) && !check(parser, TOKEN_EOF)) {
            ArcAstNode *decl = parse_declaration(parser);
            if (!decl) {
                // Error recovery - skip to next declaration
                while (!check(parser, TOKEN_RBRACE) && !check(parser, TOKEN_EOF) &&
                       !is_statement_terminator(parser)) {
                    advance(parser);
                }
                if (is_statement_terminator(parser)) {
                    advance(parser);
                }
                continue;
            }  // Grow declarations array if needed
            if (declaration_count >= declaration_capacity) {
                size_t new_capacity = declaration_capacity == 0 ? 8 : declaration_capacity * 2;
                ArcAstNode **new_declarations =
                    arc_arena_alloc(parser->ast_arena, new_capacity * sizeof(ArcAstNode *));
                if (!new_declarations) {
                    return NULL;
                }

                if (declarations) {
                    memcpy(new_declarations, declarations,
                           declaration_count * sizeof(ArcAstNode *));
                }
                declarations = new_declarations;
                declaration_capacity = new_capacity;
            }

            declarations[declaration_count++] = decl;
        }

        consume(parser, TOKEN_RBRACE, "Expected '}' after module body");

        // Create module body node
        if (declaration_count > 0) {
            ArcSourceInfo body_source = source_info;
            ArcAstNode *body = arc_ast_node_create(parser, AST_STMT_BLOCK, body_source);
            if (body) {
                body->block_stmt.statements = declarations;
                body->block_stmt.statement_count = declaration_count;
                node->module_decl.body = body;
            }
        }
    } else {
        // Simple module declaration: mod name;
        consume_statement_terminator(parser);
    }

    return node;
}

static ArcAstNode *parse_module_path(ArcParser *parser) {
    // Parse a module path like: std::io::File or just identifier
    // For now, we'll create a simple identifier chain
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);
    ArcToken first_name = consume(parser, TOKEN_IDENTIFIER, "Expected module path identifier");

    // If we don't have ::, just return a simple identifier
    if (!check(parser, TOKEN_DOUBLE_COLON)) {
        ArcAstNode *node = arc_ast_node_create(parser, AST_IDENTIFIER, source_info);
        if (node) {
            node->identifier.token = first_name;
        }
        return node;
    }

    // Build a chain of field access expressions for module::path::syntax
    ArcAstNode *current = arc_ast_node_create(parser, AST_IDENTIFIER, source_info);
    if (current) {
        current->identifier.token = first_name;
    }

    while (match(parser, TOKEN_DOUBLE_COLON)) {
        // Check if the next token is { or * (import specifiers)
        if (check(parser, TOKEN_LBRACE) || check(parser, TOKEN_ASTERISK)) {
            // This :: is for import specifiers, not part of the path
            // We need to "put back" the :: token - but that's complex
            // For now, let's handle this in the use declaration parser
            break;
        }

        ArcToken next_name = consume(parser, TOKEN_IDENTIFIER, "Expected identifier after '::'");

        ArcSourceInfo access_source = arc_parser_current_source_info(parser);
        ArcAstNode *access_node = arc_ast_node_create(parser, AST_EXPR_FIELD_ACCESS, access_source);
        if (access_node) {
            access_node->field_access_expr.object = current;
            access_node->field_access_expr.field_name = next_name;
            // Use previous :: token for dot_token (semantically similar)
            access_node->field_access_expr.dot_token = parser->previous_token;
        }
        current = access_node;
    }

    return current;
}

static ArcAstNode *parse_use_declaration(ArcParser *parser) {
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);
    ArcToken use_token = consume(parser, TOKEN_KEYWORD_USE, "Expected 'use'");

    // Parse the module path - but for use declarations, we parse it differently
    // to handle use module::{imports} syntax
    ArcSourceInfo path_source = arc_parser_current_source_info(parser);
    ArcToken first_name = consume(parser, TOKEN_IDENTIFIER, "Expected module name");

    // Create simple identifier for the module name
    ArcAstNode *path = arc_ast_node_create(parser, AST_IDENTIFIER, path_source);
    if (path) {
        path->identifier.token = first_name;
    }

    // Check for import specifiers: use std::collections::{Vec, HashMap}
    ArcAstNode **imports = NULL;
    size_t import_count = 0;
    bool is_glob_import = false;

    if (match(parser, TOKEN_DOUBLE_COLON)) {
        if (match(parser, TOKEN_ASTERISK)) {
            // Glob import: use module::*
            is_glob_import = true;
        } else if (match(parser, TOKEN_LBRACE)) {
            // Specific imports: use module::{item1, item2}
            size_t import_capacity = 0;

            if (!check(parser, TOKEN_RBRACE)) {
                do {
                    ArcToken import_name =
                        consume(parser, TOKEN_IDENTIFIER, "Expected import name");

                    // Create identifier node for this import
                    ArcSourceInfo import_source = arc_parser_current_source_info(parser);
                    ArcAstNode *import_node =
                        arc_ast_node_create(parser, AST_IDENTIFIER, import_source);
                    if (import_node) {
                        import_node->identifier.token = import_name;
                    }

                    // Check for alias: item as alias (using identifier for now)
                    if (check(parser, TOKEN_IDENTIFIER)) {
                        // Peek to see if this might be 'as'
                        ArcToken peek_token = parser->current_token;
                        if (peek_token.length == 2 && strncmp(peek_token.start, "as", 2) == 0) {
                            advance(parser);  // consume 'as'
                            ArcToken alias_name =
                                consume(parser, TOKEN_IDENTIFIER, "Expected alias name");
                            // For now, store the alias in a field access expression
                            ArcAstNode *alias_node =
                                arc_ast_node_create(parser, AST_EXPR_FIELD_ACCESS, import_source);
                            if (alias_node) {
                                alias_node->field_access_expr.object = import_node;
                                alias_node->field_access_expr.field_name = alias_name;
                                alias_node->field_access_expr.dot_token =
                                    parser->previous_token;  // The 'as' token
                            }
                            import_node = alias_node;
                        }
                    }

                    // Grow imports array if needed
                    if (import_count >= import_capacity) {
                        size_t new_capacity = import_capacity == 0 ? 4 : import_capacity * 2;
                        ArcAstNode **new_imports =
                            arc_arena_alloc(parser->ast_arena, new_capacity * sizeof(ArcAstNode *));
                        if (!new_imports) {
                            return NULL;
                        }

                        if (imports) {
                            memcpy(new_imports, imports, import_count * sizeof(ArcAstNode *));
                        }
                        imports = new_imports;
                        import_capacity = new_capacity;
                    }

                    imports[import_count++] = import_node;
                } while (match(parser, TOKEN_COMMA));
            }

            consume(parser, TOKEN_RBRACE, "Expected '}' after import list");
        } else {
            // Single item import: use module::item
            ArcToken item_name = consume(parser, TOKEN_IDENTIFIER, "Expected item name after '::'");

            ArcSourceInfo item_source = arc_parser_current_source_info(parser);
            ArcAstNode *item_node = arc_ast_node_create(parser, AST_IDENTIFIER, item_source);
            if (item_node) {
                item_node->identifier.token = item_name;
            }

            // Check for alias (simplified check for now)
            if (check(parser, TOKEN_IDENTIFIER)) {
                // Peek to see if this might be 'as'
                ArcToken peek_token = parser->current_token;
                if (peek_token.length == 2 && strncmp(peek_token.start, "as", 2) == 0) {
                    advance(parser);  // consume 'as'
                    ArcToken alias_name = consume(parser, TOKEN_IDENTIFIER, "Expected alias name");
                    ArcAstNode *alias_node =
                        arc_ast_node_create(parser, AST_EXPR_FIELD_ACCESS, item_source);
                    if (alias_node) {
                        alias_node->field_access_expr.object = item_node;
                        alias_node->field_access_expr.field_name = alias_name;
                        alias_node->field_access_expr.dot_token = parser->previous_token;
                    }
                    item_node = alias_node;
                }
            }

            imports = arc_arena_alloc(parser->ast_arena, sizeof(ArcAstNode *));
            if (imports) {
                imports[0] = item_node;
                import_count = 1;
            }
        }
    }

    consume_statement_terminator(parser);

    ArcAstNode *node = arc_ast_node_create(parser, AST_DECL_USE, source_info);
    if (node) {
        node->use_decl.use_token = use_token;
        node->use_decl.path = path;
        node->use_decl.imports = imports;
        node->use_decl.import_count = import_count;
        node->use_decl.is_glob_import = is_glob_import;
    }
    return node;
}

static ArcAstNode *parse_extern_declaration(ArcParser *parser) {
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);
    ArcToken extern_token = consume(parser, TOKEN_KEYWORD_EXTERN, "Expected 'extern'");
    ArcToken func_token = consume(parser, TOKEN_KEYWORD_FUNC, "Expected 'func' after 'extern'");

    ArcToken name = consume(parser, TOKEN_IDENTIFIER, "Expected function name");

    consume(parser, TOKEN_LPAREN, "Expected '(' after function name");

    // Parse parameters (same as regular function)
    ArcAstNode **parameters = NULL;
    size_t parameter_count = 0;
    size_t parameter_capacity = 0;

    if (!check(parser, TOKEN_RPAREN)) {
        do {
            // Parse parameter: name: type
            ArcSourceInfo param_source = arc_parser_current_source_info(parser);
            ArcToken param_name = consume(parser, TOKEN_IDENTIFIER, "Expected parameter name");

            consume(parser, TOKEN_COLON, "Expected ':' after parameter name");

            // Parse parameter type
            ArcAstNode *param_type = parse_type(parser);

            // Create parameter node
            ArcAstNode *param = arc_ast_node_create(parser, AST_PARAMETER, param_source);
            if (param) {
                param->parameter.name = param_name;
                param->parameter.type_annotation = param_type;
            }

            // Add to parameters array
            if (parameter_count >= parameter_capacity) {
                size_t new_capacity = parameter_capacity == 0 ? 4 : parameter_capacity * 2;
                ArcAstNode **new_parameters =
                    realloc(parameters, sizeof(ArcAstNode *) * new_capacity);
                if (!new_parameters) {
                    free(parameters);
                    return NULL;
                }
                parameters = new_parameters;
                parameter_capacity = new_capacity;
            }
            parameters[parameter_count++] = param;

        } while (match(parser, TOKEN_COMMA));
    }

    consume(parser, TOKEN_RPAREN, "Expected ')' after parameters");

    // Parse return type
    ArcAstNode *return_type = NULL;
    if (match(parser, TOKEN_ARROW)) {
        return_type = parse_type(parser);
    }

    // Extern functions end with semicolon, no body
    consume(parser, TOKEN_SEMICOLON, "Expected ';' after extern function declaration");

    ArcAstNode *node = arc_ast_node_create(parser, AST_DECL_EXTERN, source_info);
    if (node) {
        node->extern_decl.extern_token = extern_token;
        node->extern_decl.fn_token = func_token;
        node->extern_decl.name = name;
        node->extern_decl.parameter_count = parameter_count;
        node->extern_decl.return_type = return_type;
        node->extern_decl.c_name = NULL;  // For now, use Arc name as C name

        // Convert parameters to arena/heap allocated array
        if (parameter_count > 0) {
            node->extern_decl.parameters =
                (ArcAstNode **)alloc_array(parser, parameter_count, sizeof(ArcAstNode *));
            if (node->extern_decl.parameters) {
                memcpy(node->extern_decl.parameters, parameters,
                       sizeof(ArcAstNode *) * parameter_count);
            }
        } else {
            node->extern_decl.parameters = NULL;
        }
    }

    free(parameters);
    return node;
}

static ArcAstNode *parse_declaration(ArcParser *parser) {
    ArcToken start_token = parser->current_token;  // Track starting position
    bool is_public = false;

    // Check for pub modifier
    if (match(parser, TOKEN_KEYWORD_PUB)) {
        is_public = true;
    }

    ArcAstNode *decl = NULL;
    switch (parser->current_token.type) {
        case TOKEN_KEYWORD_EXTERN:
            decl = parse_extern_declaration(parser);
            break;
        case TOKEN_KEYWORD_FUNC:
            decl = parse_function_declaration(parser);
            break;
        case TOKEN_KEYWORD_CONST:
        case TOKEN_KEYWORD_LET:
        case TOKEN_KEYWORD_MUT:
            decl = parse_variable_declaration(parser);
            break;
        case TOKEN_KEYWORD_MOD:
            decl = parse_module_declaration(parser);
            break;
        case TOKEN_KEYWORD_USE:
            decl = parse_use_declaration(parser);
            break;
        default:
            arc_parser_error(parser, "Expected declaration, got '%.*s'",
                             (int)parser->current_token.length, parser->current_token.start);

            // If we haven't advanced, force advancement to prevent infinite loop
            if (parser->current_token.type == start_token.type &&
                parser->current_token.start == start_token.start) {
                advance(parser);
            }
            return NULL;
    }

    // Set visibility flag on the declaration if successfully parsed
    if (decl && is_public) {
        // Store the visibility in the source info for now        // TODO: Add proper visibility
        // field to all declaration AST nodes
    }

    return decl;
}

// === TOP-LEVEL PARSING ===

ArcAstNode *arc_parser_parse_program(ArcParser *parser) {
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);
    ArcAstNode *program = arc_ast_node_create(parser, AST_PROGRAM, source_info);
    if (!program)
        return NULL;

    ArcAstNode **declarations = NULL;
    size_t declaration_count = 0;
    size_t declaration_capacity = 0;

    printf("Parsing program, current token: %s\n",
           arc_token_type_to_string(parser->current_token.type));

    while (!check(parser, TOKEN_EOF)) {
        printf("Loop iteration, current token: %s\n",
               arc_token_type_to_string(parser->current_token.type));  // Skip newlines at top level
        if (match(parser, TOKEN_NEWLINE)) {
            printf("Skipping %s\n", arc_token_type_to_string(parser->previous_token.type));
            continue;
        }

        ArcAstNode *decl = parse_declaration(parser);
        printf("Parsed declaration: %p\n", (void *)decl);

        if (decl) {
            // Grow array if needed
            if (declaration_count >= declaration_capacity) {
                size_t new_capacity = declaration_capacity == 0 ? 4 : declaration_capacity * 2;
                ArcAstNode **new_declarations =
                    realloc(declarations, sizeof(ArcAstNode *) * new_capacity);
                if (!new_declarations) {
                    free(declarations);
                    return program;
                }
                declarations = new_declarations;
                declaration_capacity = new_capacity;
            }
            declarations[declaration_count++] = decl;
            printf("Added declaration %zu\n", declaration_count);
        }

        if (parser->panic_mode) {
            printf("Parser in panic mode, synchronizing\n");
            arc_parser_synchronize(parser);
        }

        // Safety check to prevent infinite loops
        if (declaration_count > 100) {
            printf("Too many declarations, breaking\n");
            break;
        }
    }

    // Convert to arena/heap allocated array
    program->program.declaration_count = declaration_count;
    if (declaration_count > 0) {
        program->program.declarations =
            (ArcAstNode **)alloc_array(parser, declaration_count, sizeof(ArcAstNode *));
        if (program->program.declarations) {
            memcpy(program->program.declarations, declarations,
                   sizeof(ArcAstNode *) * declaration_count);
        }
    } else {
        program->program.declarations = NULL;
    }

    free(declarations);
    return program;
}

// === AST PRINTING (for debugging) ===

static void print_indent(int depth) {
    for (int i = 0; i < depth * 2; i++) {
        printf(" ");
    }
}

void arc_ast_node_print(const ArcAstNode *node, int depth) {
    if (!node) {
        print_indent(depth);
        printf("(null)\n");
        return;
    }

    print_indent(depth);

    switch (node->type) {
        case AST_PROGRAM:
            printf("Program (%zu declarations)\n", node->program.declaration_count);
            for (size_t i = 0; i < node->program.declaration_count; i++) {
                arc_ast_node_print(node->program.declarations[i], depth + 1);
            }
            break;

        case AST_DECL_FUNCTION:
            printf("FunctionDecl '%.*s' (%zu params)\n", (int)node->function_decl.name.length,
                   node->function_decl.name.start, node->function_decl.parameter_count);
            for (size_t i = 0; i < node->function_decl.parameter_count; i++) {
                arc_ast_node_print(node->function_decl.parameters[i], depth + 1);
            }
            if (node->function_decl.return_type) {
                print_indent(depth + 1);
                printf("ReturnType:\n");
                arc_ast_node_print(node->function_decl.return_type, depth + 2);
            }
            print_indent(depth + 1);
            printf("Body:\n");
            arc_ast_node_print(node->function_decl.body, depth + 2);
            break;

        case AST_DECL_MODULE:
            printf("ModuleDecl '%.*s'\n", (int)node->module_decl.name.length,
                   node->module_decl.name.start);
            break;

        case AST_DECL_USE:
            printf("UseDecl\n");
            print_indent(depth + 1);
            printf("Path:\n");
            arc_ast_node_print(node->use_decl.path, depth + 2);
            break;

        case AST_STMT_VAR_DECL:
            printf("VariableDecl '%.*s'\n", (int)node->var_decl_stmt.name.length,
                   node->var_decl_stmt.name.start);
            if (node->var_decl_stmt.type_annotation) {
                print_indent(depth + 1);
                printf("Type:\n");
                arc_ast_node_print(node->var_decl_stmt.type_annotation, depth + 2);
            }
            if (node->var_decl_stmt.initializer) {
                print_indent(depth + 1);
                printf("Initializer:\n");
                arc_ast_node_print(node->var_decl_stmt.initializer, depth + 2);
            }
            break;

        case AST_PARAMETER:
            printf("Parameter '%.*s'\n", (int)node->parameter.name.length,
                   node->parameter.name.start);
            if (node->parameter.type_annotation) {
                arc_ast_node_print(node->parameter.type_annotation, depth + 1);
            }
            break;

        case AST_STMT_BLOCK:
            printf("Block (%zu statements)\n", node->block_stmt.statement_count);
            for (size_t i = 0; i < node->block_stmt.statement_count; i++) {
                arc_ast_node_print(node->block_stmt.statements[i], depth + 1);
            }
            break;

        case AST_STMT_EXPRESSION:
            printf("ExpressionStmt\n");
            arc_ast_node_print(node->expr_stmt.expression, depth + 1);
            break;
        case AST_STMT_IF:
            printf("IfStmt\n");
            print_indent(depth + 1);
            printf("Condition:\n");
            arc_ast_node_print(node->if_stmt.condition, depth + 2);
            print_indent(depth + 1);
            printf("Then:\n");
            arc_ast_node_print(node->if_stmt.then_branch, depth + 2);
            if (node->if_stmt.else_branch) {
                print_indent(depth + 1);
                printf("Else:\n");
                arc_ast_node_print(node->if_stmt.else_branch, depth + 2);
            }
            break;

        case AST_STMT_WHILE:
            printf("WhileStmt\n");
            print_indent(depth + 1);
            printf("Condition:\n");
            arc_ast_node_print(node->while_stmt.condition, depth + 2);
            print_indent(depth + 1);
            printf("Body:\n");
            arc_ast_node_print(node->while_stmt.body, depth + 2);
            break;

        case AST_STMT_RETURN:
            printf("ReturnStmt\n");
            if (node->return_stmt.value) {
                arc_ast_node_print(node->return_stmt.value, depth + 1);
            }
            break;

        case AST_EXPR_BINARY:
            printf("BinaryExpr (op=%d)\n", node->binary_expr.op_type);
            print_indent(depth + 1);
            printf("Left:\n");
            arc_ast_node_print(node->binary_expr.left, depth + 2);
            print_indent(depth + 1);
            printf("Right:\n");
            arc_ast_node_print(node->binary_expr.right, depth + 2);
            break;

        case AST_EXPR_UNARY:
            printf("UnaryExpr (op=%d)\n", node->unary_expr.op_type);
            arc_ast_node_print(node->unary_expr.operand, depth + 1);
            break;

        case AST_EXPR_CALL:
            printf("CallExpr (%zu args)\n", node->call_expr.argument_count);
            print_indent(depth + 1);
            printf("Function:\n");
            arc_ast_node_print(node->call_expr.function, depth + 2);
            for (size_t i = 0; i < node->call_expr.argument_count; i++) {
                print_indent(depth + 1);
                printf("Arg %zu:\n", i);
                arc_ast_node_print(node->call_expr.arguments[i], depth + 2);
            }
            break;

        case AST_IDENTIFIER:
            printf("Identifier '%.*s'\n", (int)node->identifier.token.length,
                   node->identifier.token.start);
            break;

        case AST_LITERAL_INT:
            printf("IntLiteral '%.*s'\n", (int)node->literal_int.token.length,
                   node->literal_int.token.start);
            break;

        case AST_LITERAL_FLOAT:
            printf("FloatLiteral '%.*s'\n", (int)node->literal_float.token.length,
                   node->literal_float.token.start);
            break;

        case AST_LITERAL_STRING:
            printf("StringLiteral '%.*s'\n", (int)node->literal_string.token.length,
                   node->literal_string.token.start);
            break;

        case AST_LITERAL_BOOL:
            printf("BoolLiteral '%.*s'\n", (int)node->literal_bool.token.length,
                   node->literal_bool.token.start);
            break;

        case AST_TYPE_PRIMITIVE:
            printf("PrimitiveType '%.*s'\n", (int)node->type_primitive.token.length,
                   node->type_primitive.token.start);
            break;

        case AST_STMT_FOR:
            printf("ForStmt '%.*s' in\n", (int)node->for_stmt.iterator.length,
                   node->for_stmt.iterator.start);
            print_indent(depth + 1);
            printf("Iterable:\n");
            arc_ast_node_print(node->for_stmt.iterable, depth + 2);
            print_indent(depth + 1);
            printf("Body:\n");
            arc_ast_node_print(node->for_stmt.body, depth + 2);
            break;

        case AST_STMT_BREAK:
            printf("BreakStmt\n");
            break;

        case AST_STMT_CONTINUE:
            printf("ContinueStmt\n");
            break;

        case AST_EXPR_ARRAY_LITERAL:
            printf("ArrayLiteral (%zu elements)\n", node->array_literal_expr.element_count);
            for (size_t i = 0; i < node->array_literal_expr.element_count; i++) {
                print_indent(depth + 1);
                printf("Element %zu:\n", i);
                arc_ast_node_print(node->array_literal_expr.elements[i], depth + 2);
            }
            break;

        case AST_EXPR_INDEX:
            printf("IndexExpr\n");
            print_indent(depth + 1);
            printf("Object:\n");
            arc_ast_node_print(node->index_expr.object, depth + 2);
            print_indent(depth + 1);
            printf("Index:\n");
            arc_ast_node_print(node->index_expr.index, depth + 2);
            break;

        case AST_EXPR_FIELD_ACCESS:
            printf("FieldAccess .%.*s\n", (int)node->field_access_expr.field_name.length,
                   node->field_access_expr.field_name.start);
            print_indent(depth + 1);
            printf("Object:\n");
            arc_ast_node_print(node->field_access_expr.object, depth + 2);
            break;

        case AST_LITERAL_CHAR:
            printf("CharLiteral '%.*s'\n", (int)node->literal_char.token.length,
                   node->literal_char.token.start);
            break;

        case AST_LITERAL_NULL:
            printf("NullLiteral\n");
            break;

        case AST_TYPE_POINTER:
            printf("PointerType\n");
            arc_ast_node_print(node->type_pointer.pointed_type, depth + 1);
            break;

        case AST_TYPE_ARRAY:
            printf("ArrayType\n");
            print_indent(depth + 1);
            printf("Size:\n");
            arc_ast_node_print(node->type_array.size_expr, depth + 2);
            print_indent(depth + 1);
            printf("ElementType:\n");
            arc_ast_node_print(node->type_array.element_type, depth + 2);
            break;
        case AST_TYPE_SLICE:
            printf("SliceType\n");
            arc_ast_node_print(node->type_slice.element_type, depth + 1);
            break;

        case AST_TYPE_OPTIONAL:
            printf("OptionalType\n");
            arc_ast_node_print(node->type_optional.inner_type, depth + 1);
            break;

        case AST_TYPE_FUNCTION:
            printf("FunctionType\n");
            print_indent(depth + 1);
            printf("Parameters:\n");
            for (size_t i = 0; i < node->type_function.parameter_count; i++) {
                arc_ast_node_print(node->type_function.parameter_types[i], depth + 2);
            }
            if (node->type_function.return_type) {
                print_indent(depth + 1);
                printf("ReturnType:\n");
                arc_ast_node_print(node->type_function.return_type, depth + 2);
            }
            break;

        default:
            printf("Unknown node type: %d\n", node->type);
            break;
    }
}

static ArcAstNode *parse_scope_resolution(ArcParser *parser, ArcAstNode *left) {
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);
    ArcToken scope_token = parser->previous_token;  // '::' was already consumed

    ArcToken right_name = consume(parser, TOKEN_IDENTIFIER, "Expected identifier after '::'");

    ArcAstNode *node = arc_ast_node_create(parser, AST_EXPR_BINARY, source_info);
    if (node) {
        node->binary_expr.left = left;
        node->binary_expr.right = NULL;  // Could be a simple identifier or more complex
        node->binary_expr.op_type = BINARY_OP_SCOPE_RESOLUTION;
        node->binary_expr.operator_token = scope_token;
        // Create a simple identifier node for the right side
        ArcAstNode *right_node = arc_ast_node_create(parser, AST_IDENTIFIER, source_info);
        if (right_node) {
            right_node->identifier.token = right_name;
            node->binary_expr.right = right_node;
        }
    }
    return node;
}
