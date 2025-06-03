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
// static ArcAstNode *parse_declaration(ArcParser *parser);
// static ArcAstNode *parse_statement(ArcParser *parser);
static ArcAstNode *parse_expression(ArcParser *parser);
static ArcAstNode *parse_primary(ArcParser *parser);
// static ArcAstNode *parse_type(ArcParser *parser);

// Utility functions
static void advance(ArcParser *parser);
static bool check(const ArcParser *parser, ArcTokenType type);
static bool match(ArcParser *parser, ArcTokenType type);
static ArcToken consume(ArcParser *parser, ArcTokenType type, const char *message);
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

    while (parser->current_token.type != TOKEN_EOF) {
        if (parser->previous_token.type == TOKEN_SEMICOLON) {
            parser->synchronizing = false;
            return;
        }

        switch (parser->current_token.type) {
            case TOKEN_KEYWORD_FN:
            case TOKEN_KEYWORD_STRUCT:
            case TOKEN_KEYWORD_ENUM:
            case TOKEN_KEYWORD_TYPE:
            case TOKEN_KEYWORD_INTERFACE:
            case TOKEN_KEYWORD_IMPL:
            case TOKEN_KEYWORD_VAR:
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

// === PARSING FUNCTIONS ===

// Parse binary operator precedence
typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,   // =, +=, -=, etc.
    PREC_OR,           // ||
    PREC_AND,          // &&
    PREC_EQUALITY,     // ==, !=
    PREC_COMPARISON,   // <, >, <=, >=
    PREC_BITWISE_OR,   // |
    PREC_BITWISE_XOR,  // ^
    PREC_BITWISE_AND,  // &
    PREC_SHIFT,        // <<, >>
    PREC_TERM,         // +, -
    PREC_FACTOR,       // *, /, %
    PREC_UNARY,        // !, -, &, *
    PREC_CALL,         // (), [], .
    PREC_PRIMARY
} Precedence;

static Precedence get_token_precedence(ArcTokenType type) {
    switch (type) {
        case TOKEN_EQUAL:
        case TOKEN_PLUS_EQUAL:
        case TOKEN_MINUS_EQUAL:
        case TOKEN_ASTERISK_EQUAL:
        case TOKEN_SLASH_EQUAL:
        case TOKEN_PERCENT_EQUAL:
            return PREC_ASSIGNMENT;
        case TOKEN_PIPE_PIPE:
            return PREC_OR;
        case TOKEN_AMPERSAND_AMPERSAND:
            return PREC_AND;
        case TOKEN_EQUAL_EQUAL:
        case TOKEN_BANG_EQUAL:
            return PREC_EQUALITY;
        case TOKEN_LESS:
        case TOKEN_LESS_EQUAL:
        case TOKEN_GREATER:
        case TOKEN_GREATER_EQUAL:
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
        case TOKEN_LPAREN:
        case TOKEN_LBRACKET:
        case TOKEN_DOT:
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
        case TOKEN_AMPERSAND_AMPERSAND:
            return BINARY_OP_AND;
        case TOKEN_PIPE_PIPE:
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
        case TOKEN_EQUAL:
            return BINARY_OP_ASSIGN;
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

        case TOKEN_KEYWORD_NULL: {
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
    ArcToken lparen_token = parser->current_token;
    advance(parser);  // consume '('

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
            }
        } while (match(parser, TOKEN_COMMA));
    }

    consume(parser, TOKEN_RPAREN, "Expected ')' after function arguments");

    // Assign to node
    node->call_expr.argument_count = argument_count;
    node->call_expr.arguments = arguments;
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
        match(parser, TOKEN_ASTERISK)) {

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

    ArcAstNode *primary = parse_primary(parser);

    // Handle postfix operators (call, index, field access)
    while (true) {
        if (match(parser, TOKEN_LPAREN)) {
            primary = parse_call(parser, primary);
        } else if (match(parser, TOKEN_LBRACKET)) {
            primary = parse_index(parser, primary);
        } else if (match(parser, TOKEN_DOT)) {
            primary = parse_field_access(parser, primary);
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
/*static ArcAstNode *parse_type(ArcParser *parser) {
    ArcSourceInfo source_info = arc_parser_current_source_info(parser);

    switch (parser->current_token.type) {
        case TOKEN_IDENTIFIER: {
            // Handle built-in type names and user-defined types as identifiers
            ArcToken token = parser->current_token;
            advance(parser);

            ArcAstNode *node = arc_ast_node_create(parser, AST_TYPE_PRIMITIVE, source_info);
            if (node) {
                node->type_primitive.primitive_type = TOKEN_IDENTIFIER;
                node->type_primitive.token = token;
            }
            return node;
        }

        case TOKEN_CARET: {
            // Pointer type: ^Type
            ArcToken caret_token = parser->current_token;
            advance(parser);

            ArcAstNode *pointed_type = parse_type(parser);

            ArcAstNode *node = arc_ast_node_create(parser, AST_TYPE_POINTER, source_info);
            if (node) {
                node->type_pointer.pointed_type = pointed_type;
                node->type_pointer.caret_token = caret_token;
            }
            return node;
        }

        case TOKEN_LBRACKET: {
            // Array or slice type: [Type] or [size]Type
            ArcToken lbracket_token = parser->current_token;
            advance(parser);

            if (check(parser, TOKEN_RBRACKET)) {
                // Slice type: [Type]
                advance(parser);  // consume ']'
                ArcAstNode *element_type = parse_type(parser);

                ArcAstNode *node = arc_ast_node_create(parser, AST_TYPE_SLICE, source_info);
                if (node) {
                    node->type_slice.element_type = element_type;
                    node->type_slice.lbracket_token = lbracket_token;
                }
                return node;
            } else {
                // Array type: [size]Type
                ArcAstNode *size_expr = parse_expression(parser);
                consume(parser, TOKEN_RBRACKET, "Expected ']' after array size");
                ArcAstNode *element_type = parse_type(parser);

                ArcAstNode *node = arc_ast_node_create(parser, AST_TYPE_ARRAY, source_info);
                if (node) {
                    node->type_array.element_type = element_type;
                    node->type_array.size_expr = size_expr;
                    node->type_array.lbracket_token = lbracket_token;
                }
                return node;
            }
        }

        default:
            arc_parser_error(parser, "Expected type annotation");
            return NULL;
    }
}*/

// Continue in next part due to length...
