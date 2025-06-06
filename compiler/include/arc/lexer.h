// compiler/include/arc/lexer.h
#ifndef ARC_LEXER_H
#define ARC_LEXER_H

#include "arc/common.h"  // Includes ArcSourceLocation, ArcResult, etc.

typedef enum {
    // Single-character tokens
    TOKEN_LPAREN,
    TOKEN_RPAREN,  // ( )
    TOKEN_LBRACE,
    TOKEN_RBRACE,  // { }
    TOKEN_LBRACKET,
    TOKEN_RBRACKET,  // [ ]
    TOKEN_COMMA,
    TOKEN_DOT,  // , .
    TOKEN_MINUS,
    TOKEN_PLUS,  // - +
    TOKEN_SEMICOLON,
    TOKEN_SLASH,  // ; /
    TOKEN_ASTERISK,
    TOKEN_PERCENT,  // * %

    // One or two character tokens
    TOKEN_BANG,
    TOKEN_BANG_EQUAL,  // ! !=
    TOKEN_EQUAL,
    TOKEN_EQUAL_EQUAL,  // = ==
    TOKEN_GREATER,
    TOKEN_GREATER_EQUAL,  // > >=
    TOKEN_LESS,
    TOKEN_LESS_EQUAL,  // < <=
    TOKEN_PIPE,
    TOKEN_PIPE_PIPE,  // || (logical OR - legacy)
    TOKEN_AMPERSAND,
    TOKEN_AMPERSAND_AMPERSAND,  // && (logical AND - legacy)
    TOKEN_AND,                  // and (logical AND)
    TOKEN_OR,                   // or (logical OR)
    TOKEN_NOT,                  // not (logical NOT)
    TOKEN_CARET,                // ^ (bitwise XOR, or pointer type)
    TOKEN_QUESTION,             // ? (optional types/pointers)
    TOKEN_COLON,                // :
    TOKEN_DOUBLE_COLON,         // :: (module path separator)
    TOKEN_ARROW,                // -> (function return, pipeline)
    TOKEN_PIPELINE,             // |> (pipeline operator)
    TOKEN_ASYNC_PIPELINE,       // ~> (async pipeline)
    TOKEN_REVERSE_PIPELINE,     // <| (reverse pipeline)
    TOKEN_COMPOSITION,          // |< (composition)
    TOKEN_FUNCTION_ARROW,       // => (function arrow)
    TOKEN_NULL_COALESCING,      // ?? (null coalescing)
    TOKEN_FORCE_UNWRAP,         // !! (force unwrap)
    TOKEN_SPACESHIP,            // <=> (spaceship operator)
    TOKEN_POWER,                // ** (power operator)
    TOKEN_DOUBLE_AT,            // @@ (attribute application)
    TOKEN_DOT_DOT,              // .. (range)
    TOKEN_DOT_DOT_DOT,          // ... (variadic/spread)
    TOKEN_LEFT_SHIFT,           // <<
    TOKEN_RIGHT_SHIFT,          // >>
    TOKEN_AT,                   // @ (attribute marker)
    TOKEN_HASH,                 // # (hash/pound)
    TOKEN_TILDE,                // ~ (bitwise NOT)    // Compound assignment operators
    TOKEN_WALRUS,               // := (walrus operator)
    TOKEN_PLUS_EQUAL,           // +=
    TOKEN_MINUS_EQUAL,          // -=
    TOKEN_ASTERISK_EQUAL,       // *=
    TOKEN_SLASH_EQUAL,          // /=
    TOKEN_PERCENT_EQUAL,        // %=
    TOKEN_POWER_EQUAL,          // **=
    TOKEN_AMPERSAND_EQUAL,      // &=
    TOKEN_PIPE_EQUAL,           // |=
    TOKEN_CARET_EQUAL,          // ^=
    TOKEN_LEFT_SHIFT_EQUAL,     // <<=
    TOKEN_RIGHT_SHIFT_EQUAL,    // >>=

    // Literals
    TOKEN_IDENTIFIER,
    TOKEN_STRING_LITERAL,
    TOKEN_NUMBER_INT,    // For now, just integers
    TOKEN_NUMBER_FLOAT,  // Add later if needed
    TOKEN_CHAR_LITERAL,  // Keywords (add all keywords from your Arc specification)
    TOKEN_KEYWORD_PUB,
    TOKEN_KEYWORD_MOD,
    TOKEN_KEYWORD_USE,
    TOKEN_KEYWORD_TYPE,
    TOKEN_KEYWORD_STRUCT,
    TOKEN_KEYWORD_ENUM,
    TOKEN_KEYWORD_INTERFACE,
    TOKEN_KEYWORD_IMPL,
    TOKEN_KEYWORD_FUNC,
    TOKEN_KEYWORD_CONST,
    TOKEN_KEYWORD_LET,
    TOKEN_KEYWORD_MUT,
    TOKEN_KEYWORD_IF,
    TOKEN_KEYWORD_ELIF,
    TOKEN_KEYWORD_ELSE,
    TOKEN_KEYWORD_WHILE,
    TOKEN_KEYWORD_FOR,
    TOKEN_KEYWORD_IN,
    TOKEN_KEYWORD_MATCH,
    TOKEN_KEYWORD_WHEN,
    TOKEN_KEYWORD_BREAK,
    TOKEN_KEYWORD_CONTINUE,
    TOKEN_KEYWORD_RETURN,
    TOKEN_KEYWORD_YIELD,
    TOKEN_KEYWORD_AND,  // Logical and operator
    TOKEN_KEYWORD_OR,   // Logical or operator
    TOKEN_KEYWORD_NOT,  // Logical not operator
    TOKEN_KEYWORD_DEFER,
    TOKEN_KEYWORD_COMPTIME,
    TOKEN_KEYWORD_STREAM,
    TOKEN_KEYWORD_PHANTOM,
    TOKEN_KEYWORD_CONTEXT,
    TOKEN_KEYWORD_USING,
    TOKEN_KEYWORD_WITH,
    TOKEN_KEYWORD_GRANT,
    TOKEN_KEYWORD_REVOKE,
    TOKEN_KEYWORD_PIPELINE,
    TOKEN_KEYWORD_ASYNC,
    TOKEN_KEYWORD_AWAIT,
    TOKEN_KEYWORD_SYNC,
    TOKEN_KEYWORD_CAPABILITY,
    TOKEN_KEYWORD_PHANTOM_RESOURCE,
    TOKEN_KEYWORD_TRUE,
    TOKEN_KEYWORD_FALSE,
    TOKEN_KEYWORD_NIL,
    TOKEN_KEYWORD_VOID,
    TOKEN_KEYWORD_WITH_CONTEXT,  // For context injection (deprecated)
    TOKEN_KEYWORD_EXTERN,        // For C FFI
    TOKEN_KEYWORD_EXPORT,        // For exporting to C
    TOKEN_KEYWORD_INLINE,        // For inline functions
    TOKEN_KEYWORD_UNION,         // Union types
    TOKEN_KEYWORD_ORELSE,        // For optional handling
    TOKEN_KEYWORD_CATCH,         // For error handling
    TOKEN_KEYWORD_TRY,           // For error propagation
    TOKEN_KEYWORD_GUARD,         // Guard statements
    TOKEN_KEYWORD_THEN,          // Expression conditions

    // Primitive type keywords
    TOKEN_KEYWORD_I8,
    TOKEN_KEYWORD_I16,
    TOKEN_KEYWORD_I32,
    TOKEN_KEYWORD_I64,
    TOKEN_KEYWORD_ISIZE,
    TOKEN_KEYWORD_U8,
    TOKEN_KEYWORD_U16,
    TOKEN_KEYWORD_U32,
    TOKEN_KEYWORD_U64,
    TOKEN_KEYWORD_USIZE,
    TOKEN_KEYWORD_F32,
    TOKEN_KEYWORD_F64,
    TOKEN_KEYWORD_BOOL,
    TOKEN_KEYWORD_CHAR,

    // Legacy tokens (for backward compatibility)

    // Special tokens
    TOKEN_COMMENT,     // Usually skipped, but can be useful for tools
    TOKEN_WHITESPACE,  // Usually skipped
    TOKEN_NEWLINE,     // Sometimes significant or useful for error reporting
    TOKEN_ERROR,       // Represents a lexing error
    TOKEN_EOF          // End Of File
} ArcTokenType;

typedef struct {
    ArcTokenType type;
    ArcSourceLocation loc;  // From common.h (filename, line, col, offset)
    const char *start;      // Pointer to the beginning of the lexeme in the source
    size_t length;          // Length of the lexeme

    union {
        long long int_val;
        double float_val;
        char *string_val;  // Owned by the token, needs freeing
        char char_val;
    } value;
} ArcToken;

typedef struct {
    const char *filename;      // Name of the file being lexed (for ArcSourceLocation)
    const char *source_start;  // Pointer to the beginning of the entire source string
    const char *source_end;    // Pointer to one char *past* the end of the source string
    const char *current_char;  // Pointer to the current character being processed

    size_t current_line;             // Current line number (1-based)
    const char *current_line_start;  // Pointer to the start of the current line in the source
                                     // (used for calculating column number)
} ArcLexer;

// --- Lexer Public API ---

// Initializes the lexer with the given source code and filename.
// Source must remain valid for the lifetime of the lexer.
// Filename can be NULL if lexing from a string without a file context.
void arc_lexer_init(ArcLexer *lexer, const char *source_code, const char *filename);

// Scans and returns the next token from the source.
// The caller should check token.type for TOKEN_EOF or TOKEN_ERROR.
ArcToken arc_lexer_next_token(ArcLexer *lexer);

// Helper to convert token type to string (for debugging)
const char *arc_token_type_to_string(ArcTokenType type);

#endif  // ARC_LEXER_H
