#include "arc/lexer.h"
#include <ctype.h>
#include <string.h>

// Forward declarations
static ArcTokenType check_keyword(const char *start, size_t length);

// ----- internal helper functions -----
static bool is_at_end(const ArcLexer *lexer) {
    return lexer->current_char >= lexer->source_end || *lexer->current_char == '\0';
}

static char advance(ArcLexer *lexer) {
    if (is_at_end(lexer))
        return '\0';

    char current_c = *lexer->current_char;
    lexer->current_char++;

    if (current_c == '\n') {
        lexer->current_line++;
        lexer->current_line_start = lexer->current_char;
    }

    return current_c;
}

static char peek(const ArcLexer *lexer) {
    if (is_at_end(lexer)) {
        return '\0';
    }

    return *lexer->current_char;
}

static char peek_next(const ArcLexer *lexer) {
    if (is_at_end(lexer) || (lexer->current_char + 1) >= lexer->source_end) {
        return '\0';
    }
    return *(lexer->current_char + 1);
}

static ArcToken make_token(ArcLexer *lexer, ArcTokenType type, const char *start_of_lexeme,
                           ArcSourceLocation start_loc) {
    ArcToken token;
    token.type = type;
    token.loc = start_loc;  // Use the pre-calculated start location
    token.start = start_of_lexeme;
    token.length = (size_t)(lexer->current_char - start_of_lexeme);

    // Initialize the value union to a known state (e.g., zero)
    // This is good practice, especially if not all token types use the union.
    memset(&token.value, 0, sizeof(token.value));

    return token;
}

// You might also want a specific error token helper
static ArcToken make_error_token(
    ArcLexer *lexer, const char *error_lexeme_start, ArcSourceLocation error_loc,
    const char *error_message_for_reporting) {  // Report the error using your common.h function
    arc_report_error(&error_loc, "%s",
                     error_message_for_reporting);  // Or your preferred error reporting

    ArcToken token;
    token.type = TOKEN_ERROR;
    token.loc = error_loc;
    token.start = error_lexeme_start;
    // Length could be 1 for a single bad char, or more if it's a sequence
    token.length = (lexer->current_char > error_lexeme_start)
                       ? (size_t)(lexer->current_char - error_lexeme_start)
                       : 1;
    if (token.length == 0 && !is_at_end(lexer))
        token.length = 1;  // Ensure error token has some span if not EOF

    memset(&token.value, 0, sizeof(token.value));
    return token;
}

void arc_lexer_init(ArcLexer *lexer, const char *source_code, const char *filename) {
    ARC_ASSERT(lexer != NULL, "Lexer pointer cannot be NULL.");
    ARC_ASSERT(source_code != NULL, "Source code cannot be NULL.");

    lexer->filename = filename;  // Can be NULL
    lexer->source_start = source_code;
    lexer->current_char = source_code;
    lexer->source_end = source_code + strlen(source_code);  // Points one char *past* the end

    lexer->current_line = 1;
    lexer->current_line_start = source_code;  // Start of the first line is start of source
}

// Helper to skip whitespace (but preserve newlines for some use cases)
static void skip_whitespace(ArcLexer *lexer) {
    while (!is_at_end(lexer)) {
        char c = peek(lexer);
        if (c == ' ' || c == '\r' || c == '\t') {
            advance(lexer);
        } else {
            break;
        }
    }
}

// Helper to calculate current source location
static ArcSourceLocation get_current_location(const ArcLexer *lexer) {
    ArcSourceLocation loc;
    loc.filename = lexer->filename;
    loc.line = lexer->current_line;
    loc.column = (size_t)(lexer->current_char - lexer->current_line_start) + 1;
    loc.offset = (size_t)(lexer->current_char - lexer->source_start);
    return loc;
}

// Main lexer function
ArcToken arc_lexer_next_token(ArcLexer *lexer) {
    ARC_ASSERT(lexer != NULL, "Lexer cannot be NULL");

    // Skip whitespace
    skip_whitespace(lexer);

    // Check for end of file
    if (is_at_end(lexer)) {
        ArcSourceLocation loc = get_current_location(lexer);
        return make_token(lexer, TOKEN_EOF, lexer->current_char, loc);
    }

    // Record start position for this token
    const char *token_start = lexer->current_char;
    ArcSourceLocation token_loc = get_current_location(lexer);

    char c = advance(lexer);

    // Handle single-character tokens
    switch (c) {
        case '(':
            return make_token(lexer, TOKEN_LPAREN, token_start, token_loc);
        case ')':
            return make_token(lexer, TOKEN_RPAREN, token_start, token_loc);
        case '{':
            return make_token(lexer, TOKEN_LBRACE, token_start, token_loc);
        case '}':
            return make_token(lexer, TOKEN_RBRACE, token_start, token_loc);
        case '[':
            return make_token(lexer, TOKEN_LBRACKET, token_start, token_loc);
        case ']':
            return make_token(lexer, TOKEN_RBRACKET, token_start, token_loc);
        case ',':
            return make_token(lexer, TOKEN_COMMA, token_start, token_loc);
        case ';':
            return make_token(lexer, TOKEN_SEMICOLON, token_start, token_loc);
        case '*':
            if (peek(lexer) == '=') {
                advance(lexer);
                return make_token(lexer, TOKEN_ASTERISK_EQUAL, token_start, token_loc);
            } else if (peek(lexer) == '*') {
                advance(lexer);
                if (peek(lexer) == '=') {
                    advance(lexer);
                    return make_token(lexer, TOKEN_POWER_EQUAL, token_start, token_loc);
                }
                return make_token(lexer, TOKEN_POWER, token_start, token_loc);
            }
            return make_token(lexer, TOKEN_ASTERISK, token_start, token_loc);
        case '%':
            if (peek(lexer) == '=') {
                advance(lexer);
                return make_token(lexer, TOKEN_PERCENT_EQUAL, token_start, token_loc);
            }
            return make_token(lexer, TOKEN_PERCENT, token_start, token_loc);
        case '?':
            if (peek(lexer) == '?') {
                advance(lexer);
                return make_token(lexer, TOKEN_NULL_COALESCING, token_start, token_loc);
            }
            return make_token(lexer, TOKEN_QUESTION, token_start, token_loc);
        case ':':
            if (peek(lexer) == ':') {
                advance(lexer);  // consume second ':'
                return make_token(lexer, TOKEN_DOUBLE_COLON, token_start, token_loc);
            } else if (peek(lexer) == '=') {
                advance(lexer);
                return make_token(lexer, TOKEN_WALRUS, token_start, token_loc);
            }
            return make_token(lexer, TOKEN_COLON, token_start, token_loc);
        case '@':
            if (peek(lexer) == '@') {
                advance(lexer);
                return make_token(lexer, TOKEN_DOUBLE_AT, token_start, token_loc);
            }
            return make_token(lexer, TOKEN_AT, token_start, token_loc);
        case '#':
            return make_token(lexer, TOKEN_HASH, token_start, token_loc);
        case '~':
            if (peek(lexer) == '>') {
                advance(lexer);
                return make_token(lexer, TOKEN_ASYNC_PIPELINE, token_start, token_loc);
            }
            return make_token(lexer, TOKEN_TILDE, token_start, token_loc);
        case '\n':
            return make_token(lexer, TOKEN_NEWLINE, token_start, token_loc);

            // Handle two-character tokens and single-character alternatives        case '!':
            if (peek(lexer) == '=') {
                advance(lexer);
                return make_token(lexer, TOKEN_BANG_EQUAL, token_start, token_loc);
            } else if (peek(lexer) == '!') {
                advance(lexer);
                return make_token(lexer, TOKEN_FORCE_UNWRAP, token_start, token_loc);
            }
            return make_token(lexer, TOKEN_BANG, token_start, token_loc);
        case '=':
            if (peek(lexer) == '=') {
                advance(lexer);
                return make_token(lexer, TOKEN_EQUAL_EQUAL, token_start, token_loc);
            } else if (peek(lexer) == '>') {
                advance(lexer);
                return make_token(lexer, TOKEN_FUNCTION_ARROW, token_start, token_loc);
            }
            return make_token(lexer, TOKEN_EQUAL, token_start, token_loc);
        case '<':
            if (peek(lexer) == '=') {
                advance(lexer);
                if (peek(lexer) == '>') {
                    advance(lexer);
                    return make_token(lexer, TOKEN_SPACESHIP, token_start, token_loc);
                }
                return make_token(lexer, TOKEN_LESS_EQUAL, token_start, token_loc);
            } else if (peek(lexer) == '<') {
                advance(lexer);
                if (peek(lexer) == '=') {
                    advance(lexer);
                    return make_token(lexer, TOKEN_LEFT_SHIFT_EQUAL, token_start, token_loc);
                }
                return make_token(lexer, TOKEN_LEFT_SHIFT, token_start, token_loc);
            } else if (peek(lexer) == '|') {
                advance(lexer);
                return make_token(lexer, TOKEN_REVERSE_PIPELINE, token_start, token_loc);
            }
            return make_token(lexer, TOKEN_LESS, token_start, token_loc);
        case '>':
            if (peek(lexer) == '=') {
                advance(lexer);
                return make_token(lexer, TOKEN_GREATER_EQUAL, token_start, token_loc);
            } else if (peek(lexer) == '>') {
                advance(lexer);
                if (peek(lexer) == '=') {
                    advance(lexer);
                    return make_token(lexer, TOKEN_RIGHT_SHIFT_EQUAL, token_start, token_loc);
                }
                return make_token(lexer, TOKEN_RIGHT_SHIFT, token_start, token_loc);
            }
            return make_token(lexer, TOKEN_GREATER, token_start, token_loc);

        case '+':
            if (peek(lexer) == '=') {
                advance(lexer);
                return make_token(lexer, TOKEN_PLUS_EQUAL, token_start, token_loc);
            }
            return make_token(lexer, TOKEN_PLUS, token_start, token_loc);

        case '-':
            if (peek(lexer) == '=') {
                advance(lexer);
                return make_token(lexer, TOKEN_MINUS_EQUAL, token_start, token_loc);
            } else if (peek(lexer) == '>') {
                advance(lexer);
                return make_token(lexer, TOKEN_ARROW, token_start, token_loc);
            }
            return make_token(lexer, TOKEN_MINUS, token_start, token_loc);

        case '.':
            if (peek(lexer) == '.') {
                advance(lexer);
                if (peek(lexer) == '.') {
                    advance(lexer);
                    return make_token(lexer, TOKEN_DOT_DOT_DOT, token_start, token_loc);
                }
                return make_token(lexer, TOKEN_DOT_DOT, token_start, token_loc);
            }
            return make_token(lexer, TOKEN_DOT, token_start, token_loc);
        case '/':
            if (peek(lexer) == '=') {
                advance(lexer);
                return make_token(lexer, TOKEN_SLASH_EQUAL, token_start, token_loc);
            } else if (peek(lexer) == '/') {
                // Line comment - skip to end of line and continue tokenizing
                advance(lexer);  // Skip second '/'
                while (peek(lexer) != '\n' && !is_at_end(lexer)) {
                    advance(lexer);
                }
                // Skip the comment and get the next token
                return arc_lexer_next_token(lexer);
            } else if (peek(lexer) == '*') {
                // Block comment - skip to */ and continue tokenizing
                advance(lexer);  // Skip '*'
                bool found_end = false;
                while (!is_at_end(lexer)) {
                    if (peek(lexer) == '*' && peek_next(lexer) == '/') {
                        advance(lexer);  // Skip '*'
                        advance(lexer);  // Skip '/'
                        found_end = true;
                        break;
                    }
                    advance(lexer);
                }

                if (!found_end) {
                    return make_error_token(lexer, token_start, token_loc,
                                            "Unterminated block comment");
                }

                // Skip the comment and get the next token
                return arc_lexer_next_token(lexer);
            }
            return make_token(lexer, TOKEN_SLASH, token_start, token_loc);
        case '|':
            if (peek(lexer) == '|') {
                advance(lexer);
                return make_token(lexer, TOKEN_PIPE_PIPE, token_start, token_loc);
            } else if (peek(lexer) == '>') {
                advance(lexer);
                return make_token(lexer, TOKEN_PIPELINE, token_start, token_loc);
            } else if (peek(lexer) == '=') {
                advance(lexer);
                return make_token(lexer, TOKEN_PIPE_EQUAL, token_start, token_loc);
            } else if (peek(lexer) == '<') {
                advance(lexer);
                return make_token(lexer, TOKEN_COMPOSITION, token_start, token_loc);
            }
            return make_token(lexer, TOKEN_PIPE, token_start, token_loc);

        case '&':
            if (peek(lexer) == '&') {
                advance(lexer);
                return make_token(lexer, TOKEN_AMPERSAND_AMPERSAND, token_start, token_loc);
            } else if (peek(lexer) == '=') {
                advance(lexer);
                return make_token(lexer, TOKEN_AMPERSAND_EQUAL, token_start, token_loc);
            }
            return make_token(lexer, TOKEN_AMPERSAND, token_start, token_loc);

        case '^':
            if (peek(lexer) == '=') {
                advance(lexer);
                return make_token(lexer, TOKEN_CARET_EQUAL, token_start, token_loc);
            }
            return make_token(lexer, TOKEN_CARET, token_start, token_loc);
    }

    // Handle identifiers and keywords
    if (isalpha(c) || c == '_') {
        while (isalnum(peek(lexer)) || peek(lexer) == '_') {
            advance(lexer);
        }

        // Check if it's a keyword
        size_t length = (size_t)(lexer->current_char - token_start);
        ArcTokenType keyword_type = check_keyword(token_start, length);
        if (keyword_type != TOKEN_IDENTIFIER) {
            return make_token(lexer, keyword_type, token_start, token_loc);
        }

        return make_token(lexer, TOKEN_IDENTIFIER, token_start, token_loc);
    }

    // Handle numbers
    if (isdigit(c)) {
        // Check for different number bases
        if (c == '0' && !is_at_end(lexer)) {
            char next = peek(lexer);
            if (next == 'x' || next == 'X') {
                // Hexadecimal
                advance(lexer);  // Skip 'x'
                while (isxdigit(peek(lexer))) {
                    advance(lexer);
                }
                return make_token(lexer, TOKEN_NUMBER_INT, token_start, token_loc);
            } else if (next == 'b' || next == 'B') {
                // Binary
                advance(lexer);  // Skip 'b'
                while (peek(lexer) == '0' || peek(lexer) == '1') {
                    advance(lexer);
                }
                return make_token(lexer, TOKEN_NUMBER_INT, token_start, token_loc);
            } else if (next == 'o' || next == 'O') {
                // Octal
                advance(lexer);  // Skip 'o'
                while (peek(lexer) >= '0' && peek(lexer) <= '7') {
                    advance(lexer);
                }
                return make_token(lexer, TOKEN_NUMBER_INT, token_start, token_loc);
            }
        }

        // Regular decimal number
        while (isdigit(peek(lexer))) {
            advance(lexer);
        }

        // Check for decimal point
        if (peek(lexer) == '.' && isdigit(peek_next(lexer))) {
            advance(lexer);  // Skip '.'
            while (isdigit(peek(lexer))) {
                advance(lexer);
            }

            // Check for scientific notation
            if (peek(lexer) == 'e' || peek(lexer) == 'E') {
                advance(lexer);  // Skip 'e'
                if (peek(lexer) == '+' || peek(lexer) == '-') {
                    advance(lexer);  // Skip sign
                }
                while (isdigit(peek(lexer))) {
                    advance(lexer);
                }
            }

            return make_token(lexer, TOKEN_NUMBER_FLOAT, token_start, token_loc);
        }

        // Check for scientific notation on integers
        if (peek(lexer) == 'e' || peek(lexer) == 'E') {
            advance(lexer);  // Skip 'e'
            if (peek(lexer) == '+' || peek(lexer) == '-') {
                advance(lexer);  // Skip sign
            }
            while (isdigit(peek(lexer))) {
                advance(lexer);
            }
            return make_token(lexer, TOKEN_NUMBER_FLOAT, token_start, token_loc);
        }

        return make_token(lexer, TOKEN_NUMBER_INT, token_start, token_loc);
    }

    // Handle string literals
    if (c == '"') {
        while (peek(lexer) != '"' && !is_at_end(lexer)) {
            if (peek(lexer) == '\\') {
                advance(lexer);  // Skip backslash
                if (!is_at_end(lexer)) {
                    char escaped_char = peek(lexer);
                    advance(lexer);  // Skip escaped character

                    // Handle special unicode escape sequences in strings
                    if (escaped_char == 'u') {
                        if (peek(lexer) == '{') {
                            advance(lexer);  // Skip opening brace
                            // Skip hex digits until closing brace
                            while (peek(lexer) != '}' && !is_at_end(lexer)) {
                                if (!isxdigit(peek(lexer))) {
                                    return make_error_token(
                                        lexer, token_start, token_loc,
                                        "Invalid unicode escape sequence in string");
                                }
                                advance(lexer);
                            }
                            if (peek(lexer) != '}') {
                                return make_error_token(
                                    lexer, token_start, token_loc,
                                    "Unterminated unicode escape sequence in string");
                            }
                            advance(lexer);  // Skip closing brace
                        }
                    }
                }
            } else {
                if (peek(lexer) == '\n')
                    lexer->current_line++;
                advance(lexer);
            }
        }

        if (is_at_end(lexer)) {
            return make_error_token(lexer, token_start, token_loc, "Unterminated string literal");
        }

        advance(lexer);  // Skip closing '"'
        return make_token(lexer, TOKEN_STRING_LITERAL, token_start, token_loc);
    }

    // Handle character literals
    if (c == '\'') {
        // Handle character content
        if (peek(lexer) == '\\') {
            advance(lexer);  // Skip backslash
            char escaped_char = peek(lexer);
            advance(lexer);  // Skip the escaped character

            // Handle special unicode escape sequences
            if (escaped_char == 'u') {
                if (peek(lexer) == '{') {
                    advance(lexer);  // Skip opening brace
                    // Skip hex digits until closing brace
                    while (peek(lexer) != '}' && !is_at_end(lexer)) {
                        if (!isxdigit(peek(lexer))) {
                            return make_error_token(lexer, token_start, token_loc,
                                                    "Invalid unicode escape sequence");
                        }
                        advance(lexer);
                    }
                    if (peek(lexer) != '}') {
                        return make_error_token(lexer, token_start, token_loc,
                                                "Unterminated unicode escape sequence");
                    }
                    advance(lexer);  // Skip closing brace
                }
            }
        } else {
            // Regular character
            advance(lexer);
        }

        if (peek(lexer) != '\'') {
            return make_error_token(lexer, token_start, token_loc,
                                    "Unterminated character literal");
        }

        advance(lexer);  // Skip closing '\''
        return make_token(lexer, TOKEN_CHAR_LITERAL, token_start, token_loc);
    }

    // Unknown character
    return make_error_token(lexer, token_start, token_loc, "Unexpected character");
}

// Helper function to check if an identifier is a keyword
static ArcTokenType check_keyword(const char *start, size_t length) {
    // Simple keyword lookup - you can optimize this later with a hash table
    struct {
        const char *keyword;
        ArcTokenType type;
    } keywords[] = {{"pub", TOKEN_KEYWORD_PUB},
                    {"mod", TOKEN_KEYWORD_MOD},
                    {"use", TOKEN_KEYWORD_USE},
                    {"type", TOKEN_KEYWORD_TYPE},
                    {"struct", TOKEN_KEYWORD_STRUCT},
                    {"enum", TOKEN_KEYWORD_ENUM},
                    {"interface", TOKEN_KEYWORD_INTERFACE},
                    {"impl", TOKEN_KEYWORD_IMPL},
                    {"func", TOKEN_KEYWORD_FUNC},
                    {"const", TOKEN_KEYWORD_CONST},
                    {"let", TOKEN_KEYWORD_LET},
                    {"mut", TOKEN_KEYWORD_MUT},
                    {"if", TOKEN_KEYWORD_IF},
                    {"elif", TOKEN_KEYWORD_ELIF},
                    {"else", TOKEN_KEYWORD_ELSE},
                    {"while", TOKEN_KEYWORD_WHILE},
                    {"for", TOKEN_KEYWORD_FOR},
                    {"in", TOKEN_KEYWORD_IN},
                    {"match", TOKEN_KEYWORD_MATCH},
                    {"when", TOKEN_KEYWORD_WHEN},
                    {"break", TOKEN_KEYWORD_BREAK},
                    {"continue", TOKEN_KEYWORD_CONTINUE},
                    {"return", TOKEN_KEYWORD_RETURN},
                    {"yield", TOKEN_KEYWORD_YIELD},
                    {"defer", TOKEN_KEYWORD_DEFER},
                    {"comptime", TOKEN_KEYWORD_COMPTIME},
                    {"stream", TOKEN_KEYWORD_STREAM},
                    {"phantom", TOKEN_KEYWORD_PHANTOM},
                    {"context", TOKEN_KEYWORD_CONTEXT},
                    {"using", TOKEN_KEYWORD_USING},
                    {"with", TOKEN_KEYWORD_WITH},
                    {"grant", TOKEN_KEYWORD_GRANT},
                    {"revoke", TOKEN_KEYWORD_REVOKE},
                    {"pipeline", TOKEN_KEYWORD_PIPELINE},
                    {"async", TOKEN_KEYWORD_ASYNC},
                    {"await", TOKEN_KEYWORD_AWAIT},
                    {"sync", TOKEN_KEYWORD_SYNC},
                    {"capability", TOKEN_KEYWORD_CAPABILITY},
                    {"phantom_resource", TOKEN_KEYWORD_PHANTOM_RESOURCE},
                    {"true", TOKEN_KEYWORD_TRUE},
                    {"false", TOKEN_KEYWORD_FALSE},
                    {"nil", TOKEN_KEYWORD_NIL},
                    {"void", TOKEN_KEYWORD_VOID},
                    {"with_context", TOKEN_KEYWORD_WITH_CONTEXT},
                    {"extern", TOKEN_KEYWORD_EXTERN},
                    {"export", TOKEN_KEYWORD_EXPORT},
                    {"inline", TOKEN_KEYWORD_INLINE},
                    {"union", TOKEN_KEYWORD_UNION},
                    {"orelse", TOKEN_KEYWORD_ORELSE},
                    {"catch", TOKEN_KEYWORD_CATCH},
                    {"try", TOKEN_KEYWORD_TRY},
                    {"guard", TOKEN_KEYWORD_GUARD},
                    {"then", TOKEN_KEYWORD_THEN},
                    {"and", TOKEN_AND},
                    {"or", TOKEN_OR},
                    {"not", TOKEN_NOT},
                    // Legacy keywords for backward compatibility
                    // Primitive types
                    {"i8", TOKEN_KEYWORD_I8},
                    {"i16", TOKEN_KEYWORD_I16},
                    {"i32", TOKEN_KEYWORD_I32},
                    {"i64", TOKEN_KEYWORD_I64},
                    {"isize", TOKEN_KEYWORD_ISIZE},
                    {"u8", TOKEN_KEYWORD_U8},
                    {"u16", TOKEN_KEYWORD_U16},
                    {"u32", TOKEN_KEYWORD_U32},
                    {"u64", TOKEN_KEYWORD_U64},
                    {"usize", TOKEN_KEYWORD_USIZE},
                    {"f32", TOKEN_KEYWORD_F32},
                    {"f64", TOKEN_KEYWORD_F64},
                    {"bool", TOKEN_KEYWORD_BOOL},
                    {"char", TOKEN_KEYWORD_CHAR},
                    {"void", TOKEN_KEYWORD_VOID}};

    for (size_t i = 0; i < sizeof(keywords) / sizeof(keywords[0]); i++) {
        if (strlen(keywords[i].keyword) == length &&
            strncmp(start, keywords[i].keyword, length) == 0) {
            return keywords[i].type;
        }
    }

    return TOKEN_IDENTIFIER;
}

// Convert token type to string for debugging
const char *arc_token_type_to_string(ArcTokenType type) {
    switch (type) {
        case TOKEN_LPAREN:
            return "LPAREN";
        case TOKEN_RPAREN:
            return "RPAREN";
        case TOKEN_LBRACE:
            return "LBRACE";
        case TOKEN_RBRACE:
            return "RBRACE";
        case TOKEN_LBRACKET:
            return "LBRACKET";
        case TOKEN_RBRACKET:
            return "RBRACKET";
        case TOKEN_COMMA:
            return "COMMA";
        case TOKEN_DOT:
            return "DOT";
        case TOKEN_DOT_DOT:
            return "DOT_DOT";
        case TOKEN_DOT_DOT_DOT:
            return "DOT_DOT_DOT";
        case TOKEN_MINUS:
            return "MINUS";
        case TOKEN_PLUS:
            return "PLUS";
        case TOKEN_SEMICOLON:
            return "SEMICOLON";
        case TOKEN_SLASH:
            return "SLASH";
        case TOKEN_ASTERISK:
            return "ASTERISK";
        case TOKEN_PERCENT:
            return "PERCENT";
        case TOKEN_CARET:
            return "CARET";
        case TOKEN_BANG:
            return "BANG";
        case TOKEN_BANG_EQUAL:
            return "BANG_EQUAL";
        case TOKEN_EQUAL:
            return "EQUAL";
        case TOKEN_EQUAL_EQUAL:
            return "EQUAL_EQUAL";
        case TOKEN_GREATER:
            return "GREATER";
        case TOKEN_GREATER_EQUAL:
            return "GREATER_EQUAL";
        case TOKEN_LESS:
            return "LESS";
        case TOKEN_LESS_EQUAL:
            return "LESS_EQUAL";
        case TOKEN_LEFT_SHIFT:
            return "LEFT_SHIFT";
        case TOKEN_RIGHT_SHIFT:
            return "RIGHT_SHIFT";
        case TOKEN_PIPELINE:
            return "PIPELINE";
        case TOKEN_PIPE:
            return "PIPE";
        case TOKEN_PIPE_PIPE:
            return "PIPE_PIPE";
        case TOKEN_PIPE_EQUAL:
            return "PIPE_EQUAL";
        case TOKEN_AMPERSAND:
            return "AMPERSAND";
        case TOKEN_AMPERSAND_AMPERSAND:
            return "AMPERSAND_AMPERSAND";
        case TOKEN_AMPERSAND_EQUAL:
            return "AMPERSAND_EQUAL";
        case TOKEN_PLUS_EQUAL:
            return "PLUS_EQUAL";
        case TOKEN_MINUS_EQUAL:
            return "MINUS_EQUAL";
        case TOKEN_SLASH_EQUAL:
            return "SLASH_EQUAL";
        case TOKEN_CARET_EQUAL:
            return "CARET_EQUAL";
        case TOKEN_QUESTION:
            return "QUESTION";
        case TOKEN_COLON:
            return "COLON";
        case TOKEN_DOUBLE_COLON:
            return "DOUBLE_COLON";
        case TOKEN_AT:
            return "AT";
        case TOKEN_HASH:
            return "HASH";
        case TOKEN_TILDE:
            return "TILDE";
        case TOKEN_ARROW:
            return "ARROW";
        case TOKEN_IDENTIFIER:
            return "IDENTIFIER";
        case TOKEN_STRING_LITERAL:
            return "STRING_LITERAL";
        case TOKEN_NUMBER_INT:
            return "NUMBER_INT";
        case TOKEN_NUMBER_FLOAT:
            return "NUMBER_FLOAT";
        case TOKEN_CHAR_LITERAL:
            return "CHAR_LITERAL";
        case TOKEN_KEYWORD_PUB:
            return "KEYWORD_PUB";
        case TOKEN_KEYWORD_MOD:
            return "KEYWORD_MOD";
        case TOKEN_KEYWORD_USE:
            return "KEYWORD_USE";
        case TOKEN_KEYWORD_TYPE:
            return "KEYWORD_TYPE";
        case TOKEN_KEYWORD_STRUCT:
            return "KEYWORD_STRUCT";
        case TOKEN_KEYWORD_ENUM:
            return "KEYWORD_ENUM";
        case TOKEN_KEYWORD_INTERFACE:
            return "KEYWORD_INTERFACE";
        case TOKEN_KEYWORD_IMPL:
            return "KEYWORD_IMPL";
        case TOKEN_KEYWORD_FUNC:
            return "KEYWORD_FUNC";
        case TOKEN_KEYWORD_CONST:
            return "KEYWORD_CONST";
        case TOKEN_KEYWORD_LET:
            return "KEYWORD_LET";
        case TOKEN_KEYWORD_IF:
            return "KEYWORD_IF";
        case TOKEN_KEYWORD_ELIF:
            return "KEYWORD_ELIF";
        case TOKEN_KEYWORD_ELSE:
            return "KEYWORD_ELSE";
        case TOKEN_KEYWORD_WHILE:
            return "KEYWORD_WHILE";
        case TOKEN_KEYWORD_FOR:
            return "KEYWORD_FOR";
        case TOKEN_KEYWORD_IN:
            return "KEYWORD_IN";
        case TOKEN_KEYWORD_MATCH:
            return "KEYWORD_MATCH";
        case TOKEN_KEYWORD_BREAK:
            return "KEYWORD_BREAK";
        case TOKEN_KEYWORD_CONTINUE:
            return "KEYWORD_CONTINUE";
        case TOKEN_KEYWORD_RETURN:
            return "KEYWORD_RETURN";
        case TOKEN_KEYWORD_DEFER:
            return "KEYWORD_DEFER";
        case TOKEN_KEYWORD_COMPTIME:
            return "KEYWORD_COMPTIME";
        case TOKEN_KEYWORD_STREAM:
            return "KEYWORD_STREAM";
        case TOKEN_KEYWORD_CAPABILITY:
            return "KEYWORD_CAPABILITY";
        case TOKEN_KEYWORD_PHANTOM_RESOURCE:
            return "KEYWORD_PHANTOM_RESOURCE";
        case TOKEN_KEYWORD_TRUE:
            return "KEYWORD_TRUE";
        case TOKEN_KEYWORD_FALSE:
            return "KEYWORD_FALSE";
        case TOKEN_KEYWORD_USING:
            return "KEYWORD_USING";
        case TOKEN_KEYWORD_WITH_CONTEXT:
            return "KEYWORD_WITH_CONTEXT";
        case TOKEN_KEYWORD_CONTEXT:
            return "KEYWORD_CONTEXT";
        case TOKEN_KEYWORD_EXTERN:
            return "KEYWORD_EXTERN";
        case TOKEN_KEYWORD_EXPORT:
            return "KEYWORD_EXPORT";
        case TOKEN_KEYWORD_INLINE:
            return "KEYWORD_INLINE";
        case TOKEN_KEYWORD_UNION:
            return "KEYWORD_UNION";
        case TOKEN_KEYWORD_PHANTOM:
            return "KEYWORD_PHANTOM";
        case TOKEN_KEYWORD_ORELSE:
            return "KEYWORD_ORELSE";
        case TOKEN_KEYWORD_CATCH:
            return "KEYWORD_CATCH";
        case TOKEN_KEYWORD_TRY:
            return "KEYWORD_TRY";
        // Primitive types
        case TOKEN_KEYWORD_I8:
            return "KEYWORD_I8";
        case TOKEN_KEYWORD_I16:
            return "KEYWORD_I16";
        case TOKEN_KEYWORD_I32:
            return "KEYWORD_I32";
        case TOKEN_KEYWORD_I64:
            return "KEYWORD_I64";
        case TOKEN_KEYWORD_ISIZE:
            return "KEYWORD_ISIZE";
        case TOKEN_KEYWORD_U8:
            return "KEYWORD_U8";
        case TOKEN_KEYWORD_U16:
            return "KEYWORD_U16";
        case TOKEN_KEYWORD_U32:
            return "KEYWORD_U32";
        case TOKEN_KEYWORD_U64:
            return "KEYWORD_U64";
        case TOKEN_KEYWORD_USIZE:
            return "KEYWORD_USIZE";
        case TOKEN_KEYWORD_F32:
            return "KEYWORD_F32";
        case TOKEN_KEYWORD_F64:
            return "KEYWORD_F64";
        case TOKEN_KEYWORD_BOOL:
            return "KEYWORD_BOOL";
        case TOKEN_KEYWORD_CHAR:
            return "KEYWORD_CHAR";
        case TOKEN_KEYWORD_VOID:
            return "KEYWORD_VOID";
        // New tokens
        case TOKEN_KEYWORD_MUT:
            return "KEYWORD_MUT";
        case TOKEN_KEYWORD_WHEN:
            return "KEYWORD_WHEN";
        case TOKEN_KEYWORD_YIELD:
            return "KEYWORD_YIELD";
        case TOKEN_KEYWORD_WITH:
            return "KEYWORD_WITH";
        case TOKEN_KEYWORD_GRANT:
            return "KEYWORD_GRANT";
        case TOKEN_KEYWORD_REVOKE:
            return "KEYWORD_REVOKE";
        case TOKEN_KEYWORD_PIPELINE:
            return "KEYWORD_PIPELINE";
        case TOKEN_KEYWORD_ASYNC:
            return "KEYWORD_ASYNC";
        case TOKEN_KEYWORD_AWAIT:
            return "KEYWORD_AWAIT";
        case TOKEN_KEYWORD_SYNC:
            return "KEYWORD_SYNC";
        case TOKEN_KEYWORD_NIL:
            return "KEYWORD_NIL";
        case TOKEN_KEYWORD_GUARD:
            return "KEYWORD_GUARD";
        case TOKEN_KEYWORD_THEN:
            return "KEYWORD_THEN";
        case TOKEN_AND:
            return "AND";
        case TOKEN_OR:
            return "OR";
        case TOKEN_NOT:
            return "NOT";
        case TOKEN_ASYNC_PIPELINE:
            return "ASYNC_PIPELINE";
        case TOKEN_REVERSE_PIPELINE:
            return "REVERSE_PIPELINE";
        case TOKEN_COMPOSITION:
            return "COMPOSITION";
        case TOKEN_FUNCTION_ARROW:
            return "FUNCTION_ARROW";
        case TOKEN_NULL_COALESCING:
            return "NULL_COALESCING";
        case TOKEN_FORCE_UNWRAP:
            return "FORCE_UNWRAP";
        case TOKEN_SPACESHIP:
            return "SPACESHIP";
        case TOKEN_POWER:
            return "POWER";
        case TOKEN_DOUBLE_AT:
            return "DOUBLE_AT";
        case TOKEN_WALRUS:
            return "WALRUS";
        case TOKEN_POWER_EQUAL:
            return "POWER_EQUAL";
        case TOKEN_COMMENT:
            return "COMMENT";
        case TOKEN_NEWLINE:
            return "NEWLINE";
        case TOKEN_ERROR:
            return "ERROR";
        case TOKEN_EOF:
            return "EOF";
        default:
            return "UNKNOWN";
    }
}
