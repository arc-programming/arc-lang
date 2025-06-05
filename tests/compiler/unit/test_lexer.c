#include "arc/common.h"
#include "arc/lexer.h"
#include "test_framework.h"
#include "unity.h"
#include <string.h>

// Helper function to create and verify a single token
static void verify_single_token(const char *source, ArcTokenType expected_type,
                                const char *expected_text) {
    ArcLexer lexer;
    arc_lexer_init(&lexer, source, "test");

    ArcToken token = arc_lexer_next_token(&lexer);
    ARC_TEST_ASSERT_TOKEN_TYPE(expected_type, token.type);

    if (expected_text) {
        ARC_TEST_ASSERT_TOKEN_TEXT(expected_text, token);
    }
}

// Helper function to skip whitespace and get next token
static ArcToken get_next_token_skip_whitespace(ArcLexer *lexer) {
    ArcToken token;
    do {
        token = arc_lexer_next_token(lexer);
    } while (token.type == TOKEN_NEWLINE || token.type == TOKEN_COMMENT);
    return token;
}

void test_lexer_simple_tokens(void) {
    printf("Testing simple tokens...\n");

    // Test basic punctuation
    verify_single_token("(", TOKEN_LPAREN, "(");
    verify_single_token(")", TOKEN_RPAREN, ")");
    verify_single_token("{", TOKEN_LBRACE, "{");
    verify_single_token("}", TOKEN_RBRACE, "}");
    verify_single_token("[", TOKEN_LBRACKET, "[");
    verify_single_token("]", TOKEN_RBRACKET, "]");
    verify_single_token(",", TOKEN_COMMA, ",");
    verify_single_token(";", TOKEN_SEMICOLON, ";");
    verify_single_token(".", TOKEN_DOT, ".");

    // Test basic operators
    verify_single_token("+", TOKEN_PLUS, "+");
    verify_single_token("-", TOKEN_MINUS, "-");
    verify_single_token("*", TOKEN_ASTERISK, "*");
    verify_single_token("/", TOKEN_SLASH, "/");
    verify_single_token("=", TOKEN_EQUAL, "=");
    verify_single_token("!", TOKEN_BANG, "!");

    // Test compound operators
    verify_single_token("==", TOKEN_EQUAL_EQUAL, "==");
    verify_single_token("!=", TOKEN_BANG_EQUAL, "!=");
    verify_single_token("<=", TOKEN_LESS_EQUAL, "<=");
    verify_single_token(">=", TOKEN_GREATER_EQUAL, ">=");
    verify_single_token("->", TOKEN_ARROW, "->");
    verify_single_token("|>", TOKEN_PIPELINE, "|>");
}

void test_lexer_keywords(void) {
    printf("Testing keywords...\n");

    // Test function-related keywords
    verify_single_token("func", TOKEN_KEYWORD_FUNC, "func");
    verify_single_token("return", TOKEN_KEYWORD_RETURN, "return");
    verify_single_token("extern", TOKEN_KEYWORD_EXTERN, "extern");
    verify_single_token("export", TOKEN_KEYWORD_EXPORT, "export");
    verify_single_token("inline", TOKEN_KEYWORD_INLINE, "inline");

    // Test variable keywords
    verify_single_token("let", TOKEN_KEYWORD_LET, "let");
    verify_single_token("const", TOKEN_KEYWORD_CONST, "const");

    // Test type keywords
    verify_single_token("type", TOKEN_KEYWORD_TYPE, "type");
    verify_single_token("struct", TOKEN_KEYWORD_STRUCT, "struct");
    verify_single_token("enum", TOKEN_KEYWORD_ENUM, "enum");
    verify_single_token("union", TOKEN_KEYWORD_UNION, "union");
    verify_single_token("interface", TOKEN_KEYWORD_INTERFACE, "interface");
    verify_single_token("impl", TOKEN_KEYWORD_IMPL, "impl");

    // Test control flow keywords
    verify_single_token("if", TOKEN_KEYWORD_IF, "if");
    verify_single_token("else", TOKEN_KEYWORD_ELSE, "else");
    verify_single_token("while", TOKEN_KEYWORD_WHILE, "while");
    verify_single_token("for", TOKEN_KEYWORD_FOR, "for");
    verify_single_token("match", TOKEN_KEYWORD_MATCH, "match");
    verify_single_token("break", TOKEN_KEYWORD_BREAK, "break");
    verify_single_token("continue", TOKEN_KEYWORD_CONTINUE, "continue");

    // Test literals
    verify_single_token("true", TOKEN_KEYWORD_TRUE, "true");
    verify_single_token("false", TOKEN_KEYWORD_FALSE, "false");
    verify_single_token("nil", TOKEN_KEYWORD_NIL, "nil");

    // Test Arc-specific keywords
    verify_single_token("comptime", TOKEN_KEYWORD_COMPTIME, "comptime");
    verify_single_token("defer", TOKEN_KEYWORD_DEFER, "defer");
    verify_single_token("using", TOKEN_KEYWORD_USING, "using");
    verify_single_token("context", TOKEN_KEYWORD_CONTEXT, "context");
    verify_single_token("phantom", TOKEN_KEYWORD_PHANTOM, "phantom");
    verify_single_token("capability", TOKEN_KEYWORD_CAPABILITY, "capability");
    verify_single_token("stream", TOKEN_KEYWORD_STREAM, "stream");
}

void test_lexer_operators(void) {
    printf("Testing operators...\n");

    // Test arithmetic operators
    verify_single_token("+", TOKEN_PLUS, "+");
    verify_single_token("-", TOKEN_MINUS, "-");
    verify_single_token("*", TOKEN_ASTERISK, "*");
    verify_single_token("/", TOKEN_SLASH, "/");
    verify_single_token("%", TOKEN_PERCENT, "%");

    // Test comparison operators
    verify_single_token("<", TOKEN_LESS, "<");
    verify_single_token(">", TOKEN_GREATER, ">");
    verify_single_token("<=", TOKEN_LESS_EQUAL, "<=");
    verify_single_token(">=", TOKEN_GREATER_EQUAL, ">=");
    verify_single_token("==", TOKEN_EQUAL_EQUAL, "==");
    verify_single_token("!=", TOKEN_BANG_EQUAL, "!=");

    // Test assignment operators
    verify_single_token("=", TOKEN_EQUAL, "=");
    verify_single_token("+=", TOKEN_PLUS_EQUAL, "+=");
    verify_single_token("-=", TOKEN_MINUS_EQUAL, "-=");
    verify_single_token("/=", TOKEN_SLASH_EQUAL, "/=");

    // Test logical operators
    verify_single_token("&&", TOKEN_AMPERSAND_AMPERSAND, "&&");
    verify_single_token("||", TOKEN_PIPE_PIPE, "||");
    verify_single_token("!", TOKEN_BANG, "!");

    // Test bitwise operators
    verify_single_token("&", TOKEN_AMPERSAND, "&");
    verify_single_token("|", TOKEN_PIPE, "|");
    verify_single_token("^", TOKEN_CARET, "^");
    verify_single_token("~", TOKEN_TILDE, "~");
    verify_single_token("<<", TOKEN_LEFT_SHIFT, "<<");
    verify_single_token(">>", TOKEN_RIGHT_SHIFT, ">>");
}

void test_lexer_numbers(void) {
    printf("Testing numbers...\n");

    // Test decimal integers
    verify_single_token("0", TOKEN_NUMBER_INT, "0");
    verify_single_token("42", TOKEN_NUMBER_INT, "42");
    verify_single_token("12345", TOKEN_NUMBER_INT, "12345");

    // Test hexadecimal numbers
    verify_single_token("0x0", TOKEN_NUMBER_INT, "0x0");
    verify_single_token("0xDEADBEEF", TOKEN_NUMBER_INT, "0xDEADBEEF");
    verify_single_token("0xFF", TOKEN_NUMBER_INT, "0xFF");

    // Test binary numbers
    verify_single_token("0b0", TOKEN_NUMBER_INT, "0b0");
    verify_single_token("0b1010", TOKEN_NUMBER_INT, "0b1010");
    verify_single_token("0b11111111", TOKEN_NUMBER_INT, "0b11111111");

    // Test octal numbers
    verify_single_token("0o0", TOKEN_NUMBER_INT, "0o0");
    verify_single_token("0o755", TOKEN_NUMBER_INT, "0o755");
    verify_single_token("0o123", TOKEN_NUMBER_INT, "0o123");

    // Test floating point numbers
    verify_single_token("3.14", TOKEN_NUMBER_FLOAT, "3.14");
    verify_single_token("0.5", TOKEN_NUMBER_FLOAT, "0.5");
    verify_single_token("123.456", TOKEN_NUMBER_FLOAT, "123.456");

    // Test scientific notation
    verify_single_token("1e5", TOKEN_NUMBER_FLOAT, "1e5");
    verify_single_token("3.14e-2", TOKEN_NUMBER_FLOAT, "3.14e-2");
    verify_single_token("2.5E+3", TOKEN_NUMBER_FLOAT, "2.5E+3");
}

void test_lexer_strings(void) {
    printf("Testing strings...\n");

    // Test simple strings
    verify_single_token("\"hello\"", TOKEN_STRING_LITERAL, "\"hello\"");
    verify_single_token("\"world\"", TOKEN_STRING_LITERAL, "\"world\"");
    verify_single_token("\"\"", TOKEN_STRING_LITERAL, "\"\"");

    // Test strings with escape sequences
    verify_single_token("\"hello\\nworld\"", TOKEN_STRING_LITERAL, "\"hello\\nworld\"");
    verify_single_token("\"tab\\there\"", TOKEN_STRING_LITERAL, "\"tab\\there\"");
    verify_single_token("\"quote\\\"test\"", TOKEN_STRING_LITERAL, "\"quote\\\"test\"");
    verify_single_token("\"backslash\\\\test\"", TOKEN_STRING_LITERAL, "\"backslash\\\\test\"");

    // Test character literals
    verify_single_token("'a'", TOKEN_CHAR_LITERAL, "'a'");
    verify_single_token("'Z'", TOKEN_CHAR_LITERAL, "'Z'");
    verify_single_token("'\\n'", TOKEN_CHAR_LITERAL, "'\\n'");
    verify_single_token("'\\''", TOKEN_CHAR_LITERAL, "'\\''");
    verify_single_token("'\\\\'", TOKEN_CHAR_LITERAL, "'\\\\'");

    // Test unicode escape sequences
    verify_single_token("'\\u{1F60A}'", TOKEN_CHAR_LITERAL, "'\\u{1F60A}'");
}

void test_lexer_comments(void) {
    printf("Testing comments...\n");

    // Test line comments
    verify_single_token("// this is a comment", TOKEN_COMMENT, "// this is a comment");

    // Test block comments
    verify_single_token("/* block comment */", TOKEN_COMMENT, "/* block comment */");
    verify_single_token("/* multi\nline\ncomment */", TOKEN_COMMENT, "/* multi\nline\ncomment */");

    // Test code with comments
    ArcLexer lexer;
    arc_lexer_init(&lexer, "fn main() { // comment\n return 42; }", "test");

    ArcToken token = arc_lexer_next_token(&lexer);
    ARC_TEST_ASSERT_TOKEN_TYPE(TOKEN_KEYWORD_FUNC, token.type);

    token = get_next_token_skip_whitespace(&lexer);
    ARC_TEST_ASSERT_TOKEN_TYPE(TOKEN_IDENTIFIER, token.type);
    ARC_TEST_ASSERT_TOKEN_TEXT("main", token);
}

void test_lexer_error_handling(void) {
    printf("Testing error handling...\n");

    // Test unterminated string
    verify_single_token("\"unterminated", TOKEN_ERROR, NULL);

    // Test unterminated character literal
    verify_single_token("'unterminated", TOKEN_ERROR, NULL);

    // Test invalid characters
    verify_single_token("$", TOKEN_ERROR, NULL);

    // Test unterminated block comment
    verify_single_token("/* unterminated", TOKEN_ERROR, NULL);
}
