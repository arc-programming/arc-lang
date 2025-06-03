#include "unity.h"
#include <stdio.h>

// Forward declarations of test functions
void test_lexer_simple_tokens(void);
void test_lexer_keywords(void);
void test_lexer_operators(void);
void test_lexer_numbers(void);
void test_lexer_strings(void);
void test_lexer_comments(void);
void test_lexer_error_handling(void);

// Unity framework setup/teardown
void setUp(void) {
    // Called before each test
    // Add any per-test setup here
}

void tearDown(void) {
    // Called after each test
    // Add any per-test cleanup here
}

int main(void) {
    printf("Running Arc Compiler Unit Tests\n");
    printf("================================\n");

    UNITY_BEGIN();

    // Lexer Tests
    printf("\n--- Lexer Tests ---\n");
    RUN_TEST(test_lexer_simple_tokens);
    RUN_TEST(test_lexer_keywords);
    RUN_TEST(test_lexer_operators);
    RUN_TEST(test_lexer_numbers);
    RUN_TEST(test_lexer_strings);
    RUN_TEST(test_lexer_comments);
    RUN_TEST(test_lexer_error_handling);

    // Parser Tests (add when ready)
    // printf("\n--- Parser Tests ---\n");
    // RUN_TEST(test_parser_expressions);
    // RUN_TEST(test_parser_statements);

    // AST Tests (add when ready)
    // printf("\n--- AST Tests ---\n");
    // RUN_TEST(test_ast_creation);

    printf("\n");
    return UNITY_END();
}
