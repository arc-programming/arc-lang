#ifndef ARC_TEST_FRAMEWORK_H
#define ARC_TEST_FRAMEWORK_H

#include "unity.h"
#include <string.h>

// Suppress Windows deprecation warnings
#ifdef _WIN32
#define _CRT_SECURE_NO_WARNINGS
#pragma warning(push)
#pragma warning(disable : 4996)
#endif

// Common test macros for Arc
#define ARC_TEST_ASSERT_TOKEN_TYPE(expected, actual) TEST_ASSERT_EQUAL_INT(expected, actual)

#define ARC_TEST_ASSERT_TOKEN_TEXT(expected, token)                                                \
    do {                                                                                           \
        char token_text[1024];                                                                     \
        size_t len = (token).length < 1023 ? (token).length : 1023;                                \
        strncpy(token_text, (token).start, len);                                                   \
        token_text[len] = '\0';                                                                    \
        TEST_ASSERT_EQUAL_STRING(expected, token_text);                                            \
    } while (0)

// Test utilities
char *load_test_file(const char *filename);
void free_test_file(char *content);

#ifdef _WIN32
#pragma warning(pop)
#endif

#endif /* ARC_TEST_FRAMEWORK_H */
