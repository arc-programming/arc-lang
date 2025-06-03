#include "arc/common.h"
#include "arc/lexer.h"
#include <stdio.h>

// Quick lexer test function
void test_lexer(void) {
    printf("=== Testing Arc Lexer ===\n");

    // Test basic Arc code
    const char *test_code = "fn main() {\n"
                            "    var x = 42;\n"
                            "    var name = \"Arc Language\";\n"
                            "    if x > 0 {\n"
                            "        return true;\n"
                            "    }\n"
                            "}\n";

    printf("Input code:\n%s\n", test_code);
    printf("Tokens:\n");

    ArcLexer lexer;
    arc_lexer_init(&lexer, test_code, "test.arc");

    ArcToken token;
    int token_count = 0;
    do {
        token = arc_lexer_next_token(&lexer);

        // Skip whitespace and comments for cleaner output
        if (token.type == TOKEN_COMMENT) {
            continue;
        }

        printf("%3d: %-20s", ++token_count, arc_token_type_to_string(token.type));

        // Print the actual lexeme
        if (token.length > 0) {
            printf(" '");
            for (size_t i = 0; i < token.length; i++) {
                printf("%c", token.start[i]);
            }
            printf("'");
        }

        // Print location info
        printf(" (line %zu, col %zu)", token.loc.line, token.loc.column);
        printf("\n");

    } while (token.type != TOKEN_EOF && token.type != TOKEN_ERROR && token_count < 100);

    if (token.type == TOKEN_ERROR) {
        printf("*** Lexer encountered an error! ***\n");
    }

    printf("=== Lexer test complete ===\n\n");
}

int main(int argc, char *argv[]) {
    // Run quick lexer test first
    test_lexer();

    if (argc < 2) {
        printf("Arc Compiler v%s - Usage: %s <input_file.arc>\n",
               "0.0.1",  // Replace with ARC_VERSION from arc_config.h later
               argv[0]);
        return 1;
    }

    printf("Arc Compiler: Processing file '%s'\n", argv[1]);

    // Test lexer on actual file
    char *file_content = arc_read_file(argv[1]);
    if (!file_content) {
        arc_report_error(NULL, "Could not read file: %s", argv[1]);
        return 1;
    }

    printf("=== Lexing file: %s ===\n", argv[1]);
    ArcLexer lexer;
    arc_lexer_init(&lexer, file_content, argv[1]);

    ArcToken token;
    int token_count = 0;
    do {
        token = arc_lexer_next_token(&lexer);

        // Skip whitespace and newlines for cleaner output
        if (token.type == TOKEN_NEWLINE) {
            continue;
        }

        printf("%3d: %-20s", ++token_count, arc_token_type_to_string(token.type));

        if (token.length > 0 && token.length < 50) {  // Limit length for readability
            printf(" '");
            for (size_t i = 0; i < token.length; i++) {
                printf("%c", token.start[i]);
            }
            printf("'");
        }

        printf(" (line %zu)", token.loc.line);
        printf("\n");

        if (token_count > 200) {  // Prevent infinite output
            printf("... (truncated after 200 tokens)\n");
            break;
        }

    } while (token.type != TOKEN_EOF && token.type != TOKEN_ERROR);

    if (token.type == TOKEN_ERROR) {
        printf("*** Lexer encountered an error in file! ***\n");
        FREE(file_content);
        return 1;
    }

    printf("=== File lexing complete ===\n");
    FREE(file_content);

    // TODO:
    // 4. Initialize parser
    // 5. Parse AST
    // 6. Semantic analysis
    // 7. Code generation

    printf("Compilation placeholder complete.\n");
    return 0;
}
