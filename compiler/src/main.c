#include "arc/codegen.h"
#include "arc/common.h"
#include "arc/lexer.h"
#include "arc/parser.h"
#include "arc/semantic.h"
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

    } while (token.type != TOKEN_EOF && token.type != TOKEN_ERROR);

    if (token.type == TOKEN_ERROR) {
        printf("*** Lexer encountered an error in file! ***\n");
        FREE(file_content);
        return 1;
    }
    printf("=== File lexing complete ===\n");

    // Test parser on the file
    printf("\n=== Parsing file: %s ===\n", argv[1]);

    // Reset lexer for parsing
    arc_lexer_init(&lexer, file_content, argv[1]);

    // Create arena for AST memory management
    ArcArena *arena = arc_arena_create(0);  // Use default size
    if (!arena) {
        printf("*** Failed to create arena for parser! ***\n");
        FREE(file_content);
        return 1;
    }  // Initialize parser
    ArcParser parser;
    arc_parser_init(&parser, &lexer, arena);

    printf("Starting parser...\n");

    // Parse the program
    ArcAstNode *ast = arc_parser_parse_program(&parser);

    printf("Parser finished.\n");

    if (arc_parser_had_error(&parser)) {
        printf("*** Parser encountered errors! ***\n");

        // Print diagnostics
        const ArcDiagnostic *diag = arc_parser_get_diagnostics(&parser);
        while (diag) {
            printf("Error: %s\n", diag->message);
            diag = diag->next;
        }

        arc_parser_cleanup(&parser);
        arc_arena_destroy(arena);
        FREE(file_content);
        return 1;
    }  // Print AST
    printf("=== AST Structure ===\n");
    arc_ast_node_print(ast, 0);
    printf("=== AST Complete ===\n");  // Semantic analysis stage
    printf("\n=== Starting Semantic Analysis ===\n");

    ArcSemanticAnalyzer *analyzer = arc_semantic_analyzer_create();
    if (!analyzer) {
        printf("*** Failed to create semantic analyzer! ***\n");
        arc_parser_cleanup(&parser);
        arc_arena_destroy(arena);
        FREE(file_content);
        return 1;
    }

    bool sema_success = arc_semantic_analyze(analyzer, ast);

    if (!sema_success || arc_semantic_has_errors(analyzer)) {
        printf("*** Semantic analysis encountered errors! ***\n");

        // Print semantic analysis diagnostics
        arc_diagnostic_print_all(analyzer);

        arc_semantic_analyzer_destroy(analyzer);
        arc_parser_cleanup(&parser);
        arc_arena_destroy(arena);
        FREE(file_content);
        return 1;
    }
    printf("=== Semantic Analysis Complete ===\n");

    // Code generation stage
    printf("\n=== Starting Code Generation ===\n");

    ArcCodegenOptions codegen_options = arc_codegen_default_options();
    codegen_options.target_name = "arc_program";
    codegen_options.output_dir = ".";
    codegen_options.emit_debug_info = true;

    ArcCodegen *codegen = arc_codegen_create(&codegen_options);
    if (!codegen) {
        printf("*** Failed to create code generator! ***\n");
        arc_semantic_analyzer_destroy(analyzer);
        arc_parser_cleanup(&parser);
        arc_arena_destroy(arena);
        FREE(file_content);
        return 1;
    }

    bool codegen_success = arc_codegen_generate(codegen, ast, analyzer);

    if (!codegen_success || arc_codegen_has_errors(codegen)) {
        printf("*** Code generation encountered errors! ***\n");
        const char *error = arc_codegen_get_error(codegen);
        if (error) {
            printf("Error: %s\n", error);
        }

        arc_codegen_destroy(codegen);
        arc_semantic_analyzer_destroy(analyzer);
        arc_parser_cleanup(&parser);
        arc_arena_destroy(arena);
        FREE(file_content);
        return 1;
    }

    printf("=== Code Generation Complete ===\n");
    printf("Generated files:\n");
    printf("  - arc_program.h\n");
    printf("  - arc_program.c\n");
    printf("  - main.c\n");

    // Cleanup
    arc_codegen_destroy(codegen);
    arc_semantic_analyzer_destroy(analyzer);
    arc_parser_cleanup(&parser);
    arc_arena_destroy(arena);
    FREE(file_content);

    printf("Compilation complete.\n");
    return 0;
}
