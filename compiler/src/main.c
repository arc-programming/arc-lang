#include "arc/lexer.h"  // Uncomment when you create lexer.h
#include <stdio.h>

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Arc Compiler v%s - Usage: %s <input_file.arc>\n",
               "0.0.1",  // Replace with ARC_VERSION from arc_config.h later
               argv[0]);
        return 1;
    }

    printf("Arc Compiler: Processing file '%s'\n", argv[1]);

    // TODO:
    // 1. Read file content
    // 2. Initialize lexer
    // 3. Lex tokens
    // 4. Initialize parser
    // 5. Parse AST
    // 6. Semantic analysis
    // 7. Code generation

    printf("Compilation placeholder complete.\n");
    return 0;
}
