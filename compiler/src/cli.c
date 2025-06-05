#include "arc/cli.h"
#include "arc/codegen.h"
#include "arc/common.h"
#include "arc/lexer.h"
#include "arc/parser.h"
#include "arc/semantic.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Global command registry
static const CliCommand commands[] = {
    {"compile", "Compile Arc source files to C and binary", cmd_compile},
    {"build", "Build Arc project", cmd_build},
    {"run", "Build and run Arc program", cmd_run},
    {"check", "Check Arc source for errors without compiling", cmd_check},
    {"fmt", "Format Arc source code", cmd_fmt},
    {"doc", "Generate documentation", cmd_doc},
    {"test", "Run tests", cmd_test},
    {"clean", "Clean build artifacts", cmd_clean},
    {NULL, NULL, NULL}  // Sentinel
};

void cli_print_help(const char *program_name) {
    printf("Arc Programming Language Compiler\n\n");
    printf("USAGE:\n");
    printf("    %s [OPTIONS] <COMMAND> [ARGS...]\n\n", program_name);

    printf("OPTIONS:\n");
    printf("    -v, --verbose       Enable verbose output\n");
    printf("    -q, --quiet         Suppress non-error output\n");
    printf("    -o, --output <DIR>  Set output directory\n");
    printf("    -h, --help          Print this help message\n");
    printf("    --version           Print version information\n\n");

    printf("COMMANDS:\n");
    for (const CliCommand *cmd = commands; cmd->name != NULL; cmd++) {
        printf("    %-12s %s\n", cmd->name, cmd->description);
    }

    printf("\nUse '%s <command> --help' for more information about a specific command.\n",
           program_name);
}

void cli_print_version(void) {
    printf("Arc compiler version 0.1.0\n");
    printf("Built with love for systems programming\n");
}

CliResult cli_parse_global_options(CliContext *ctx, int *argc, char ***argv) {
    // Initialize context with defaults
    ctx->verbose = false;
    ctx->quiet = false;
    ctx->output_dir = ".";
    ctx->target_name = "arc_program";

    int new_argc = 0;
    char **new_argv = malloc(sizeof(char *) * (*argc));

    // Skip argv[0] (program name) and start from argv[1]
    for (int i = 1; i < *argc; i++) {
        if (strcmp((*argv)[i], "-v") == 0 || strcmp((*argv)[i], "--verbose") == 0) {
            ctx->verbose = true;
        } else if (strcmp((*argv)[i], "-q") == 0 || strcmp((*argv)[i], "--quiet") == 0) {
            ctx->quiet = true;
        } else if (strcmp((*argv)[i], "-o") == 0 || strcmp((*argv)[i], "--output") == 0) {
            if (i + 1 < *argc) {
                ctx->output_dir = (*argv)[++i];
            } else {
                fprintf(stderr, "Error: %s requires an argument\n", (*argv)[i]);
                free(new_argv);
                return CLI_ERROR_ARGS;
            }
        } else if (strcmp((*argv)[i], "-h") == 0 || strcmp((*argv)[i], "--help") == 0) {
            free(new_argv);
            return CLI_SUCCESS;  // Will be handled by caller
        } else if (strcmp((*argv)[i], "--version") == 0) {
            free(new_argv);
            return CLI_SUCCESS;  // Will be handled by caller
        } else {
            // This is not a global option, so include it in the new argv
            new_argv[new_argc++] = (*argv)[i];
        }
    }

    *argc = new_argc;
    *argv = new_argv;
    return CLI_SUCCESS;
}

CliResult cli_execute_command(const char *command, CliContext *ctx, int argc, char **argv) {
    for (const CliCommand *cmd = commands; cmd->name != NULL; cmd++) {
        if (strcmp(command, cmd->name) == 0) {
            return cmd->function(ctx, argc, argv);
        }
    }

    fprintf(stderr, "Error: Unknown command '%s'\n", command);
    fprintf(stderr, "Run 'arc --help' for available commands.\n");
    return CLI_ERROR_UNKNOWN_COMMAND;
}

// Utility function for compilation pipeline
static CliResult compile_arc_file(CliContext *ctx, const char *filename, const char *output_name) {
    if (ctx->verbose) {
        printf("Compiling %s...\n", filename);
    }

    // Read the file
    char *file_content = arc_read_file(filename);
    if (!file_content) {
        fprintf(stderr, "Error: Could not read file '%s'\n", filename);
        return CLI_ERROR_FILE;
    }

    // Initialize lexer
    ArcLexer lexer;
    arc_lexer_init(&lexer, file_content, filename);

    // Create arena for AST memory management
    ArcArena *arena = arc_arena_create(0);
    if (!arena) {
        fprintf(stderr, "Error: Failed to create memory arena\n");
        FREE(file_content);
        return CLI_ERROR_COMPILE;
    }

    // Initialize parser
    ArcParser parser;
    arc_parser_init(&parser, &lexer, arena);

    if (ctx->verbose) {
        printf("Parsing...\n");
    }

    // Parse the program
    ArcAstNode *ast = arc_parser_parse_program(&parser);

    if (arc_parser_had_error(&parser)) {
        fprintf(stderr, "Parse errors in %s:\n", filename);
        const ArcDiagnostic *diag = arc_parser_get_diagnostics(&parser);
        while (diag) {
            fprintf(stderr, "  %s\n", diag->message);
            diag = diag->next;
        }
        arc_parser_cleanup(&parser);
        arc_arena_destroy(arena);
        FREE(file_content);
        return CLI_ERROR_COMPILE;
    }

    if (ctx->verbose) {
        printf("Semantic analysis...\n");
    }

    // Semantic analysis
    ArcSemanticAnalyzer *analyzer = arc_semantic_analyzer_create();
    if (!analyzer) {
        fprintf(stderr, "Error: Failed to create semantic analyzer\n");
        arc_parser_cleanup(&parser);
        arc_arena_destroy(arena);
        FREE(file_content);
        return CLI_ERROR_COMPILE;
    }

    // Keep a pointer to the main arena
    ArcArena *main_arena = analyzer->arena;

    bool sema_success = arc_semantic_analyze(analyzer, ast);

    if (!sema_success || arc_semantic_has_errors(analyzer)) {
        fprintf(stderr, "Semantic errors in %s:\n", filename);
        arc_diagnostic_print_all(analyzer);
        arc_semantic_analyzer_destroy(analyzer);
        // arc_parser_cleanup(&parser); // Assuming this frees from arena
        arc_arena_destroy(main_arena);  // FIX: Destroy the main arena on failure
        FREE(file_content);
        return CLI_ERROR_COMPILE;
    }
    if (ctx->verbose) {
        printf("Generating C code...\n");
    }

    // Code generation
    ArcCodegenOptions codegen_options = arc_codegen_default_options();
    codegen_options.target_name = output_name ? output_name : ctx->target_name;
    codegen_options.output_dir = ctx->output_dir;
    codegen_options.emit_debug_info = ctx->verbose;

    ArcCodegen *codegen = arc_codegen_create(&codegen_options);
    if (!codegen) {
        fprintf(stderr, "Error: Failed to create code generator\n");
        arc_semantic_analyzer_destroy(analyzer);
        arc_parser_cleanup(&parser);
        arc_arena_destroy(arena);
        FREE(file_content);
        return CLI_ERROR_COMPILE;
    }

    bool codegen_success = arc_codegen_generate(codegen, ast, analyzer);

    if (!codegen_success || arc_codegen_has_errors(codegen)) {
        fprintf(stderr, "Code generation errors:\n");
        const char *error = arc_codegen_get_error(codegen);
        if (error) {
            fprintf(stderr, "  %s\n", error);
        }
        arc_codegen_destroy(codegen);
        arc_semantic_analyzer_destroy(analyzer);
        arc_parser_cleanup(&parser);
        arc_arena_destroy(arena);
        FREE(file_content);
        return CLI_ERROR_COMPILE;
    }

    // Cleanup
    // Cleanup
    arc_codegen_destroy(codegen);
    arc_semantic_analyzer_destroy(analyzer);
    // arc_parser_cleanup(&parser); // Assuming this frees from arena
    arc_arena_destroy(main_arena);  // FIX: Destroy the main arena on success
    FREE(file_content);
    if (!ctx->quiet) {
        printf("Successfully generated C code in %s/\n", ctx->output_dir);
    }

    return CLI_SUCCESS;
}

// Utility function to compile C code to binary
static CliResult compile_c_to_binary(CliContext *ctx, const char *output_name) {
    if (ctx->verbose) {
        printf("Compiling C code to binary...\n");
    }

    // Build the command - use the output_name for the generated C files
    char command[512];
    snprintf(command, sizeof(command), "clang -o \"%s/%s.exe\" \"%s/%s.c\" \"%s/main.c\"",
             ctx->output_dir, output_name, ctx->output_dir, output_name, ctx->output_dir);

    if (ctx->verbose) {
        printf("Running: %s\n", command);
    }

    int result = system(command);
    if (result != 0) {
        fprintf(stderr, "Error: C compilation failed\n");
        return CLI_ERROR_COMPILE;
    }

    if (!ctx->quiet) {
        printf("Successfully built %s/%s.exe\n", ctx->output_dir, output_name);
    }

    return CLI_SUCCESS;
}

// Command implementations
CliResult cmd_compile(CliContext *ctx, int argc, char **argv) {
    if (argc < 1) {
        fprintf(stderr, "Error: No input files specified\n");
        fprintf(stderr, "Usage: arc compile <file.arc> [output_name]\n");
        return CLI_ERROR_ARGS;
    }

    const char *input_file = argv[0];
    const char *output_name = (argc > 1) ? argv[1] : "output";

    // Compile Arc to C
    CliResult result = compile_arc_file(ctx, input_file, output_name);
    if (result != CLI_SUCCESS) {
        return result;
    }

    // Compile C to binary
    return compile_c_to_binary(ctx, output_name);
}

CliResult cmd_build(CliContext *ctx, int argc, char **argv) {
    // For now, just alias to compile
    // In the future, this will build entire projects
    if (argc < 1) {
        fprintf(stderr, "Error: No input files specified\n");
        fprintf(stderr, "Usage: arc build <file.arc>\n");
        return CLI_ERROR_ARGS;
    }

    return cmd_compile(ctx, argc, argv);
}

CliResult cmd_run(CliContext *ctx, int argc, char **argv) {
    if (argc < 1) {
        fprintf(stderr, "Error: No input files specified\n");
        fprintf(stderr, "Usage: arc run <file.arc>\n");
        return CLI_ERROR_ARGS;
    }

    const char *input_file = argv[0];
    const char *output_name = "temp_run";

    // Compile first
    CliResult result = compile_arc_file(ctx, input_file, output_name);
    if (result != CLI_SUCCESS) {
        return result;
    }

    result = compile_c_to_binary(ctx, output_name);
    if (result != CLI_SUCCESS) {
        return result;
    }

    // Run the compiled binary
    char run_command[256];
    snprintf(run_command, sizeof(run_command), "\"%s/%s.exe\"", ctx->output_dir, output_name);

    if (ctx->verbose) {
        printf("Running: %s\n", run_command);
    }

    int exit_code = system(run_command);  // Clean up temporary files
    char cleanup_cmd[512];
    snprintf(cleanup_cmd, sizeof(cleanup_cmd),
             "del \"%s\\%s.exe\" \"%s\\%s.c\" \"%s\\%s.h\" \"%s\\main.c\" 2>nul", ctx->output_dir,
             output_name, ctx->output_dir, output_name, ctx->output_dir, output_name,
             ctx->output_dir);
    system(cleanup_cmd);

    return (exit_code == 0) ? CLI_SUCCESS : CLI_ERROR_COMPILE;
}

CliResult cmd_check(CliContext *ctx, int argc, char **argv) {
    if (argc < 1) {
        fprintf(stderr, "Error: No input files specified\n");
        fprintf(stderr, "Usage: arc check <file.arc>\n");
        return CLI_ERROR_ARGS;
    }

    const char *filename = argv[0];

    if (ctx->verbose) {
        printf("Checking %s...\n", filename);
    }

    // Read and parse file (same as compile but without codegen)
    char *file_content = arc_read_file(filename);
    if (!file_content) {
        fprintf(stderr, "Error: Could not read file '%s'\n", filename);
        return CLI_ERROR_FILE;
    }

    ArcLexer lexer;
    arc_lexer_init(&lexer, file_content, filename);

    ArcArena *arena = arc_arena_create(0);
    if (!arena) {
        fprintf(stderr, "Error: Failed to create memory arena\n");
        FREE(file_content);
        return CLI_ERROR_COMPILE;
    }

    ArcParser parser;
    arc_parser_init(&parser, &lexer, arena);

    ArcAstNode *ast = arc_parser_parse_program(&parser);

    if (arc_parser_had_error(&parser)) {
        fprintf(stderr, "Parse errors in %s:\n", filename);
        const ArcDiagnostic *diag = arc_parser_get_diagnostics(&parser);
        while (diag) {
            fprintf(stderr, "  %s\n", diag->message);
            diag = diag->next;
        }
        arc_parser_cleanup(&parser);
        arc_arena_destroy(arena);
        FREE(file_content);
        return CLI_ERROR_COMPILE;
    }

    ArcSemanticAnalyzer *analyzer = arc_semantic_analyzer_create();
    if (!analyzer) {
        fprintf(stderr, "Error: Failed to create semantic analyzer\n");
        arc_parser_cleanup(&parser);
        arc_arena_destroy(arena);
        FREE(file_content);
        return CLI_ERROR_COMPILE;
    }

    bool sema_success = arc_semantic_analyze(analyzer, ast);

    if (!sema_success || arc_semantic_has_errors(analyzer)) {
        fprintf(stderr, "Semantic errors in %s:\n", filename);
        arc_diagnostic_print_all(analyzer);
        arc_semantic_analyzer_destroy(analyzer);
        arc_parser_cleanup(&parser);
        arc_arena_destroy(arena);
        FREE(file_content);
        return CLI_ERROR_COMPILE;
    }

    arc_semantic_analyzer_destroy(analyzer);
    arc_parser_cleanup(&parser);
    arc_arena_destroy(arena);
    FREE(file_content);

    if (!ctx->quiet) {
        printf("✓ %s: No errors found\n", filename);
    }

    return CLI_SUCCESS;
}

// Future command stubs
CliResult cmd_fmt(CliContext *ctx, int argc, char **argv) {
    fprintf(stderr, "Error: 'fmt' command not yet implemented\n");
    return CLI_ERROR_UNKNOWN_COMMAND;
}

CliResult cmd_doc(CliContext *ctx, int argc, char **argv) {
    fprintf(stderr, "Error: 'doc' command not yet implemented\n");
    return CLI_ERROR_UNKNOWN_COMMAND;
}

CliResult cmd_test(CliContext *ctx, int argc, char **argv) {
    fprintf(stderr, "Error: 'test' command not yet implemented\n");
    return CLI_ERROR_UNKNOWN_COMMAND;
}

CliResult cmd_clean(CliContext *ctx, int argc, char **argv) {
    fprintf(stderr, "Error: 'clean' command not yet implemented\n");
    return CLI_ERROR_UNKNOWN_COMMAND;
}
