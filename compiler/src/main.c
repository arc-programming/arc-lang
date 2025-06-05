#include "arc/cli.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[]) {
    // Handle no arguments case
    if (argc < 2) {
        cli_print_help(argv[0]);
        return CLI_ERROR_ARGS;
    }

    // Initialize CLI context
    CliContext ctx;

    // Parse global options (like --verbose, --output, etc.)
    // This modifies argc/argv to remove global options
    int original_argc = argc;
    char **original_argv = argv;

    CliResult parse_result = cli_parse_global_options(&ctx, &argc, &argv);
    if (parse_result != CLI_SUCCESS) {
        return parse_result;
    }

    // Handle special cases after parsing global options
    for (int i = 1; i < original_argc; i++) {
        if (strcmp(original_argv[i], "-h") == 0 || strcmp(original_argv[i], "--help") == 0) {
            cli_print_help(original_argv[0]);
            free(argv);  // Free the modified argv from cli_parse_global_options
            return CLI_SUCCESS;
        }
        if (strcmp(original_argv[i], "--version") == 0) {
            cli_print_version();
            free(argv);  // Free the modified argv from cli_parse_global_options
            return CLI_SUCCESS;
        }
    }

    // After parsing global options, we should have at least a command
    if (argc < 1) {
        fprintf(stderr, "Error: No command specified\n");
        cli_print_help(original_argv[0]);
        free(argv);
        return CLI_ERROR_ARGS;
    }

    // Get the command name and remaining arguments
    const char *command = argv[0];
    int cmd_argc = argc - 1;
    char **cmd_argv = &argv[1];

    // Execute the command
    CliResult result = cli_execute_command(command, &ctx, cmd_argc, cmd_argv);

    // Clean up the modified argv
    free(argv);

    return result;
}
