#ifndef ARC_CLI_H
#define ARC_CLI_H

#include <stdbool.h>

// CLI command result codes
typedef enum {
    CLI_SUCCESS = 0,
    CLI_ERROR_ARGS = 1,
    CLI_ERROR_FILE = 2,
    CLI_ERROR_COMPILE = 3,
    CLI_ERROR_UNKNOWN_COMMAND = 4
} CliResult;

// CLI context structure for sharing data between commands
typedef struct {
    bool verbose;
    bool quiet;
    const char *output_dir;
    const char *target_name;
} CliContext;

// Command function signature
typedef CliResult (*CommandFunction)(CliContext *ctx, int argc, char **argv);

// Command registration structure
typedef struct {
    const char *name;
    const char *description;
    CommandFunction function;
} CliCommand;

// Main CLI functions
void cli_print_help(const char *program_name);
void cli_print_version(void);
CliResult cli_parse_global_options(CliContext *ctx, int *argc, char ***argv);
CliResult cli_execute_command(const char *command, CliContext *ctx, int argc, char **argv);

// Command implementations
CliResult cmd_compile(CliContext *ctx, int argc, char **argv);
CliResult cmd_build(CliContext *ctx, int argc, char **argv);
CliResult cmd_run(CliContext *ctx, int argc, char **argv);
CliResult cmd_check(CliContext *ctx, int argc, char **argv);

// Future commands (stubs for now)
CliResult cmd_fmt(CliContext *ctx, int argc, char **argv);
CliResult cmd_doc(CliContext *ctx, int argc, char **argv);
CliResult cmd_test(CliContext *ctx, int argc, char **argv);
CliResult cmd_clean(CliContext *ctx, int argc, char **argv);

#endif  // ARC_CLI_H
