# Arc Language Code Style Guide

This document describes the coding conventions used in the Arc language project.

## General Principles

- **Clarity over cleverness**: Code should be readable and self-documenting
- **Consistency**: Follow established patterns within the codebase
- **Simplicity**: Prefer simple, straightforward solutions
- **Performance**: Consider performance implications, but not at the expense of clarity

## C Code Style (Compiler)

### Naming Conventions

#### Variables and Functions
- Use `snake_case` for variables, functions, and file names
- Use descriptive names that clearly indicate purpose
- Avoid abbreviations unless they're widely understood (e.g., `ptr`, `len`)

```c
// Good
int token_count = 0;
char* source_file_path = NULL;
Arc_Result parse_expression(Arc_Parser* parser);

// Bad  
int tc = 0;
char* sfp = NULL;
Arc_Result parseExpr(Arc_Parser* p);
```

#### Types and Structures
- Use `PascalCase` for type names
- Prefix Arc-specific types with `Arc_`
- Use `typedef struct` pattern

```c
// Good
typedef struct Arc_Token {
    Arc_TokenType type;
    char* value;
    size_t line, column;
} Arc_Token;

typedef enum Arc_TokenType {
    TOKEN_IDENTIFIER,
    TOKEN_NUMBER,
    TOKEN_STRING
} Arc_TokenType;

// Bad
typedef struct token {
    int type;
    char* val;
} token_t;
```

#### Constants and Macros
- Use `UPPER_SNAKE_CASE` for constants and macros
- Group related constants in enums when possible

```c
// Good
#define MAX_IDENTIFIER_LENGTH 256
#define ARC_VERSION_MAJOR 0

const size_t DEFAULT_BUFFER_SIZE = 4096;

// Bad
#define maxIdLen 256
#define ArcVersionMajor 0
```

### Code Formatting

#### Indentation and Spacing
- Use 4 spaces for indentation (no tabs)
- Use spaces around operators
- No trailing whitespace
- Single space after keywords (`if`, `for`, `while`)

```c
// Good
if (condition) {
    result = value_a + value_b;
    return result * 2;
}

for (size_t i = 0; i < count; i++) {
    process_item(items[i]);
}

// Bad
if(condition){
    result=value_a+value_b;
    return result*2;
}

for(size_t i=0;i<count;i++){
    process_item(items[i]);
}
```

#### Braces
- Use K&R brace style
- Always use braces for control structures, even single statements

```c
// Good  
if (condition) {
    do_something();
}

while (running) {
    process_events();
}

// Bad
if (condition)
{
    do_something();
}

if (condition)
    do_something();
```

#### Function Definitions
- Opening brace on same line as function signature
- Parameter alignment for long signatures

```c
// Good
Arc_Result arc_lexer_tokenize(Arc_Lexer* lexer, const char* source, 
                              Arc_Token** tokens, size_t* token_count) {
    // Implementation
}

// Good (short signature)
void arc_token_free(Arc_Token* token) {
    // Implementation
}
```

### Error Handling

- Use `Arc_Result` enum for function return values
- Check all return values from functions that can fail
- Use early returns to reduce nesting
- Clean up resources in reverse order of acquisition

```c
// Good
Arc_Result parse_function(Arc_Parser* parser, Arc_FunctionDecl** func) {
    if (!parser || !func) {
        return ARC_ERROR_INVALID_PARAMETER;
    }
    
    Arc_Token* name_token = arc_parser_expect_token(parser, TOKEN_IDENTIFIER);
    if (!name_token) {
        return ARC_ERROR_SYNTAX;
    }
    
    Arc_FunctionDecl* function = arc_malloc(sizeof(Arc_FunctionDecl));
    if (!function) {
        return ARC_ERROR_MEMORY;
    }
    
    // ... implementation ...
    
    *func = function;
    return ARC_OK;
}
```

### Memory Management

- Always check `malloc`/`calloc` return values
- Free memory in reverse order of allocation
- Set pointers to `NULL` after freeing
- Use consistent naming: `_new()`, `_free()`, `_init()`, `_cleanup()`

```c
// Good
Arc_String* arc_string_new(const char* str) {
    Arc_String* string = arc_malloc(sizeof(Arc_String));
    if (!string) {
        return NULL;
    }
    
    string->data = arc_strdup(str);
    if (!string->data) {
        arc_free(string);
        return NULL;
    }
    
    string->length = strlen(str);
    return string;
}

void arc_string_free(Arc_String* string) {
    if (string) {
        arc_free(string->data);
        string->data = NULL;
        arc_free(string);
    }
}
```

### Comments

- Use `//` for single-line comments
- Use `/* */` for multi-line comments and header documentation
- Write comments that explain "why", not "what"
- Document complex algorithms and non-obvious code

```c
// Good
/* 
 * Parse a function declaration including parameters and return type.
 * This handles both regular functions and generic functions with
 * compile-time parameters.
 */
Arc_Result parse_function_declaration(Arc_Parser* parser) {
    // Skip 'fn' keyword - already consumed by caller
    
    // Function parameters use different scoping rules than variables
    arc_parser_enter_scope(parser);
    
    // ... implementation
}

// Bad
// Parse function
Arc_Result parse_function_declaration(Arc_Parser* parser) {
    // Skip fn
    
    // Enter scope
    arc_parser_enter_scope(parser);
}
```

## Arc Language Style (Future Self-Hosted Compiler)

### Naming Conventions
- Variables, functions, modules: `snake_case`
- Types: `PascalCase`
- Constants: `UPPER_SNAKE_CASE`

### Code Organization
- One type definition per file when possible
- Group related functionality in modules
- Keep functions focused and small (< 50 lines typically)

### Example Arc Code Style
```arc
// Good Arc code style
type TokenType enum {
    Identifier,
    Number,
    String,
    Keyword(KeywordKind),
}

fn parse_expression(parser ^Parser) Result(Expression, ParseError) {
    var left = parse_primary(parser)!;
    
    while parser.current_token_is_binary_op() {
        var op = parser.consume_token()!;
        var right = parse_primary(parser)!;
        left = Expression.Binary{ 
            .left = left, 
            .op = op, 
            .right = right 
        };
    }
    
    return .ok(left);
}
```

## File Organization

### Header Files
- Use include guards or `#pragma once`
- Forward declare when possible to reduce dependencies
- Group includes: system headers first, then project headers
- Document public APIs

```c
#ifndef ARC_PARSER_H
#define ARC_PARSER_H

#include <stddef.h>
#include <stdbool.h>

#include "arc/common.hpp"
#include "arc/lexer.h"

// Forward declarations
typedef struct Arc_AST Arc_AST;
typedef struct Arc_Parser Arc_Parser;

/* Parse source code into an Abstract Syntax Tree */
Arc_Result arc_parse(const char* source, Arc_AST** ast);

#endif // ARC_PARSER_H
```

## Testing

- Write unit tests for all public functions
- Test error conditions and edge cases
- Use descriptive test names that explain what is being tested
- Keep tests focused on single behaviors

```c
// Good test name and structure
void test_lexer_handles_unterminated_string_literal(void) {
    Arc_Lexer lexer;
    arc_lexer_init(&lexer, "\"unterminated string");
    
    Arc_Token token = arc_lexer_next_token(&lexer);
    
    assert(token.type == TOKEN_ERROR);
    assert(strstr(token.error_message, "unterminated") != NULL);
    
    arc_lexer_cleanup(&lexer);
}
```

## Performance Guidelines

- Profile before optimizing
- Prefer clarity over micro-optimizations
- Document performance-critical code sections
- Use appropriate data structures for the task
- Minimize memory allocations in hot paths

## Tools and Automation

This codebase uses several tools to maintain code quality:

- **clang-format**: Automatic code formatting
- **clang-tidy**: Static analysis and linting
- **valgrind**: Memory leak detection (Linux)
- **AddressSanitizer**: Memory error detection
- **Unit tests**: Automated testing with check or similar framework

Run these tools regularly during development:

```bash
# Format code
make format

# Run static analysis  
make lint

# Run tests with memory checking
make test-valgrind
```

## Contributing

When contributing to Arc:

1. Follow the existing code style in the file you're modifying
2. Run the formatting and linting tools before submitting
3. Add tests for new functionality
4. Update documentation for public API changes
5. Write clear, descriptive commit messages

Remember: Code is read more often than it's written. Prioritize clarity and maintainability.