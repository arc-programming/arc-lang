#ifndef ARC_COMMON_H
#define ARC_COMMON_H

#include <stdio.h>   // For printf (debugging), file operations eventually
#include <stdlib.h>  // For malloc, free, exit, NULL
#include <string.h>  // For strlen, strcpy, strcmp, memcpy, memset
#include <stdbool.h> // For bool, true, false
#include <stdint.h>  // For fixed-width integer types like uint32_t, int64_t
#include <stddef.h>  // For size_t, ptrdiff_t, NULL (again, but good practice)
#include <assert.h>  // For assert() macro, very useful for debugging

// Platform-specific includes and definitions
#ifdef _WIN32
#include <windows.h>
#define PATH_SEPARATOR '\\'
#define PATH_SEPARATOR_STR "\\"
#else
#include <unistd.h>
#define PATH_SEPARATOR '/'
#define PATH_SEPARATOR_STR "/"
#endif

// Compiler attributes and macros
#ifdef __GNUC__
#define UNUSED __attribute__((unused))
#define NORETURN __attribute__((noreturn))
#define INLINE __attribute__((always_inline)) inline
#elif defined(_MSC_VER)
#define UNUSED
#define NORETURN __declspec(noreturn)
#define INLINE __forceinline
#else
#define UNUSED
#define NORETURN
#define INLINE inline
#endif

// Memory allocation macros with error checking
#define MALLOC(size) arc_malloc(size)
#define CALLOC(count, size) arc_calloc(count, size)
#define REALLOC(ptr, size) arc_realloc(ptr, size)
#define FREE(ptr) arc_free(ptr)

// Safe memory allocation functions
void *arc_malloc(size_t size);
void *arc_calloc(size_t count, size_t size);
void *arc_realloc(void *ptr, size_t size);
void arc_free(void *ptr);

// Error handling
typedef enum
{
    ARC_SUCCESS = 0,
    ARC_ERROR_MEMORY,
    ARC_ERROR_FILE_NOT_FOUND,
    ARC_ERROR_INVALID_SYNTAX,
    ARC_ERROR_TYPE_MISMATCH,
    ARC_ERROR_UNDEFINED_SYMBOL,
    ARC_ERROR_INTERNAL
} arc_error_t;

typedef struct
{
    const char *filename;
    size_t line;
    size_t column;
    size_t offset;
} ArcSourceLocation;

// Error reporting
void arc_report_error(const ArcSourceLocation *loc, const char *format, ...);
void arc_report_warning(const ArcSourceLocation *loc, const char *format, ...);
void arc_report_info(const ArcSourceLocation *loc, const char *format, ...);
NORETURN void arc_report_fatal(const ArcSourceLocation *loc, const char *format, ...);

// String utilities
char *arc_strdup(const char *str);
char *arc_strndup(const char *str, size_t n);
bool arc_str_starts_with(const char *str, const char *prefix);
bool arc_str_ends_with(const char *str, const char *suffix);

// Dynamic array (vector) structure - useful for tokens, AST nodes, etc.
typedef struct
{
    void *data;
    size_t size;
    size_t capacity;
    size_t element_size;
} arc_vector_t;

arc_vector_t *arc_vector_create(size_t element_size);
void arc_vector_destroy(arc_vector_t *vec);
void arc_vector_push(arc_vector_t *vec, const void *element);
void *arc_vector_get(arc_vector_t *vec, size_t index);
void arc_vector_clear(arc_vector_t *vec);
size_t arc_vector_size(arc_vector_t *vec);

// Hash table for symbol tables
typedef struct arc_hash_entry
{
    char *key;
    void *value;
    struct arc_hash_entry *next;
} arc_hash_entry_t;

typedef struct
{
    arc_hash_entry_t **buckets;
    size_t bucket_count;
    size_t size;
} arc_hash_table_t;

arc_hash_table_t *arc_hash_table_create(size_t initial_capacity);
void arc_hash_table_destroy(arc_hash_table_t *table);
void arc_hash_table_insert(arc_hash_table_t *table, const char *key, void *value);
void *arc_hash_table_get(arc_hash_table_t *table, const char *key);
bool arc_hash_table_contains(arc_hash_table_t *table, const char *key);
void arc_hash_table_remove(arc_hash_table_t *table, const char *key);

// File utilities
char *arc_read_file(const char *filename);
bool arc_file_exists(const char *filename);
char *arc_get_file_extension(const char *filename);
char *arc_get_basename(const char *path);
char *arc_get_dirname(const char *path);

// Debug macros
#ifdef DEBUG
#define ARC_DEBUG(fmt, ...) fprintf(stderr, "[DEBUG] %s:%d: " fmt "\n", __FILE__, __LINE__, ##__VA_ARGS__)
#define ARC_TRACE(fmt, ...) fprintf(stderr, "[TRACE] %s:%d in %s(): " fmt "\n", __FILE__, __LINE__, __func__, ##__VA_ARGS__)
#else
#define ARC_DEBUG(fmt, ...)
#define ARC_TRACE(fmt, ...)
#endif

// Useful macros
#define ARC_ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))
#define ARC_MAX(a, b) ((a) > (b) ? (a) : (b))
#define ARC_MIN(a, b) ((a) < (b) ? (a) : (b))
#define ARC_CLAMP(value, min, max) ARC_MAX(min, ARC_MIN(value, max))

// Common constants
#define ARC_MAX_IDENTIFIER_LENGTH 256
#define ARC_MAX_STRING_LENGTH 4096
#define ARC_DEFAULT_VECTOR_CAPACITY 16
#define ARC_DEFAULT_HASH_TABLE_CAPACITY 64

#endif
