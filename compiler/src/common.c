#include "arc/common.h"
#include <stdarg.h>

// Safe memory allocation functions
void* arc_malloc(size_t size) {
    void* ptr = malloc(size);
    if (!ptr && size > 0) {
        arc_fatal("Memory allocation failed: requested %zu bytes", size);
    }
    return ptr;
}

void* arc_calloc(size_t count, size_t size) {
    void* ptr = calloc(count, size);
    if (!ptr && count > 0 && size > 0) {
        arc_fatal("Memory allocation failed: requested %zu elements of %zu bytes", count, size);
    }
    return ptr;
}

void* arc_realloc(void* ptr, size_t size) {
    void* new_ptr = realloc(ptr, size);
    if (!new_ptr && size > 0) {
        arc_fatal("Memory reallocation failed: requested %zu bytes", size);
    }
    return new_ptr;
}

void arc_free(void* ptr) {
    if (ptr) {
        free(ptr);
    }
}

// Error reporting functions
void arc_error(const char* format, ...) {
    fprintf(stderr, "[ERROR] ");
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fprintf(stderr, "\n");
}

void arc_warning(const char* format, ...) {
    fprintf(stderr, "[WARNING] ");
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fprintf(stderr, "\n");
}

void arc_info(const char* format, ...) {
    fprintf(stdout, "[INFO] ");
    va_list args;
    va_start(args, format);
    vfprintf(stdout, format, args);
    va_end(args);
    fprintf(stdout, "\n");
}

NORETURN void arc_fatal(const char* format, ...) {
    fprintf(stderr, "[FATAL] ");
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fprintf(stderr, "\n");
    exit(EXIT_FAILURE);
}

// String utilities
char* arc_strdup(const char* str) {
    if (!str) return NULL;
    
    size_t len = strlen(str);
    char* copy = MALLOC(len + 1);
    memcpy(copy, str, len + 1);
    return copy;
}

char* arc_strndup(const char* str, size_t n) {
    if (!str) return NULL;
    
    size_t len = strlen(str);
    if (n < len) len = n;
    
    char* copy = MALLOC(len + 1);
    memcpy(copy, str, len);
    copy[len] = '\0';
    return copy;
}

bool arc_str_starts_with(const char* str, const char* prefix) {
    if (!str || !prefix) return false;
    return strncmp(str, prefix, strlen(prefix)) == 0;
}

bool arc_str_ends_with(const char* str, const char* suffix) {
    if (!str || !suffix) return false;
    
    size_t str_len = strlen(str);
    size_t suffix_len = strlen(suffix);
    
    if (suffix_len > str_len) return false;
    
    return strcmp(str + str_len - suffix_len, suffix) == 0;
}

// Dynamic array (vector) implementation
arc_vector_t* arc_vector_create(size_t element_size) {
    arc_vector_t* vec = MALLOC(sizeof(arc_vector_t));
    vec->data = MALLOC(ARC_DEFAULT_VECTOR_CAPACITY * element_size);
    vec->size = 0;
    vec->capacity = ARC_DEFAULT_VECTOR_CAPACITY;
    vec->element_size = element_size;
    return vec;
}

void arc_vector_destroy(arc_vector_t* vec) {
    if (vec) {
        FREE(vec->data);
        FREE(vec);
    }
}

void arc_vector_push(arc_vector_t* vec, const void* element) {
    if (!vec || !element) return;
    
    if (vec->size >= vec->capacity) {
        vec->capacity *= 2;
        vec->data = REALLOC(vec->data, vec->capacity * vec->element_size);
    }
    
    memcpy((char*)vec->data + vec->size * vec->element_size, element, vec->element_size);
    vec->size++;
}

void* arc_vector_get(arc_vector_t* vec, size_t index) {
    if (!vec || index >= vec->size) return NULL;
    return (char*)vec->data + index * vec->element_size;
}

void arc_vector_clear(arc_vector_t* vec) {
    if (vec) {
        vec->size = 0;
    }
}

size_t arc_vector_size(arc_vector_t* vec) {
    return vec ? vec->size : 0;
}

// Simple hash function
static size_t arc_hash_string(const char* str, size_t bucket_count) {
    size_t hash = 5381;
    int c;
    while ((c = *str++)) {
        hash = ((hash << 5) + hash) + c;
    }
    return hash % bucket_count;
}

// Hash table implementation
arc_hash_table_t* arc_hash_table_create(size_t initial_capacity) {
    arc_hash_table_t* table = MALLOC(sizeof(arc_hash_table_t));
    table->bucket_count = initial_capacity > 0 ? initial_capacity : ARC_DEFAULT_HASH_TABLE_CAPACITY;
    table->buckets = CALLOC(table->bucket_count, sizeof(arc_hash_entry_t*));
    table->size = 0;
    return table;
}

void arc_hash_table_destroy(arc_hash_table_t* table) {
    if (!table) return;
    
    for (size_t i = 0; i < table->bucket_count; i++) {
        arc_hash_entry_t* entry = table->buckets[i];
        while (entry) {
            arc_hash_entry_t* next = entry->next;
            FREE(entry->key);
            FREE(entry);
            entry = next;
        }
    }
    
    FREE(table->buckets);
    FREE(table);
}

void arc_hash_table_insert(arc_hash_table_t* table, const char* key, void* value) {
    if (!table || !key) return;
    
    size_t hash = arc_hash_string(key, table->bucket_count);
    arc_hash_entry_t* entry = table->buckets[hash];
    
    // Check if key already exists
    while (entry) {
        if (strcmp(entry->key, key) == 0) {
            entry->value = value; // Update existing
            return;
        }
        entry = entry->next;
    }
    
    // Create new entry
    arc_hash_entry_t* new_entry = MALLOC(sizeof(arc_hash_entry_t));
    new_entry->key = arc_strdup(key);
    new_entry->value = value;
    new_entry->next = table->buckets[hash];
    table->buckets[hash] = new_entry;
    table->size++;
}

void* arc_hash_table_get(arc_hash_table_t* table, const char* key) {
    if (!table || !key) return NULL;
    
    size_t hash = arc_hash_string(key, table->bucket_count);
    arc_hash_entry_t* entry = table->buckets[hash];
    
    while (entry) {
        if (strcmp(entry->key, key) == 0) {
            return entry->value;
        }
        entry = entry->next;
    }
    
    return NULL;
}

bool arc_hash_table_contains(arc_hash_table_t* table, const char* key) {
    return arc_hash_table_get(table, key) != NULL;
}

void arc_hash_table_remove(arc_hash_table_t* table, const char* key) {
    if (!table || !key) return;
    
    size_t hash = arc_hash_string(key, table->bucket_count);
    arc_hash_entry_t* entry = table->buckets[hash];
    arc_hash_entry_t* prev = NULL;
    
    while (entry) {
        if (strcmp(entry->key, key) == 0) {
            if (prev) {
                prev->next = entry->next;
            } else {
                table->buckets[hash] = entry->next;
            }
            
            FREE(entry->key);
            FREE(entry);
            table->size--;
            return;
        }
        prev = entry;
        entry = entry->next;
    }
}

// File utilities
char* arc_read_file(const char* filename) {
    if (!filename) return NULL;

    FILE* file = NULL;
    fopen_s(&file, filename, "rb");
    if (!file) {
        arc_error("Could not open file: %s", filename);
        return NULL;
    }
    
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    if (size < 0) {
        fclose(file);
        arc_error("Could not determine file size: %s", filename);
        return NULL;
    }
    
    char* content = MALLOC(size + 1);
    size_t read_size = fread(content, 1, size, file);
    content[read_size] = '\0';
    
    fclose(file);
    return content;
}

bool arc_file_exists(const char* filename) {
    if (!filename) return false;

    FILE* file = NULL;
    fopen_s(&file, filename, "r");
    if (file) {
        fclose(file);
        return true;
    }
    return false;
}

char* arc_get_file_extension(const char* filename) {
    if (!filename) return NULL;
    
    const char* dot = strrchr(filename, '.');
    if (!dot || dot == filename) return NULL;
    
    return arc_strdup(dot + 1);
}

char* arc_get_basename(const char* path) {
    if (!path) return NULL;
    
    const char* last_sep = strrchr(path, PATH_SEPARATOR);
    if (!last_sep) return arc_strdup(path);
    
    return arc_strdup(last_sep + 1);
}

char* arc_get_dirname(const char* path) {
    if (!path) return NULL;
    
    const char* last_sep = strrchr(path, PATH_SEPARATOR);
    if (!last_sep) return arc_strdup(".");
    
    if (last_sep == path) return arc_strdup(PATH_SEPARATOR_STR);
    
    return arc_strndup(path, last_sep - path);
}
