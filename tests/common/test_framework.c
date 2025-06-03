#include "test_framework.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *load_test_file(const char *filename) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        return NULL;
    }

    // Get file size
    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    // Allocate buffer
    char *content = malloc(length + 1);
    if (!content) {
        fclose(file);
        return NULL;
    }

    // Read file
    size_t read_length = fread(content, 1, length, file);
    content[read_length] = '\0';

    fclose(file);
    return content;
}

void free_test_file(char *content) {
    if (content) {
        free(content);
    }
}
