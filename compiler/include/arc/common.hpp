#pragma once

// C++ Standard Library
#include <algorithm>      // For std::min, std::max, std::clamp, std::sort, etc.
#include <cassert>        // For assert()
#include <cstdarg>        // For va_list, va_start, va_end for variadic functions
#include <cstddef>        // For std::size_t, std::ptrdiff_t, std::nullptr_t
#include <cstdint>        // For std::uint32_t, std::int64_t etc.
#include <cstdio>         // For std::snprintf, std::vsnprintf (can be useful)
#include <cstdlib>        // For std::exit
#include <cstring>        // For std::memcpy, std::memcmp (use with caution)
#include <filesystem>     // For C++17 file system operations
#include <fstream>        // For std::ifstream, std::ofstream
#include <iostream>       // For std::cout, std::cerr, std::endl
#include <optional>       // For std::optional (C++17)
#include <sstream>        // For std::stringstream
#include <string>         // For std::string
#include <string_view>    // For std::string_view (C++17, efficient read-only strings)
#include <system_error>   // For std::error_code with filesystem
#include <unordered_map>  // For std::unordered_map
#include <vector>         // For std::vector

namespace arc {

// --- Platform-specific Definitions ---
#ifdef _WIN32
constexpr char PATH_SEPARATOR = '\\';
constexpr const char *PATH_SEPARATOR_STR = "\\";
#else
constexpr char PATH_SEPARATOR = '/';
constexpr const char *PATH_SEPARATOR_STR = "/";
#endif

// --- Compiler Attributes ---
// ARC_UNUSED can often be replaced by [[maybe_unused]] (C++17) or by
// simply not naming the parameter if it's a function argument.
#ifdef __GNUC__
#define ARC_MAYBE_UNUSED __attribute__((unused))
#define ARC_INLINE __attribute__((always_inline)) inline
#elif defined(_MSC_VER)
#define ARC_MAYBE_UNUSED
#define ARC_INLINE __forceinline
#else
#define ARC_MAYBE_UNUSED
#define ARC_INLINE inline
#endif
// [[noreturn]] is standard C++11, so ARC_NORETURN can be defined directly
#define ARC_NORETURN [[noreturn]]

// --- Source Location ---
struct ArcSourceLocation {
    std::string filename;  // Store filename as std::string
    size_t line = 0;
    size_t column = 0;
    size_t offset = 0;

    // Default constructor
    ArcSourceLocation() = default;

    // Constructor for convenience
    ArcSourceLocation(std::string_view fname, size_t l, size_t c, size_t off = 0)
        : filename(fname), line(l), column(c), offset(off) {}
};

// --- Error Handling ---
enum class ErrorCode {
    SUCCESS = 0,
    MEMORY,  // Can be used if new throws bad_alloc and you catch it
    FILE_NOT_FOUND,
    INVALID_SYNTAX,
    TYPE_MISMATCH,
    UNDEFINED_SYMBOL,
    INTERNAL,
    IO_ERROR,  // Added for file operations
    FORMATTING_ERROR
    // Add more as needed
};

// Error reporting functions (implementations would go in common.cpp)
void report_error(const ArcSourceLocation *loc, ErrorCode code, const std::string &message);
void report_error(const ArcSourceLocation *loc, ErrorCode code, const char *format,
                  ...);  // Keep C-style variadic for now if preferred

void report_warning(const ArcSourceLocation *loc, const std::string &message);
void report_warning(const ArcSourceLocation *loc, const char *format, ...);

void report_info(const ArcSourceLocation *loc, const std::string &message);
void report_info(const ArcSourceLocation *loc, const char *format, ...);

ARC_NORETURN void report_fatal(const ArcSourceLocation *loc, ErrorCode code,
                               const std::string &message);
ARC_NORETURN void report_fatal(const ArcSourceLocation *loc, ErrorCode code, const char *format,
                               ...);

// --- String Utilities ---
// For string formatting, consider using <format> (C++20) or a library like fmtlib.
// This is a safe wrapper around snprintf.
std::string string_vformat(const char *format, std::va_list args);
std::string string_format(const char *format, ...);

// std::string has starts_with and ends_with in C++20.
// Here are C++17 compatible versions using string_view for efficiency.
inline bool str_starts_with(std::string_view str, std::string_view prefix) {
    return str.rfind(prefix, 0) == 0;  // or str.substr(0, prefix.length()) == prefix;
}

inline bool str_ends_with(std::string_view str, std::string_view suffix) {
    if (suffix.length() > str.length()) {
        return false;
    }
    return str.compare(str.length() - suffix.length(), suffix.length(), suffix) == 0;
}

// arc_strdup and arc_strndup are replaced by std::string construction.
// e.g., std::string new_str = old_c_str;
//       std::string new_str_n(old_c_str, n);

// --- File Utilities ---
std::optional<std::string> read_file_to_string(const std::filesystem::path &filepath);
bool file_exists(const std::filesystem::path &filepath);

// These can leverage std::filesystem
inline std::string get_file_extension(const std::filesystem::path &filepath) {
    return filepath.extension().string();
}

inline std::string get_basename(const std::filesystem::path &filepath) {
    return filepath.stem().string();  // .stem() for name without extension
                                      // .filename() for name with extension
}

inline std::string get_dirname(const std::filesystem::path &filepath) {
    if (filepath.has_parent_path()) {
        return filepath.parent_path().string();
    }
    return ".";  // Or empty string, depending on desired behavior
}

// --- Debug Macros ---
// Using std::cerr for output
#ifdef DEBUG
#define ARC_DEBUG_PRINT(level, file, line, func, ...)                                              \
    do {                                                                                           \
        std::cerr << "[" << level << "] " << file << ":" << line;                                  \
        if (func)                                                                                  \
            std::cerr << " in " << func << "()";                                                   \
        std::cerr << ": " << arc::string_format(__VA_ARGS__) << std::endl;                         \
    } while (0)

#define ARC_DEBUG(...) ARC_DEBUG_PRINT("DEBUG", __FILE__, __LINE__, nullptr, __VA_ARGS__)
#define ARC_TRACE(...) ARC_DEBUG_PRINT("TRACE", __FILE__, __LINE__, __func__, __VA_ARGS__)

#define ARC_ASSERT(condition, ...)                                                                 \
    do {                                                                                           \
        if (!(condition)) {                                                                        \
            std::cerr << "[ASSERT] " << __FILE__ << ":" << __LINE__                                \
                      << ": Condition '" #condition "' failed. "                                   \
                      << arc::string_format(__VA_ARGS__) << std::endl;                             \
            std::abort();                                                                          \
        }                                                                                          \
    } while (0)
#else
#define ARC_DEBUG(...) ((void)0)
#define ARC_TRACE(...) ((void)0)
#define ARC_ASSERT(condition, ...) assert(condition)  // Uses standard assert
#endif

// --- Common Constants ---
// Using static constexpr for type safety and compile-time evaluation
static constexpr size_t MAX_IDENTIFIER_LENGTH = 256;
static constexpr size_t MAX_STRING_LENGTH = 4096;  // For legacy checks, if any
// Constants for std::vector or std::unordered_map pre-allocation can be defined
// if specific default capacities are desired, e.g.:
// static constexpr size_t DEFAULT_TOKEN_VECTOR_CAPACITY = 100;
// static constexpr size_t DEFAULT_SYMBOL_TABLE_BUCKETS = 64;

// Note: ARC_ARRAY_SIZE, ARC_MAX, ARC_MIN, ARC_CLAMP macros are removed.
// Use:
// - std::size(arr) (for C-style arrays, C++17) or container.size()
// - std::max(a, b)
// - std::min(a, b)
// - std::clamp(value, low, high) (C++17)
// All from <algorithm>

}  // namespace arc
