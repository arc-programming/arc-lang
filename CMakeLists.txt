cmake_minimum_required(VERSION 3.16)

project(ArcLang
    VERSION 0.0.1
    DESCRIPTION "The Arc Programming Language Project"
    LANGUAGES C)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Set the C standard for the whole project
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

# Default build type
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build." FORCE)
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS Debug Release RelWithDebInfo MinSizeRel)
endif()

# Output directories
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

message(STATUS "Building ArcLang with CMAKE_BUILD_TYPE: ${CMAKE_BUILD_TYPE}")

# --- Build Options ---
option(ARC_BUILD_TESTS "Build Arc language tests" ON)
option(ARC_BUILD_EXAMPLES "Build Arc language examples" ON)
option(ARC_BUILD_TOOLS "Build Arc development tools" ON)
option(ARC_BUILD_BENCHMARKS "Build Arc benchmarks" ON)

# --- Subdirectories ---
add_subdirectory(compiler)

# --- Testing ---
if(ARC_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

# --- Examples ---
if(ARC_BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()

# --- Tools ---
if(ARC_BUILD_TOOLS)
    add_subdirectory(tools)
endif()

# --- Benchmarks ---
if(ARC_BUILD_BENCHMARKS)
    add_subdirectory(benchmarks)
endif()