# arc-lang/compiler/CMakeLists.txt

# --- Compiler Executable Name ---
set(COMPILER_EXECUTABLE_NAME arc)

# --- Source Files ---
set(COMPILER_SOURCES
    src/main.c
    # src/lexer/lexer.c
    # src/lexer/token.c
    # src/utils/memory.c
    # ... add more .c files here as you create them
)

# --- Create the Compiler Executable FIRST ---
add_executable(${COMPILER_EXECUTABLE_NAME} ${COMPILER_SOURCES})
message(STATUS "Configuring Arc Compiler: ${COMPILER_EXECUTABLE_NAME}") # Moved message up for clarity

# --- Include Directories ---
# Now that 'arc' target exists, we can modify it.
target_include_directories(${COMPILER_EXECUTABLE_NAME} PUBLIC
    "${CMAKE_CURRENT_SOURCE_DIR}/include" # For compiler's own headers like arc/lexer.h
)
# If you generate arc_config.h (e.g., for version info from CMake)
# target_include_directories(${COMPILER_EXECUTABLE_NAME} PRIVATE
#    "${CMAKE_CURRENT_BINARY_DIR}" # For generated headers like arc_config.h
# )

# --- Compiler Flags ---
set(ARC_C_FLAGS_LIST "") # Initialize as an empty list (or string, CMake is flexible here)

if(MSVC)
    # MSVC specific flags
    list(APPEND ARC_C_FLAGS_LIST "/W4" "/WX")
else()
    # GCC/Clang specific flags
    list(APPEND ARC_C_FLAGS_LIST
        "-Wall"
        "-Wextra"
        "-Wpedantic"
        "-Werror"
        "-Wno-unused-parameter"
    )
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        list(APPEND ARC_C_FLAGS_LIST "-g" "-O0")
    else() # For Release or other types
        list(APPEND ARC_C_FLAGS_LIST "-O2") # Or -O3, -Os
    endif()
endif()

# Apply the list of flags
target_compile_options(${COMPILER_EXECUTABLE_NAME} PRIVATE ${ARC_C_FLAGS_LIST})

# --- Link Libraries (Example: LLVM - add when needed) ---
# if(LLVM_FOUND) # Assuming LLVM_FOUND is set by the top-level CMakeLists.txt
#     target_link_libraries(${COMPILER_EXECUTABLE_NAME} PRIVATE ${LLVM_LIBRARIES})
#     if(UNIX AND NOT APPLE)
#         find_package(Threads REQUIRED)
#         target_link_libraries(${COMPILER_EXECUTABLE_NAME} PRIVATE Threads::Threads)
#     endif()
# else()
#     message(WARNING "LLVM not found by compiler/CMakeLists.txt. Codegen will not work if LLVM is expected.")
# endif()

# --- Optional: Compiler Tests (can be added later) ---
# if(ARC_BUILD_TESTS)
#     enable_testing()
#     add_subdirectory(tests)
# endif()

# --- Optional: Configuration Header (arc_config.h) ---
# configure_file(
#     "${CMAKE_CURRENT_SOURCE_DIR}/include/arc_config.h.in"
#     "${CMAKE_CURRENT_BINARY_DIR}/arc_config.h"
# )
# target_include_directories(${COMPILER_EXECUTABLE_NAME} PRIVATE
#     "${CMAKE_CURRENT_BINARY_DIR}"
# )