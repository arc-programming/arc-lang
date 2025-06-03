# arc-lang/compiler/CMakeLists.txt

# --- Compiler Executable Name ---
set(COMPILER_EXECUTABLE_NAME arc)

# --- Source Files ---
# Separate library sources from main
set(COMPILER_LIB_SOURCES
    src/common.c
    src/lexer.c
    src/parser.c
    # src/lexer/token.c
    # src/utils/memory.c
    # ... add more .c files here as you create them (but NOT main.c)
)

set(COMPILER_MAIN_SOURCES
    src/main.c
)

# --- Create the Compiler Library FIRST ---
# This library contains all the compiler functionality except main()
add_library(arc_compiler_lib STATIC ${COMPILER_LIB_SOURCES})
message(STATUS "Configuring Arc Compiler Library: arc_compiler_lib")

# --- Include Directories for Library ---
target_include_directories(arc_compiler_lib PUBLIC
    "${CMAKE_CURRENT_SOURCE_DIR}/include" # For compiler's own headers like arc/lexer.h
)

# --- Create the Compiler Executable ---
add_executable(${COMPILER_EXECUTABLE_NAME} ${COMPILER_MAIN_SOURCES})
message(STATUS "Configuring Arc Compiler Executable: ${COMPILER_EXECUTABLE_NAME}")

# --- Include Directories for Executable ---
target_include_directories(${COMPILER_EXECUTABLE_NAME} PRIVATE
    "${CMAKE_CURRENT_SOURCE_DIR}/include"
)

# --- Link the library to the executable ---
target_link_libraries(${COMPILER_EXECUTABLE_NAME} PRIVATE arc_compiler_lib)

# --- Compiler Flags ---
set(ARC_C_FLAGS_LIST "") # Initialize as an empty list

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

# Apply the list of flags to both library and executable
target_compile_options(arc_compiler_lib PRIVATE ${ARC_C_FLAGS_LIST})
target_compile_options(${COMPILER_EXECUTABLE_NAME} PRIVATE ${ARC_C_FLAGS_LIST})

# --- Windows specific definitions ---
if(WIN32)
    target_compile_definitions(arc_compiler_lib PRIVATE
        _CRT_SECURE_NO_WARNINGS
        _CRT_NONSTDC_NO_DEPRECATE
    )
    target_compile_definitions(${COMPILER_EXECUTABLE_NAME} PRIVATE
        _CRT_SECURE_NO_WARNINGS
        _CRT_NONSTDC_NO_DEPRECATE
    )
endif()

# --- Link Libraries (Example: LLVM - add when needed) ---
# if(LLVM_FOUND) # Assuming LLVM_FOUND is set by the top-level CMakeLists.txt
#     target_link_libraries(arc_compiler_lib PRIVATE ${LLVM_LIBRARIES})
#     if(UNIX AND NOT APPLE)
#         find_package(Threads REQUIRED)
#         target_link_libraries(arc_compiler_lib PRIVATE Threads::Threads)
#     endif()
# else()
#     message(WARNING "LLVM not found by compiler/CMakeLists.txt. Codegen will not work if LLVM is expected.")
# endif()

# --- Optional: Configuration Header (arc_config.h) ---
# configure_file(
#     "${CMAKE_CURRENT_SOURCE_DIR}/include/arc_config.h.in"
#     "${CMAKE_CURRENT_BINARY_DIR}/arc_config.h"
# )
# target_include_directories(arc_compiler_lib PRIVATE
#     "${CMAKE_CURRENT_BINARY_DIR}"
# )