# Compiler Unit Tests Configuration

# Compiler unit test sources
set(COMPILER_TEST_SOURCES
    unit/test_lexer.c
)

# Create the compiler tests executable
add_executable(compiler_tests
    ${COMPILER_TEST_SOURCES}
    test_main.c
)

# Set up include directories
target_include_directories(compiler_tests PRIVATE
    ${CMAKE_SOURCE_DIR}/compiler/include/    # For arc/lexer.h, arc/common.hpp
    ${CMAKE_SOURCE_DIR}/tests/common/        # For test_framework.h
    ${unity_SOURCE_DIR}/src/                 # For unity.h
)

# Link libraries
target_link_libraries(compiler_tests
    arc_compiler_lib        # Your compiler library (now it exists!)
    arc_test_common         # Common test utilities
    unity                   # Unity test framework
)

# Add compiler options to suppress warnings
target_compile_options(compiler_tests PRIVATE
    $<$<C_COMPILER_ID:Clang>:-Wno-deprecated-declarations -Wno-macro-redefined>
    $<$<C_COMPILER_ID:GNU>:-Wno-deprecated-declarations>
    $<$<C_COMPILER_ID:MSVC>:/wd4996>
)

# Add Windows specific definitions
if(WIN32)
    target_compile_definitions(compiler_tests PRIVATE
        _CRT_SECURE_NO_WARNINGS
        _CRT_NONSTDC_NO_DEPRECATE
    )
endif()

# Register the test with CTest
add_test(NAME CompilerUnitTests COMMAND compiler_tests)

# Set test properties
set_tests_properties(CompilerUnitTests PROPERTIES
    TIMEOUT 30
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

# Copy test fixtures
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/fixtures")
    file(COPY fixtures/ DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/fixtures/)
else()
    message(STATUS "Creating test fixture directories")
    file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/fixtures/valid)
    file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/fixtures/invalid)
endif()