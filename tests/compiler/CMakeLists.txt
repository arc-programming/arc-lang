# Compiler unit tests
set(COMPILER_TEST_SOURCES
    unit/test_lexer.c
    unit/test_parser.c
    unit/test_ast.c
    unit/test_error_reporting.c
    unit/test_source_location.c
)

# Create test executable
add_executable(compiler_tests
    ${COMPILER_TEST_SOURCES}
    test_main.c  # Main function for all compiler tests
)

target_include_directories(compiler_tests PRIVATE
    ${CMAKE_SOURCE_DIR}/compiler/
    ../common/
    ${unity_SOURCE_DIR}/src/
)

target_link_libraries(compiler_tests
    arc_compiler_lib  # Your compiler library
    arc_test_common
    unity
)

# Integration tests (separate executable)
add_executable(compiler_integration_tests
    integration/test_full_compilation.c
    integration/test_error_cases.c
    integration_test_main.c
)

target_include_directories(compiler_integration_tests PRIVATE
    ${CMAKE_SOURCE_DIR}/compiler/
    ../common/
    ${unity_SOURCE_DIR}/src/
)

target_link_libraries(compiler_integration_tests
    arc_compiler_lib
    arc_test_common
    unity
)

# Register tests with CTest
add_test(NAME CompilerUnitTests COMMAND compiler_tests)
add_test(NAME CompilerIntegrationTests COMMAND compiler_integration_tests)

# Copy test fixtures
file(COPY fixtures/ DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/fixtures/)