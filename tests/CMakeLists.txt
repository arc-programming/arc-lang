# Download and configure a lightweight test framework
include(FetchContent)

# Using Unity test framework (lightweight and simple)
FetchContent_Declare(
    unity
    GIT_REPOSITORY https://github.com/ThrowTheSwitch/Unity.git
    GIT_TAG v2.5.2
)
FetchContent_MakeAvailable(unity)

# Common test utilities
add_library(arc_test_common STATIC
    common/test_helpers.c
    common/test_framework.c
)

target_include_directories(arc_test_common PUBLIC
    common/
    ${unity_SOURCE_DIR}/src/
    ${CMAKE_SOURCE_DIR}/compiler/  # Access to compiler headers
)

target_link_libraries(arc_test_common 
    unity
)

# Add subdirectories for different test suites
add_subdirectory(compiler)
add_subdirectory(stdlib)

# Custom target to run all tests
add_custom_target(test_all
    COMMAND ${CMAKE_CTEST_COMMAND} --verbose
    DEPENDS compiler_tests stdlib_tests
    COMMENT "Running all Arc tests"
)