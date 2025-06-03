# Arc Language Test Configuration
cmake_minimum_required(VERSION 3.16)

# Download and configure Unity test framework
include(FetchContent)

FetchContent_Declare(
    unity
    GIT_REPOSITORY https://github.com/ThrowTheSwitch/Unity.git
    GIT_TAG v2.5.2
)

# Configure Unity before making it available
set(UNITY_EXTENSION_MEMORY OFF CACHE BOOL "Disable Unity Memory extension")
set(UNITY_EXTENSION_FIXTURE OFF CACHE BOOL "Disable Unity Fixture extension")

FetchContent_MakeAvailable(unity)

# Configure Unity to suppress warnings
if(TARGET unity)
    target_compile_options(unity PRIVATE
        $<$<C_COMPILER_ID:Clang>:-Wno-error -Wno-missing-noreturn>
        $<$<C_COMPILER_ID:GNU>:-Wno-error -Wno-missing-noreturn>
        $<$<C_COMPILER_ID:MSVC>:/wd4996>
    )
    
    # Add Windows specific definitions to Unity
    if(WIN32)
        target_compile_definitions(unity PRIVATE
            _CRT_SECURE_NO_WARNINGS
            _CRT_NONSTDC_NO_DEPRECATE
        )
    endif()
endif()

# Common test utilities library
add_library(arc_test_common STATIC
    common/test_framework.c
    common/test_helpers.c
)

target_include_directories(arc_test_common PUBLIC
    common/
    ${unity_SOURCE_DIR}/src/
    ${CMAKE_SOURCE_DIR}/compiler/        # Main compiler headers
    ${CMAKE_SOURCE_DIR}/compiler/include/ # If you have an include subdirectory
)

target_link_libraries(arc_test_common PUBLIC
    unity
)

# Add Windows specific definitions
if(WIN32)
    target_compile_definitions(arc_test_common PRIVATE
        _CRT_SECURE_NO_WARNINGS
        _CRT_NONSTDC_NO_DEPRECATE
    )
endif()

# Add compiler subdirectory
add_subdirectory(compiler)

# Add stdlib tests if they exist
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/stdlib/CMakeLists.txt")
    add_subdirectory(stdlib)
    set(STDLIB_TESTS_TARGET stdlib_tests)
else()
    message(STATUS "Stdlib tests not found, skipping")
    set(STDLIB_TESTS_TARGET "")
endif()

# Custom target to run all tests
add_custom_target(test_all
    COMMAND ${CMAKE_CTEST_COMMAND} --verbose --output-on-failure
    DEPENDS compiler_tests ${STDLIB_TESTS_TARGET}
    COMMENT "Running all Arc tests"
)