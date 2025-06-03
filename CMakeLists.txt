cmake_minimum_required(VERSION 3.16)

project(ArcLang
    VERSION 0.0.1
    DESCRIPTION "The Arc Programming Language Project"
    LANGUAGES C)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Set the C standard for the whole project
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF) # Prefer to be explicit

# Default build type if none is specified (e.g., Debug, Release, RelWithDebInfo, MinSizeRel)
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build." FORCE)
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS Debug Release RelWithDebInfo MinSizeRel)
endif()

# Output directories for executables and libraries
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# Add a message to show the build type
message(STATUS "Building ArcLang with CMAKE_BUILD_TYPE: ${CMAKE_BUILD_TYPE}")

# --- Subdirectories ---
# Add the compiler subdirectory. CMake will look for another CMakeLists.txt there.
add_subdirectory(compiler)

# --- Optional: Global Options (can be added later) ---
# option(ARC_BUILD_TESTS "Build Arc language tests" ON)
# option(ARC_BUILD_EXAMPLES "Build Arc language examples" ON)
# option(ARC_BUILD_TOOLS "Build Arc development tools" ON)

# --- Optional: Find LLVM (can be added later when you need codegen) ---
# find_package(LLVM REQUIRED CONFIG)
# if(LLVM_FOUND)
#     message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
#     message(STATUS "Using LLVMConfig.cmake in: ${LLVM_CMAKE_DIR}")
#     include_directories(SYSTEM ${LLVM_INCLUDE_DIRS})
#     link_directories(${LLVM_LIBRARY_DIRS})
#     add_definitions(${LLVM_DEFINITIONS})
# else()
#     message(FATAL_ERROR "LLVM not found. Please set LLVM_DIR or install LLVM.")
# endif()

# --- Optional: Enable Testing (can be added later) ---
# if(ARC_BUILD_TESTS)
#     enable_testing()
#     # add_subdirectory(tests) # If you have a top-level tests directory for integration tests
# endif()

# --- Optional: Installation (can be added later) ---
# include(GNUInstallDirs)
# install(TARGETS arc # Assuming 'arc' is the compiler executable
#     RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
#     LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
#     ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
# )