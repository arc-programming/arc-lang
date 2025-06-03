message(STATUS "Configuring Arc examples")

# For now, just copy any .arc files to build directory
file(GLOB ARC_EXAMPLES "*.arc")
if(ARC_EXAMPLES)
    message(STATUS "Found ${CMAKE_CURRENT_LIST_LENGTH} example files")
    file(COPY ${ARC_EXAMPLES} DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/)
else()
    message(STATUS "No example files found yet")
endif()

# Future: Add example compilation targets here
# add_custom_target(compile_examples 
#     COMMAND arc ${CMAKE_CURRENT_SOURCE_DIR}/hello_world.arc
#     DEPENDS arc
# )