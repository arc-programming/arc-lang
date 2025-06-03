# Arc Language Development Tools
message(STATUS "Configuring Arc development tools")

# Future tools will go here:
# add_executable(arc_fmt arc_fmt.c)
# add_executable(arc_lint arc_lint.c)

# For now, just a placeholder
add_custom_target(tools_placeholder
    COMMAND ${CMAKE_COMMAND} -E echo "Arc tools will be built here"
    COMMENT "Placeholder for Arc development tools"
)