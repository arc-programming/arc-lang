# Arc Language Benchmarks
message(STATUS "Configuring Arc benchmarks")

# Future benchmarks will go here:
# add_executable(lexer_bench lexer_bench.c)
# add_executable(parser_bench parser_bench.c)

# For now, just a placeholder
add_custom_target(benchmarks_placeholder
    COMMAND ${CMAKE_COMMAND} -E echo "Arc benchmarks will be built here"
    COMMENT "Placeholder for Arc benchmarks"
)