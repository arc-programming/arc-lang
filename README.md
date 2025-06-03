# Arc Programming Language

A systems programming language designed for performance-critical applications requiring explicit resource control and predictable execution characteristics.

## Building the Compiler

Arc requires LLVM for code generation. Make sure you have LLVM 15+ installed before building.

### Prerequisites
- **CMake**
- **C Compiler (e.g, clang, gcc, msvc)**

### Manual Build

```bash
mkdir build && cd build
cmake -DCMAKE_BUILD_TYPE=Debug ..
cmake --build .
```



## Project Structure

- `compiler/` - Arc compiler implementation (C)
- `examples/` - Example Arc programs
- `tests/` - Compiler and language tests
- `tools/` - Development tools (LSP, formatter, etc.)
- `docs/` - Documentation
- `benchmarks/` - Performance benchmarks
- `SPECIFICATION.md` - Language specification

## License

See LICENSE file for details.