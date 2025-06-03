# Arc Programming Language

A systems programming language designed for performance-critical applications requiring explicit resource control and predictable execution characteristics.

## Building the Compiler

Arc requires LLVM for code generation. Make sure you have LLVM 15+ installed before building.

### Prerequisites

- **LLVM 15+**: Required for code generation backend
- **CMake 3.20+**: Build system
- **C17 compatible compiler**: GCC, Clang, or MSVC

### Quick Build

**Windows:**
```cmd
build.bat
```

**Unix/Linux/macOS:**
```bash
chmod +x build.sh
./build.sh
```

### Manual Build

```bash
mkdir build && cd build
cmake -DCMAKE_BUILD_TYPE=Debug ..
cmake --build .
```

### Installing LLVM

**Ubuntu/Debian:**
```bash
sudo apt update
sudo apt install llvm-dev libllvm15-dev
```

**Windows (vcpkg):**
```cmd
vcpkg install llvm[tools]
```

**macOS:**
```bash
brew install llvm
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