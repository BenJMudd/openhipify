# Openhipify

Openhipify is an OpenCL to HIP transpiler, currently still in development.
This can be used for OpenCL C translation (i.e. OpenCL kernels), and for the OpenCL 3.0
C API (C++ API is currently not supported).

## Installation

Openhipify uses LibTooling in clang, and requires LLVM to build and use.

1. Download the LLVM source from the [LLVM github](https://github.com/llvm/llvm-project/releases).
2. Clone the openhipify repository into the tools section:

```bash
cd /llvm_project/clang/tools
git clone https://github.com/benjmudd/openhipify
```

3. Proceed with standard installation process (Found on [LibTooling](https://clang.llvm.org/docs/LibASTMatchersTutorial.html) documentation)

```bash
cd /llvm_project
echo "add_clang_subdirectory(openhipify)" > clang/tools/CMakeLists.txt
mkdir build && cd build
cmake -G Ninja ../llvm-project/llvm -DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra" -DCMAKE_BUILD_TYPE=Release -DLLVM_BUILD_TESTS=ON
ninja openhipify
```

The binary will be produced in `/llvm_project/build/bin`

## Usage

Openhipify can be invoked on OpenCL kernel files (.cl extentions) and OpenCL host
files using the OpenCL 3.0 C API.

```bash
# Kernel example
openhipify kernel.cl --
#host example
openhipify host.c --
#dual example
openhipify host.c kernel.cl --
```

Multiple host and kernel files can be inputted in any order into the tool. Each will be
translated into a HIP file with the same name, with an extra .cpp extention (i.e. kernel.cl -> kernel.cl.cpp)

Examples of full translation can be found at the
[Openipify test suite](https://github.com/BenJMudd/openhipify-test-suite).
