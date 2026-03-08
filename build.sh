# 1. Nuclear Clean
rm -rf build

# 2. Set the paths discovered by your 'find' command
export LLVM_ROOT="/usr/local/opt/llvm"
export LLVM_CXX_LIB="${LLVM_ROOT}/lib/c++"
export SDK_PATH=$(xcrun --sdk macosx --show-sdk-path)

# 3. Configure with the correct "Homebrew-First" flags
cmake -S . -B build -G Ninja \
  -DCMAKE_CXX_COMPILER="${LLVM_ROOT}/bin/clang++" \
  -DCMAKE_C_COMPILER="${LLVM_ROOT}/bin/clang" \
  -DCMAKE_CXX_FLAGS="-stdlib=libc++ -isysroot ${SDK_PATH}" \
  -DCMAKE_EXE_LINKER_FLAGS="-L${LLVM_CXX_LIB} -L${LLVM_ROOT}/lib -lc++ -Wl,-rpath,${LLVM_CXX_LIB},-rpath,${LLVM_ROOT}/lib" \
  -DCMAKE_CXX_SCAN_FOR_MODULES=ON

# 4. Build
cmake --build build
