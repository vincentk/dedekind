# jlt-tool: the Jlt checker's checks that need code rather than a pattern
# (#1060).  A standalone LibTooling binary on clang's shared library, run on
# the compilation database like clang-query; built against the same LLVM that
# built the tree (Clang_DIR / LLVM_DIR point at it).
cmake_minimum_required(VERSION 3.25)
project(jlt-tool C CXX)  # C too: LLVM's CMake modules probe headers in C
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(LLVM REQUIRED CONFIG)
find_package(Clang REQUIRED CONFIG)
message(STATUS "jlt-tool: LLVM ${LLVM_PACKAGE_VERSION} at ${LLVM_DIR}")

add_executable(jlt-tool jlt_tool.cpp)
target_include_directories(jlt-tool SYSTEM PRIVATE ${LLVM_INCLUDE_DIRS} ${CLANG_INCLUDE_DIRS})
target_compile_definitions(jlt-tool PRIVATE ${LLVM_DEFINITIONS})
if(NOT LLVM_ENABLE_RTTI)
  target_compile_options(jlt-tool PRIVATE -fno-rtti)
endif()
# The shared clang library when the installation has it (Homebrew, apt);
# the component libraries otherwise.
if(TARGET clang-cpp)
  target_link_libraries(jlt-tool PRIVATE clang-cpp LLVM)
else()
  target_link_libraries(jlt-tool PRIVATE clangTooling clangFrontend clangAnalysis
    clangAST clangBasic clangLex clangSerialization clangDriver LLVMSupport)
endif()
