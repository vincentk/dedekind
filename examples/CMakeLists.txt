# CMakeLists.txt for dedekind examples
# Builds standalone demonstration and validation examples

cmake_minimum_required(VERSION 3.28)

# Example: ISO-3166 country code disjoint set intersection
add_executable(set-pruning-ir-demo
    set-pruning-ir-demo/iso3166_countries.cpp)

# IR fixture: compile-time pruning witness via dedekind.sets + 𝔹
# Links against the full dedekind library so module BMIs are available.
add_executable(set-pruning-ir-fixture
    set-pruning-ir-demo/pruning_noop_vs_runtime_fixture.cpp)

set_target_properties(set-pruning-ir-fixture PROPERTIES
    CXX_STANDARD 23
    CXX_STANDARD_REQUIRED ON
    CXX_EXTENSIONS OFF
    CXX_SCAN_FOR_MODULES ON)

target_compile_options(set-pruning-ir-fixture PRIVATE
    -Wall -Wextra -Wpedantic -Werror
    -Wno-c2y-extensions
    -O2)

target_link_libraries(set-pruning-ir-fixture PRIVATE dedekind)

set_target_properties(set-pruning-ir-demo PROPERTIES
    CXX_STANDARD 23
    CXX_STANDARD_REQUIRED ON
    CXX_EXTENSIONS OFF)

target_compile_options(set-pruning-ir-demo PRIVATE
    -Wall -Wextra -Wpedantic -Werror
    -Wno-c2y-extensions
    -O2)

# For IR inspection: compile with -emit-llvm
# Usage: cmake --build . --target set-pruning-ir-demo-llvm
add_custom_command(TARGET set-pruning-ir-demo POST_BUILD
    COMMAND ${CMAKE_CXX_COMPILER} -std=c++23 -O2 -emit-llvm -c
        ${CMAKE_SOURCE_DIR}/examples/set-pruning-ir-demo/iso3166_countries.cpp
        -o ${CMAKE_BINARY_DIR}/set-pruning-ir-demo.ll
    COMMENT "Emitting LLVM IR for set-pruning-ir-demo (inspect with llvm-dis)"
    VERBATIM)
