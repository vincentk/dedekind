# The Jlt linter in the build (#1060): `lint` lints the whole scope in one pass
# once everything is built, into build/jlt-lint/report.txt; lint-<part> lints
# one part of it (a layer, or the fixtures).  Which line belongs to which part, jlt_lint.py
# says (part_of); the build only names the parts.
option(DEDEKIND_JLT_LINT "Lint the tree with tools/jlt-lint once it is built" ON)
if(NOT DEDEKIND_JLT_LINT)
    return()
endif()

# The LLVM that compiles the tree: its clang-query, and its CMake packages for
# the linter's own binary.  Either can be named instead (-DDEDEKIND_CLANG_QUERY,
# -DClang_DIR, -DLLVM_DIR).
file(REAL_PATH "${CMAKE_CXX_COMPILER}" _jlt_compiler)
cmake_path(GET _jlt_compiler PARENT_PATH _jlt_bin)
cmake_path(GET _jlt_bin PARENT_PATH _jlt_llvm)
find_program(DEDEKIND_CLANG_QUERY clang-query HINTS ${_jlt_bin})
find_path(Clang_DIR ClangConfig.cmake HINTS ${_jlt_llvm}/lib/cmake/clang NO_DEFAULT_PATH)
find_path(LLVM_DIR LLVMConfig.cmake HINTS ${_jlt_llvm}/lib/cmake/llvm NO_DEFAULT_PATH)

# Not found: `lint` says so and fails, rather than pass having linted nothing.
if(NOT DEDEKIND_CLANG_QUERY OR NOT Clang_DIR OR NOT LLVM_DIR)
    set(_jlt_missing "jlt-lint: no clang-query or Clang/LLVM CMake packages next to ${_jlt_compiler} (DEDEKIND_CLANG_QUERY=${DEDEKIND_CLANG_QUERY}, Clang_DIR=${Clang_DIR}, LLVM_DIR=${LLVM_DIR}); name them, or configure with -DDEDEKIND_JLT_LINT=OFF")
    message(WARNING "${_jlt_missing}")
    add_custom_target(lint
        COMMAND ${CMAKE_COMMAND} -E echo "${_jlt_missing}"
        COMMAND ${CMAKE_COMMAND} -E false
        VERBATIM)
    return()
endif()

# The linter's binary (tool/), a project of its own on LLVM's flags, built by
# the same compiler as the tree.
include(ExternalProject)
set(_jlt_tool ${CMAKE_BINARY_DIR}/jlt-lint/jlt-lint)
set(_jlt_tool_args
    -DCMAKE_BUILD_TYPE=Release
    -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
    -DClang_DIR=${Clang_DIR}
    -DLLVM_DIR=${LLVM_DIR})
if(CMAKE_C_COMPILER)
    list(APPEND _jlt_tool_args -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER})
endif()
# A binary built earlier from the same tool sources and LLVM (CI keeps one in
# its cache) is used as it is: the lint of each part then waits for that part
# only, not for the tool's own build, which compiles LibTooling's headers for
# many minutes and so held every part back until the compilation had ended.
set(DEDEKIND_JLT_LINT_TOOL "" CACHE FILEPATH
    "A jlt-lint binary built from these tool sources and this LLVM; empty builds it")
if(DEDEKIND_JLT_LINT_TOOL AND EXISTS "${DEDEKIND_JLT_LINT_TOOL}")
    set(_jlt_tool ${DEDEKIND_JLT_LINT_TOOL})
    add_custom_target(jlt-lint-tool)
else()
    ExternalProject_Add(jlt-lint-tool
        SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/tool
        BINARY_DIR ${CMAKE_BINARY_DIR}/jlt-lint
        CMAKE_ARGS ${_jlt_tool_args}
        INSTALL_COMMAND ""
        BUILD_ALWAYS ON
        BUILD_BYPRODUCTS ${_jlt_tool}
        EXCLUDE_FROM_ALL ON)
endif()

if(NOT Python_EXECUTABLE)
    find_package(Python COMPONENTS Interpreter REQUIRED)
endif()

# One part at a time (each parses NCPU files at once), beside the compiler.
set_property(GLOBAL APPEND PROPERTY JOB_POOLS jlt_lint=1)
set(_jlt_parts ${CMAKE_BINARY_DIR}/jlt-lint/parts)

# lint-<PART>, after the targets whose module maps its files are parsed with.
function(add_jlt_lint_part PART)
    add_custom_target(lint-${PART}
        COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/jlt_lint.py
                ${DEDEKIND_CLANG_QUERY} ${CMAKE_BINARY_DIR} ${_jlt_tool}
                --part ${PART} --out ${_jlt_parts}
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        JOB_POOL jlt_lint
        VERBATIM)
    add_dependencies(lint-${PART} jlt-lint-tool ${ARGN})
    set_property(GLOBAL APPEND PROPERTY DEDEKIND_LINT_PARTS lint-${PART})
endfunction()

# A layer's files: its library, and its tests where it has them.  lint-<PART>
# lints one part, for a look at one layer.
set(_jlt_all_built)
get_property(_jlt_layers GLOBAL PROPERTY DEDEKIND_LAYERS)
foreach(layer IN LISTS _jlt_layers)
    set(_jlt_built dedekind_${layer})
    if(TARGET test_${layer})
        list(APPEND _jlt_built test_${layer})
    endif()
    add_jlt_lint_part(${layer} ${_jlt_built})
    list(APPEND _jlt_all_built ${_jlt_built})
endforeach()
# The fixtures are standalone: they need only the linter.
add_jlt_lint_part(fixtures)

# The whole scope in one pass, once everything is built: one pool of NCPU
# workers over every file, so no part waits for another part's last files.
# (Part by part, in sequence, the lint took four times as long: each part's
# pool idled on its tail, and a small layer used few of the cores.)
add_custom_target(lint
    # The report is this run's or none: an earlier pass's report goes first.
    COMMAND ${CMAKE_COMMAND} -E rm -f ${CMAKE_BINARY_DIR}/jlt-lint/report.txt
    COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/jlt_lint.py
            ${DEDEKIND_CLANG_QUERY} ${CMAKE_BINARY_DIR} ${_jlt_tool} --out ${_jlt_parts}
    COMMAND ${CMAKE_COMMAND} -E copy ${_jlt_parts}/all.txt ${CMAKE_BINARY_DIR}/jlt-lint/report.txt
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    VERBATIM)
add_dependencies(lint jlt-lint-tool ${_jlt_all_built})
