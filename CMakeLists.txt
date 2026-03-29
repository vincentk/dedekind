cmake_minimum_required(VERSION 3.28)
project(Dedekind VERSION 1.0.0 LANGUAGES CXX)

# Force C++23 standard for Clang 21
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_EXTENSIONS OFF)
# Disable global scanning to protect FetchContent/dependencies
set(CMAKE_CXX_SCAN_FOR_MODULES OFF)

# For clangd:
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# 1. THE LIBRARY (src/main)
#add_library(dedekind STATIC)

# Explicitly enable scanning only for this target
#set_target_properties(dedekind PROPERTIES CXX_SCAN_FOR_MODULES ON)

# --- 1a. THE CATEGORY LAYER (Levels -1 to 0b) ---
add_library(dedekind_category OBJECT)
set_target_properties(dedekind_category PROPERTIES CXX_SCAN_FOR_MODULES ON)

target_sources(dedekind_category
    PUBLIC 
    FILE_SET CXX_MODULES 
    BASE_DIRS src/main/modules
    FILES
        src/main/modules/dedekind/category/category.cppm
        src/main/modules/dedekind/category/logic.cppm
        src/main/modules/dedekind/category/species.cppm
        src/main/modules/dedekind/category/functorial.cppm
        src/main/modules/dedekind/category/kleisli.cppm
        src/main/modules/dedekind/category/cartesian.cppm
        src/main/modules/dedekind/category/posetal.cppm
        src/main/modules/dedekind/category/small.cppm
        src/main/modules/dedekind/category/etcs.cppm
)
target_compile_options(dedekind_category PRIVATE ${DEDEKIND_WARNING_FLAGS})

# --- 1b. THE ONTOLOGY LAYER (Level 1+) ---
add_library(dedekind_ontology OBJECT)
set_target_properties(dedekind_ontology PROPERTIES CXX_SCAN_FOR_MODULES ON)

target_sources(dedekind_ontology
    PUBLIC 
    FILE_SET CXX_MODULES 
    BASE_DIRS src/main/modules
    FILES
        src/main/modules/dedekind/ontology/ontology.cppm
        src/main/modules/dedekind/ontology/mereology.cppm
        src/main/modules/dedekind/ontology/order.cppm
        src/main/modules/dedekind/ontology/topology.cppm
        src/main/modules/dedekind/ontology/sequences.cppm
        src/main/modules/dedekind/ontology/algebra.cppm
        src/main/modules/dedekind/ontology/morphologies.cppm
        src/main/modules/dedekind/ontology/geometry.cppm
        src/main/modules/dedekind/ontology/numbers.cppm
)
# Link ontology to category so Level 1+ can 'import dedekind.category'
target_link_libraries(dedekind_ontology PUBLIC dedekind_category)
target_compile_options(dedekind_ontology PRIVATE ${DEDEKIND_WARNING_FLAGS})

# --- 2. THE SETS LAYER ---
add_library(dedekind_sets OBJECT)
set_target_properties(dedekind_sets PROPERTIES CXX_SCAN_FOR_MODULES ON)

target_sources(dedekind_sets
    PUBLIC 
    FILE_SET CXX_MODULES 
    BASE_DIRS src/main/modules
    FILES
        src/main/modules/dedekind/sets/sets.cppm
        src/main/modules/dedekind/sets/boundaries.cppm
        src/main/modules/dedekind/sets/family.cppm
        src/main/modules/dedekind/sets/singleton.cppm
)
target_link_libraries(dedekind_sets PUBLIC dedekind_ontology)
target_compile_options(dedekind_sets PRIVATE ${DEDEKIND_WARNING_FLAGS})

# --- 3. THE FINAL ASSEMBLY (The Public Interface) ---

# The book of math according to the dedekind project.
# Each chapter corresponds the book to an object file which in turn
# corresponds to a module interface unit.
# The "dedekind" library is the final product
add_library(dedekind INTERFACE)
add_dependencies(dedekind dedekind_ontology dedekind_sets)
# No source files needed here; it just bundles the objects
target_link_libraries(dedekind INTERFACE dedekind_ontology dedekind_sets)
target_compile_features(dedekind INTERFACE cxx_std_23)

# 2. THE TESTS (src/test) - Maven's "test" phase
include(FetchContent)
FetchContent_Declare(
  Catch2
  GIT_REPOSITORY https://github.com/catchorg/Catch2.git
  GIT_TAG        v3.4.0
  #DOWNLOAD_EXTRACT_TIMESTAMP true
)
# For macOS compatibility, ensure we don't override global compiler settings
#set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(Catch2)

enable_testing()

add_executable(dedekind_test 
    src/test/cpp/modules/dedekind/category/category_test.cpp
    src/test/cpp/modules/dedekind/ontology/mereology_test.cpp
    src/test/cpp/modules/dedekind/sets/boundaries_test.cpp
    src/test/cpp/modules/dedekind/sets/singleton_test.cpp
#    src/test/cpp/modules/dedekind/sets/expressions_test.cpp
)

# --- 1. RIGOROUS COMPILER ENFORCEMENT ---
if(MSVC)
    # Windows/MSVC flags
    target_compile_options(dedekind PRIVATE /W4 /WX /permissive-)
    target_compile_options(dedekind_test PRIVATE /W4 /WX /permissive-)
else()
    # Clang/GCC flags
    set(DEDEKIND_WARNING_FLAGS
        -Wall          # Essential warnings
        -Wextra        # Even more warnings
        -Wpedantic     # Strict ISO C++ compliance
        -Wshadow       # Warn if a local variable hides a member
        -Wunused       # Catch unused variables/imports
        -Wunreachable-code # Catch logic that can never execute
        -Wconversion   # Warn on implicit type conversions (e.g. long to int)
        -Werror        # Treat warnings as errors (The "Strict Math" mode)
    )
    
    target_compile_options(dedekind INTERFACE ${DEDEKIND_WARNING_FLAGS})
    target_compile_options(dedekind_test PRIVATE ${DEDEKIND_WARNING_FLAGS})
endif()

set_target_properties(dedekind_test PROPERTIES CXX_SCAN_FOR_MODULES ON)

target_link_libraries(dedekind_test PRIVATE dedekind dedekind_category dedekind_ontology dedekind_sets Catch2::Catch2WithMain)
# Register the test with CTest (allows running 'ctest' from build folder)
include(Catch)
catch_discover_tests(dedekind_test)

install(TARGETS dedekind dedekind_category dedekind_ontology dedekind_sets
    EXPORT dedekind-targets
    # Ensure CXX_MODULES are installed for all targets
    FILE_SET CXX_MODULES DESTINATION lib/modules/dedekind
)


install(EXPORT dedekind-targets
    FILE dedekindConfig.cmake
    NAMESPACE dedekind::
    DESTINATION lib/cmake/dedekind
)

# --- Doxygen Documentation (Maven-style) ---
find_package(Doxygen)
if (DOXYGEN_FOUND)
    # Define where Doxygen looks for code and where it outputs HTML
    set(DOXYGEN_INPUT_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src/main/modules)
    set(DOXYGEN_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/docs)

    # Customise for C++20 Modules (requires manual Doxyfile settings if needed)
    set(DOXYGEN_EXTENSION_MAPPING "cppm=C++") # Treat .cppm files as C++
    set(DOXYGEN_FILE_PATTERNS "*.cppm" "*.cpp")

    # Hide symbols not marked with 'export' or documented specifically
    set(DOXYGEN_EXTRACT_ALL NO)
    set(DOXYGEN_EXTRACT_PRIVATE NO)
    set(DOXYGEN_EXTRACT_STATIC NO)

    # Automatically pulls "1.0.0" from the project() call above
    set(DOXYGEN_PROJECT_NUMBER ${PROJECT_VERSION})
    
    # Optional: Add a project brief (like a Maven description)
    set(DOXYGEN_PROJECT_BRIEF "A modern C++20 Module Library")
    
    # Use the FindDoxygen built-in macro to configure a Doxyfile automatically
    doxygen_add_docs(docs
        ${DOXYGEN_INPUT_DIR}
        COMMENT "Generating API documentation with Doxygen"
    )
    
endif()


# Add coverage flags for Clang
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    target_compile_options(dedekind INTERFACE -fprofile-instr-generate -fcoverage-mapping)
    target_link_options(dedekind INTERFACE -fprofile-instr-generate)
    
    target_compile_options(dedekind_test PRIVATE -fprofile-instr-generate -fcoverage-mapping)
    target_link_options(dedekind_test PRIVATE -fprofile-instr-generate)
endif()


# Python bindings using nanobind:
#find_package(Python 3.13 REQUIRED COMPONENTS Interpreter Development)

# Determine nanobind's location
#execute_process(
#    COMMAND "${Python_EXECUTABLE}" -m nanobind --cmake_dir
#    OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE nanobind_ROOT)
#
#list(APPEND CMAKE_PREFIX_PATH "${nanobind_ROOT}")

#find_package(nanobind CONFIG REQUIRED)
# Remove: find_package(nanobind CONFIG REQUIRED)
#add_subdirectory(ext/nanobind)

#nanobind_add_module(dedekind_py src/main/bindings.cpp)

# 3. CRITICAL: Disable scanning for the Python binding target
# AppleClang can 'import' the .pcm file if it's linked, 
# but CMake's automatic 'scanner' doesn't know how to ask AppleClang for the graph yet.
#set_target_properties(dedekind_py PROPERTIES CMAKE_CXX_SCAN_FOR_MODULES OFF)

#target_link_libraries(dedekind_py PRIVATE dedekind)
