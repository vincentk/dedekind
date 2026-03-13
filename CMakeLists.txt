cmake_minimum_required(VERSION 3.14)
project(Dedekind VERSION 1.0.0 LANGUAGES CXX)

# Force C++20 standard for macOS Clang
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
# Disable global scanning to protect FetchContent/dependencies
set(CMAKE_CXX_SCAN_FOR_MODULES OFF)

# 1. THE LIBRARY (src/main)
add_library(dedekind STATIC)

# Explicitly enable scanning only for this target
set_target_properties(dedekind PROPERTIES CXX_SCAN_FOR_MODULES ON)


# 2. Add the Module Interface explicitly
# IMPORTANT: The keyword 'FILE_SET' must follow 'PUBLIC' or 'INTERFACE' immediately
target_sources(dedekind
    PUBLIC 
    FILE_SET CXX_MODULES 
    BASE_DIRS src/main/modules
    FILES
        src/main/modules/dedekind/sets/cardinalities.cppm
#        src/main/modules/dedekind/sets/algebra.cppm
#        src/main/modules/dedekind/sets/universes.cppm
#        src/main/modules/dedekind/sets/expressions.cppm
#        src/main/modules/dedekind/sets/operators.cppm
#        src/main/modules/dedekind/sets/predicates.cppm
        src/main/modules/dedekind/sets/sets.cppm
)


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
    src/test/cpp/modules/dedekind/sets/cardinalities_test.cpp
    # src/test/cpp/dedekind/sets_test.cpp 
)

set_target_properties(dedekind_test PROPERTIES CXX_SCAN_FOR_MODULES ON)

target_link_libraries(dedekind_test PRIVATE dedekind Catch2::Catch2WithMain)

# Register the test with CTest (allows running 'ctest' from build folder)
include(Catch)
catch_discover_tests(dedekind_test)

install(TARGETS dedekind
    EXPORT dedekind-targets
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
    target_compile_options(dedekind PRIVATE -fprofile-instr-generate -fcoverage-mapping)
    target_link_options(dedekind PRIVATE -fprofile-instr-generate)
    
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
