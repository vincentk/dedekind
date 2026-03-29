set(GRAPHVIZ_EXECUTABLES FALSE)
set(GRAPHVIZ_STATIC_LIBS TRUE)
set(GRAPHVIZ_MODULE_LIBS TRUE) # Refers to CMake MODULE libraries, not C++ modules

# Example: Exclude external libraries and private targets
set(GRAPHVIZ_EXTERNAL_LIBS FALSE)
set(GRAPHVIZ_IGNORE_TARGETS ".*_test")
