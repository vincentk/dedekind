# CMake generated Testfile for 
# Source directory: /home/runner/work/dedekind/dedekind
# Build directory: /home/runner/work/dedekind/dedekind/build2
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test([=[test_category]=] "/home/runner/work/dedekind/dedekind/build2/test_category")
set_tests_properties([=[test_category]=] PROPERTIES  ENVIRONMENT "LLVM_PROFILE_FILE=/home/runner/work/dedekind/dedekind/build/dedekind-category-%m.profraw" _BACKTRACE_TRIPLES "/home/runner/work/dedekind/dedekind/CMakeLists.txt;105;add_test;/home/runner/work/dedekind/dedekind/CMakeLists.txt;124;add_dedekind_layer;/home/runner/work/dedekind/dedekind/CMakeLists.txt;0;")
subdirs("_deps/catch2-build")
