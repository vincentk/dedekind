# Install script for directory: /Users/vincent/git/github.com/vincentk/dedekind

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/var/folders/6h/wy8nq2q14jg7pgtzy16hhk840000gn/T/tmpgcslxzgw/wheel/platlib")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "Release")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

# Set path to fallback-tool for dependency-resolution.
if(NOT DEFINED CMAKE_OBJDUMP)
  set(CMAKE_OBJDUMP "/usr/local/opt/llvm/bin/llvm-objdump")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/Users/vincent/git/github.com/vincentk/dedekind/build-python/cp313-cp313-macosx_15_0_x86_64/_deps/catch2-build/cmake_install.cmake")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/dedekind" TYPE MODULE FILES "/Users/vincent/git/github.com/vincentk/dedekind/build-python/cp313-cp313-macosx_15_0_x86_64/python/dedekind/_dedekind.cpython-313-darwin.so")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/dedekind/_dedekind.cpython-313-darwin.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/dedekind/_dedekind.cpython-313-darwin.so")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" -x "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/dedekind/_dedekind.cpython-313-darwin.so")
    endif()
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/Users/vincent/git/github.com/vincentk/dedekind/build-python/cp313-cp313-macosx_15_0_x86_64/libdedekind_category.a")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_category.a" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_category.a")
    execute_process(COMMAND "/usr/local/opt/llvm/bin/llvm-ranlib" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_category.a")
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/modules/dedekind/dedekind/category" TYPE FILE FILES
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/category/action.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/category/cartesian.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/category/category.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/category/discrete.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/category/etcs.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/category/functor.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/category/kleisli.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/category/limit.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/category/logic.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/category/mereology.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/category/monad.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/category/morphism.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/category/natural.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/category/numeric.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/category/partial.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/category/posetal.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/category/pullback.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/category/small.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/category/species.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/category/topoi.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/category/total.cppm"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/Users/vincent/git/github.com/vincentk/dedekind/build-python/cp313-cp313-macosx_15_0_x86_64/libdedekind_ieee.a")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_ieee.a" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_ieee.a")
    execute_process(COMMAND "/usr/local/opt/llvm/bin/llvm-ranlib" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_ieee.a")
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/modules/dedekind/dedekind/ieee" TYPE FILE FILES
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/ieee/approx.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/ieee/ieee.cppm"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/Users/vincent/git/github.com/vincentk/dedekind/build-python/cp313-cp313-macosx_15_0_x86_64/libdedekind_sets.a")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_sets.a" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_sets.a")
    execute_process(COMMAND "/usr/local/opt/llvm/bin/llvm-ranlib" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_sets.a")
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/modules/dedekind/dedekind/sets" TYPE FILE FILES
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/sets/boundaries.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/sets/expressions.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/sets/family.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/sets/interop.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/sets/mereology.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/sets/relational.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/sets/sets.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/sets/singleton.cppm"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/Users/vincent/git/github.com/vincentk/dedekind/build-python/cp313-cp313-macosx_15_0_x86_64/libdedekind_order.a")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_order.a" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_order.a")
    execute_process(COMMAND "/usr/local/opt/llvm/bin/llvm-ranlib" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_order.a")
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/modules/dedekind/dedekind/order" TYPE FILE FILES "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/order/order.cppm")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/Users/vincent/git/github.com/vincentk/dedekind/build-python/cp313-cp313-macosx_15_0_x86_64/libdedekind_topology.a")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_topology.a" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_topology.a")
    execute_process(COMMAND "/usr/local/opt/llvm/bin/llvm-ranlib" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_topology.a")
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/modules/dedekind/dedekind/topology" TYPE FILE FILES
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/topology/interval.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/topology/neighborhood.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/topology/topology.cppm"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/Users/vincent/git/github.com/vincentk/dedekind/build-python/cp313-cp313-macosx_15_0_x86_64/libdedekind_sequences.a")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_sequences.a" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_sequences.a")
    execute_process(COMMAND "/usr/local/opt/llvm/bin/llvm-ranlib" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_sequences.a")
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/modules/dedekind/dedekind/sequences" TYPE FILE FILES
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/sequences/curve.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/sequences/limits.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/sequences/net.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/sequences/path.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/sequences/ranges.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/sequences/sequences.cppm"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/Users/vincent/git/github.com/vincentk/dedekind/build-python/cp313-cp313-macosx_15_0_x86_64/libdedekind_algebra.a")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_algebra.a" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_algebra.a")
    execute_process(COMMAND "/usr/local/opt/llvm/bin/llvm-ranlib" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_algebra.a")
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/modules/dedekind/dedekind/algebra" TYPE FILE FILES
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/algebra/algebra.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/algebra/boolean.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/algebra/division.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/algebra/field.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/algebra/group.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/algebra/modules.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/algebra/monoid.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/algebra/polynomial.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/algebra/ring.cppm"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/Users/vincent/git/github.com/vincentk/dedekind/build-python/cp313-cp313-macosx_15_0_x86_64/libdedekind_morphologies.a")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_morphologies.a" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_morphologies.a")
    execute_process(COMMAND "/usr/local/opt/llvm/bin/llvm-ranlib" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_morphologies.a")
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/modules/dedekind/dedekind/morphologies" TYPE FILE FILES
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/morphologies/archimedean.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/morphologies/cyclic.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/morphologies/morphologies.cppm"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/Users/vincent/git/github.com/vincentk/dedekind/build-python/cp313-cp313-macosx_15_0_x86_64/libdedekind_geometry.a")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_geometry.a" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_geometry.a")
    execute_process(COMMAND "/usr/local/opt/llvm/bin/llvm-ranlib" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_geometry.a")
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/modules/dedekind/dedekind/geometry" TYPE FILE FILES
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/geometry/affine.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/geometry/euclidean.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/geometry/geometry.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/geometry/hilbert.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/geometry/inner_product.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/geometry/lattice.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/geometry/linear_map.cppm"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/Users/vincent/git/github.com/vincentk/dedekind/build-python/cp313-cp313-macosx_15_0_x86_64/libdedekind_numbers.a")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_numbers.a" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_numbers.a")
    execute_process(COMMAND "/usr/local/opt/llvm/bin/llvm-ranlib" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_numbers.a")
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/modules/dedekind/dedekind/numbers" TYPE FILE FILES
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/numbers/approx.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/numbers/booleans.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/numbers/complex.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/numbers/constants.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/numbers/dual.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/numbers/ieee.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/numbers/integer.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/numbers/lattice.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/numbers/mandelbrot.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/numbers/naturals.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/numbers/numbers.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/numbers/quaternion.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/numbers/rational.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/numbers/real.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/numbers/scalars.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/numbers/symbolic.cppm"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/Users/vincent/git/github.com/vincentk/dedekind/build-python/cp313-cp313-macosx_15_0_x86_64/libdedekind_analysis.a")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_analysis.a" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_analysis.a")
    execute_process(COMMAND "/usr/local/opt/llvm/bin/llvm-ranlib" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_analysis.a")
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/modules/dedekind/dedekind/analysis" TYPE FILE FILES
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/analysis/analysis.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/analysis/exterior.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/analysis/forms.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/analysis/hamilton.cppm"
    "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/analysis/kernels.cppm"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/Users/vincent/git/github.com/vincentk/dedekind/build-python/cp313-cp313-macosx_15_0_x86_64/libdedekind_python.a")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_python.a" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_python.a")
    execute_process(COMMAND "/usr/local/opt/llvm/bin/llvm-ranlib" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libdedekind_python.a")
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/modules/dedekind/dedekind/python" TYPE FILE FILES "/Users/vincent/git/github.com/vincentk/dedekind/src/main/modules/dedekind/python/python.cppm")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/dedekind/dedekindConfig.cmake")
    file(DIFFERENT _cmake_export_file_changed FILES
         "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/dedekind/dedekindConfig.cmake"
         "/Users/vincent/git/github.com/vincentk/dedekind/build-python/cp313-cp313-macosx_15_0_x86_64/CMakeFiles/Export/c5f9e0e933ccec5e7c33f03ece73c2ce/dedekindConfig.cmake")
    if(_cmake_export_file_changed)
      file(GLOB _cmake_old_config_files "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/dedekind/dedekindConfig-*.cmake")
      if(_cmake_old_config_files)
        string(REPLACE ";" ", " _cmake_old_config_files_text "${_cmake_old_config_files}")
        message(STATUS "Old export file \"$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/dedekind/dedekindConfig.cmake\" will be replaced.  Removing files [${_cmake_old_config_files_text}].")
        unset(_cmake_old_config_files_text)
        file(REMOVE ${_cmake_old_config_files})
      endif()
      unset(_cmake_old_config_files)
    endif()
    unset(_cmake_export_file_changed)
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/cmake/dedekind" TYPE FILE FILES "/Users/vincent/git/github.com/vincentk/dedekind/build-python/cp313-cp313-macosx_15_0_x86_64/CMakeFiles/Export/c5f9e0e933ccec5e7c33f03ece73c2ce/dedekindConfig.cmake")
  if(CMAKE_INSTALL_CONFIG_NAME MATCHES "^([Rr][Ee][Ll][Ee][Aa][Ss][Ee])$")
    file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/cmake/dedekind" TYPE FILE FILES "/Users/vincent/git/github.com/vincentk/dedekind/build-python/cp313-cp313-macosx_15_0_x86_64/CMakeFiles/Export/c5f9e0e933ccec5e7c33f03ece73c2ce/dedekindConfig-release.cmake")
  endif()
endif()

string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
if(CMAKE_INSTALL_LOCAL_ONLY)
  file(WRITE "/Users/vincent/git/github.com/vincentk/dedekind/build-python/cp313-cp313-macosx_15_0_x86_64/install_local_manifest.txt"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")
endif()
if(CMAKE_INSTALL_COMPONENT)
  if(CMAKE_INSTALL_COMPONENT MATCHES "^[a-zA-Z0-9_.+-]+$")
    set(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
  else()
    string(MD5 CMAKE_INST_COMP_HASH "${CMAKE_INSTALL_COMPONENT}")
    set(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INST_COMP_HASH}.txt")
    unset(CMAKE_INST_COMP_HASH)
  endif()
else()
  set(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  file(WRITE "/Users/vincent/git/github.com/vincentk/dedekind/build-python/cp313-cp313-macosx_15_0_x86_64/${CMAKE_INSTALL_MANIFEST}"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")
endif()
