# Install script for directory: /home/runner/work/dedekind/dedekind

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/usr/local")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "Debug")
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

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "1")
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

# Set path to fallback-tool for dependency-resolution.
if(NOT DEFINED CMAKE_OBJDUMP)
  set(CMAKE_OBJDUMP "/usr/bin/objdump")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/runner/work/dedekind/dedekind/build3/_deps/catch2-build/cmake_install.cmake")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/home/runner/work/dedekind/dedekind/build3/libdedekind_category.a")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/modules/dedekind/dedekind/category" TYPE FILE FILES
    "/home/runner/work/dedekind/dedekind/src/main/modules/dedekind/category/action.cppm"
    "/home/runner/work/dedekind/dedekind/src/main/modules/dedekind/category/cartesian.cppm"
    "/home/runner/work/dedekind/dedekind/src/main/modules/dedekind/category/category.cppm"
    "/home/runner/work/dedekind/dedekind/src/main/modules/dedekind/category/discrete.cppm"
    "/home/runner/work/dedekind/dedekind/src/main/modules/dedekind/category/etcs.cppm"
    "/home/runner/work/dedekind/dedekind/src/main/modules/dedekind/category/functor.cppm"
    "/home/runner/work/dedekind/dedekind/src/main/modules/dedekind/category/kleisli.cppm"
    "/home/runner/work/dedekind/dedekind/src/main/modules/dedekind/category/limit.cppm"
    "/home/runner/work/dedekind/dedekind/src/main/modules/dedekind/category/logic.cppm"
    "/home/runner/work/dedekind/dedekind/src/main/modules/dedekind/category/mereology.cppm"
    "/home/runner/work/dedekind/dedekind/src/main/modules/dedekind/category/monad.cppm"
    "/home/runner/work/dedekind/dedekind/src/main/modules/dedekind/category/morphism.cppm"
    "/home/runner/work/dedekind/dedekind/src/main/modules/dedekind/category/natural.cppm"
    "/home/runner/work/dedekind/dedekind/src/main/modules/dedekind/category/numeric.cppm"
    "/home/runner/work/dedekind/dedekind/src/main/modules/dedekind/category/partial.cppm"
    "/home/runner/work/dedekind/dedekind/src/main/modules/dedekind/category/posetal.cppm"
    "/home/runner/work/dedekind/dedekind/src/main/modules/dedekind/category/pullback.cppm"
    "/home/runner/work/dedekind/dedekind/src/main/modules/dedekind/category/small.cppm"
    "/home/runner/work/dedekind/dedekind/src/main/modules/dedekind/category/species.cppm"
    "/home/runner/work/dedekind/dedekind/src/main/modules/dedekind/category/topoi.cppm"
    "/home/runner/work/dedekind/dedekind/src/main/modules/dedekind/category/total.cppm"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/dedekind/dedekindConfig.cmake")
    file(DIFFERENT _cmake_export_file_changed FILES
         "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/dedekind/dedekindConfig.cmake"
         "/home/runner/work/dedekind/dedekind/build3/CMakeFiles/Export/c5f9e0e933ccec5e7c33f03ece73c2ce/dedekindConfig.cmake")
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
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/cmake/dedekind" TYPE FILE FILES "/home/runner/work/dedekind/dedekind/build3/CMakeFiles/Export/c5f9e0e933ccec5e7c33f03ece73c2ce/dedekindConfig.cmake")
  if(CMAKE_INSTALL_CONFIG_NAME MATCHES "^([Dd][Ee][Bb][Uu][Gg])$")
    file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/cmake/dedekind" TYPE FILE FILES "/home/runner/work/dedekind/dedekind/build3/CMakeFiles/Export/c5f9e0e933ccec5e7c33f03ece73c2ce/dedekindConfig-debug.cmake")
  endif()
endif()

string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
if(CMAKE_INSTALL_LOCAL_ONLY)
  file(WRITE "/home/runner/work/dedekind/dedekind/build3/install_local_manifest.txt"
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
  file(WRITE "/home/runner/work/dedekind/dedekind/build3/${CMAKE_INSTALL_MANIFEST}"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")
endif()
