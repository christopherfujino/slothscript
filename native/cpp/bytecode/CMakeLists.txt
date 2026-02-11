## Global

cmake_minimum_required(VERSION 3.31)
project(slothscript-cpp-bytecode VERSION 0.1)

# These CMAKE_* variables apply to all future targets
set(CMAKE_CXX_EXTENSIONS OFF) # why don't want GNU extension
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Generate a compile_commands.json file
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# IWYU
find_program(IWYU_PATH NAMES include-what-you-use iwyu)
if(NOT IWYU_PATH)
  message(FATAL_ERROR
    "Could not find the program include-what-you-use (apt install iwyu)")
endif()
set(CMAKE_CXX_INCLUDE_WHAT_YOU_USE ${IWYU_PATH})

add_compile_options(-Wall -Werror -Wpedantic -Wextra)

# https://stackoverflow.com/questions/24648357/compiling-a-static-executable-with-cmake
set(CMAKE_FIND_LIBRARY_SUFFIXES ".a")
set(BUILD_SHARED_LIBS OFF)
set(CMAKE_EXE_LINKER_FLAGS "-static")

## Test exe

add_executable(test test.cpp)

target_link_libraries(test bytecode)
target_include_directories(test PRIVATE ../../../ignore/test.hpp)

## Bytecode library

add_library(bytecode STATIC bytecode.cpp)
