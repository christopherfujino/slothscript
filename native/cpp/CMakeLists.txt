# WSL Ubuntu is on 25.10 as of Jan 2026
cmake_minimum_required(VERSION 3.31)

project(slothscript-cpp VERSION 0.1)

# Generate a compile_commands.json file
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_compile_options(-Wall -Werror -Wpedantic -Wextra)

add_library(sloth_native STATIC
  native.cpp
  _native.cpp)

# This runs at config time
execute_process(COMMAND ocamlc -where
  OUTPUT_STRIP_TRAILING_WHITESPACE
  OUTPUT_VARIABLE OCAML_INCLUDE)

target_include_directories(sloth_native PRIVATE
  ${OCAML_INCLUDE})
