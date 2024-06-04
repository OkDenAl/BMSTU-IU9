cmake_minimum_required(VERSION 3.4.3)
project(opt_lab2)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_executable(opt_lab2 main.cpp)

llvm_map_components_to_libnames(llvm_libs support core irreader)

target_link_libraries(opt_lab2 ${llvm_libs})