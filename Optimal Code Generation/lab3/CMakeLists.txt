cmake_minimum_required(VERSION 3.15)
project(lab3_1)

set(CMAKE_CXX_STANDARD 14)

find_package(LLVM 11.1.0 REQUIRED CONFIG)

if (NOT LLVM_ENABLE_RTTI)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti")
endif()

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_executable(lab3_1 main.cpp)
llvm_map_components_to_libnames(
        llvm_libs
        Analysis
        Core
        ExecutionEngine
        InstCombine
        Object
        OrcJIT
        RuntimeDyld
        ScalarOpts
        Support
        TransformUtils
        native
        irreader
)


target_link_libraries(lab3_1 ${llvm_libs})