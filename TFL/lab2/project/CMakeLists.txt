cmake_minimum_required(VERSION 3.26)
project(simplifier_lib)
set(CMAKE_CXX_STANDARD 20)

file(GLOB SOURCES ./src/*.cpp ./src/parser/*.cpp)
file(GLOB INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/include)

include_directories(${INCLUDE_DIRS})

add_library(${PROJECT_NAME} ${SOURCES})

set(SIMPLIFIER_LIBRARIES ${PROJECT_NAME} PARENT_SCOPE)
set(SIMPLIFIER_INCLUDE_DIRS ${INCLUDE_DIRS} PARENT_SCOPE)
