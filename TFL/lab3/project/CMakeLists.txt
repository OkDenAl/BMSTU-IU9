cmake_minimum_required(VERSION 3.14)
project(lab3_lib)
set(CMAKE_CXX_STANDARD 20)

file(GLOB SOURCES ./src/*.cpp ./src/parser/*.cpp)
file(GLOB INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/include)

include_directories(${INCLUDE_DIRS})

add_library(${PROJECT_NAME} ${SOURCES})

set(LAB3_LIBRARIES ${PROJECT_NAME} PARENT_SCOPE)
set(LAB3_INCLUDE_DIRS ${INCLUDE_DIRS} PARENT_SCOPE)
