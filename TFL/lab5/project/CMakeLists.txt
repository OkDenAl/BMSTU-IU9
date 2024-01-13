cmake_minimum_required(VERSION 3.14)
project(lab5_lib)
set(CMAKE_CXX_STANDARD 20)

file(GLOB SOURCES ./src/*.cpp)
file(GLOB INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/include)

include_directories(${INCLUDE_DIRS})

add_library(${PROJECT_NAME} ${SOURCES})

set(LAB5_LIBRARIES ${PROJECT_NAME} PARENT_SCOPE)
set(LAB5_INCLUDE_DIRS ${INCLUDE_DIRS} PARENT_SCOPE)