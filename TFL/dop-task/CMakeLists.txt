cmake_minimum_required(VERSION 3.16)
project(task)

set(CMAKE_CXX_STANDARD 20)

add_link_options(-coverage -lgcov)

#enable_testing()
add_subdirectory(project)

add_executable(${PROJECT_NAME} main.cpp)