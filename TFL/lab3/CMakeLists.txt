cmake_minimum_required(VERSION 3.14)
project(lab3)

set(CMAKE_CXX_STANDARD 20)

add_link_options(-coverage -lgcov)

add_subdirectory(project)

add_executable(${PROJECT_NAME} main.cpp)


target_include_directories(${PROJECT_NAME} PUBLIC ${LAB3_INCLUDE_DIRS})
target_link_libraries(${PROJECT_NAME} PRIVATE ${LAB3_LIBRARIES})

