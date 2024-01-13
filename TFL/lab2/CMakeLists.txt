cmake_minimum_required(VERSION 3.26)
project(simplifier)

set(CMAKE_CXX_STANDARD 20)

add_link_options(-coverage -lgcov)

add_subdirectory(project)

add_executable(${PROJECT_NAME} main.cpp)
add_executable(console console.cpp)

target_include_directories(${PROJECT_NAME} PUBLIC ${SIMPLIFIER_INCLUDE_DIRS})
target_link_libraries(${PROJECT_NAME} PRIVATE ${SIMPLIFIER_LIBRARIES})

target_include_directories(console PUBLIC ${SIMPLIFIER_INCLUDE_DIRS})
target_link_libraries(console PRIVATE ${SIMPLIFIER_LIBRARIES})

add_subdirectory(pcre2)
add_subdirectory(test)
