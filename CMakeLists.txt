cmake_minimum_required(VERSION 3.21)
project(anyany)

#add_library(anyany STATIC ${CMAKE_CURRENT_SOURCE_DIR}/anyany.ixx ${CMAKE_CURRENT_SOURCE_DIR}/main.cpp)
add_executable(anyany ${CMAKE_CURRENT_SOURCE_DIR}/anyany.ixx ${CMAKE_CURRENT_SOURCE_DIR}/main.cpp)

set_property(TARGET anyany PROPERTY CMAKE_CXX_STANDARD_REQUIRED ON)
set_property(TARGET anyany PROPERTY CXX_STANDARD 20)