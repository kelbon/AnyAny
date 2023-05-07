cmake_minimum_required(VERSION 3.21)
project(anyany_examples LANGUAGES CXX)

set(ANYANY_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/..")
# explicitly specifies build directory(build), because 'examples' are in the 'anyany' folder itself
add_subdirectory("${ANYANY_DIRECTORY}" build)

file(GLOB ANYANY_EXAMPLES_HEADERS "${CMAKE_CURRENT_SOURCE_DIR}/*.hpp")
add_executable(anyany_examples "${ANYANY_EXAMPLES_HEADERS}" "${CMAKE_CURRENT_SOURCE_DIR}/examples_main.cpp")

if("${CMAKE_CXX_STANDARD}" STREQUAL "")
	set(ANYANY_EXAMPLES_CXX_STANDARD 17)
else()
	set(ANYANY_EXAMPLES_CXX_STANDARD "${CMAKE_CXX_STANDARD}")
endif()
set_target_properties(anyany_examples PROPERTIES
	CMAKE_CXX_EXTENSIONS OFF
	LINKER_LANGUAGE CXX
	CXX_STANDARD ${ANYANY_EXAMPLES_CXX_STANDARD}
	CMAKE_CXX_STANDARD_REQUIRED ON
)

target_link_libraries(anyany_examples PUBLIC anyanylib)
# just 'test' for executing from command line
include(CTest)
add_test(NAME run_anyany_examples COMMAND anyany_examples)