cmake_minimum_required(VERSION 3.21)
project(anyany LANGUAGES CXX)

option(AA_ENABLE_TESTING "enables testing" OFF)

file(GLOB ${PROJECT_NAME}_HEADERS ${CMAKE_CURRENT_SOURCE_DIR}/include/*.hpp)
file(GLOB ${PROJECT_NAME}_NOEXPORT_HEADERS ${CMAKE_CURRENT_SOURCE_DIR}/include/noexport/*.hpp)
file(GLOB ${PROJECT_NAME}_EXAMPLES REGULAR_EXPRESSION ${CMAKE_CURRENT_SOURCE_DIR}/examples/*)

### anyanylib ###

add_library(${PROJECT_NAME}lib INTERFACE ${${PROJECT_NAME}_HEADERS}
	${${PROJECT_NAME}_NOEXPORT_HEADERS}
	${${PROJECT_NAME}_EXAMPLES})

target_include_directories(${PROJECT_NAME}lib INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)

set_target_properties(${PROJECT_NAME}lib PROPERTIES
	CMAKE_CXX_EXTENSIONS OFF
	LINKER_LANGUAGE CXX
	CMAKE_CXX_STANDARD_REQUIRED ON
	CXX_STANDARD 20)

### visual ###

source_group(Details FILES ${${PROJECT_NAME}_NOEXPORT_HEADERS})
source_group(Include FILES ${${PROJECT_NAME}_HEADERS})
source_group(Examples FILES ${${PROJECT_NAME}_EXAMPLES})

### tests ###

if(AA_ENABLE_TESTING)
	include(CTest)
	add_subdirectory(tests)
endif()