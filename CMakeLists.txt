cmake_minimum_required(VERSION 3.21)
project(anyany LANGUAGES CXX)

option(AA_ENABLE_TESTING "enables testing" OFF)

file(GLOB ${PROJECT_NAME}_HEADERS "${CMAKE_CURRENT_SOURCE_DIR}/include/anyany/*.hpp")
file(GLOB ${PROJECT_NAME}_NOEXPORT_HEADERS "${CMAKE_CURRENT_SOURCE_DIR}/include/anyany/noexport/*.hpp")

### anyanylib ###

add_library(anyanylib INTERFACE "${${PROJECT_NAME}_HEADERS}" "${${PROJECT_NAME}_NOEXPORT_HEADERS}")

target_include_directories(anyanylib INTERFACE "${CMAKE_CURRENT_SOURCE_DIR}/include")

set_target_properties(${PROJECT_NAME}lib PROPERTIES
	CMAKE_CXX_EXTENSIONS OFF
	LINKER_LANGUAGE CXX
	CMAKE_CXX_STANDARD_REQUIRED ON
	CXX_STANDARD 17)

### visual ###

source_group(Details FILES ${${PROJECT_NAME}_NOEXPORT_HEADERS})
source_group(Include FILES ${${PROJECT_NAME}_HEADERS})

### tests ###

if(AA_ENABLE_TESTING)
	include(CTest)
	add_subdirectory(tests)
endif()
if(MSVC)
  # asks stupid MSVC to work as C++ standard says, asks separatelly MSVC-preprocessor to work as C++ standard says
  # additionally asks MSVC to SHOW UP CORRECTLY __cplusplus VERSION OMG
  # and of course disables two warnings about "oh, im stupid msvc, support of this attribute in C++17 is an extension..."
  # THEN FCN IGNORE IT, IT IS FOR WHAT ATTRIBUTES EXIST
  target_compile_options(${PROJECT_NAME}lib INTERFACE "/Zc:__cplusplus" "/Zc:preprocessor" "/permissive-" "/wd5051" "/wd4848")
endif()