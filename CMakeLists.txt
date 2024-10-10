cmake_minimum_required(VERSION 3.21)
project(anyany LANGUAGES CXX)

option(AA_ENABLE_TESTING "enables testing" OFF)

### anyanylib ###

add_library(anyanylib INTERFACE)

target_include_directories(anyanylib INTERFACE "${CMAKE_CURRENT_SOURCE_DIR}/include")

set_target_properties(${PROJECT_NAME}lib PROPERTIES
	CMAKE_CXX_EXTENSIONS OFF
	LINKER_LANGUAGE CXX
	CMAKE_CXX_STANDARD_REQUIRED ON
	CXX_STANDARD 17)

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
