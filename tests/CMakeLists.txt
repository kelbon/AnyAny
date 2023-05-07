cmake_minimum_required(VERSION 3.21)

if("${CMAKE_CXX_STANDARD}" STREQUAL "")
	set(ANYANY_TESTS_CXX_STANDARD 17)
else()
	set(ANYANY_TESTS_CXX_STANDARD "${CMAKE_CXX_STANDARD}")
endif()

set(AA_PARTS anyany data_parallel_vector variant_swarm visit_invoke utilities)
foreach(PART ${AA_PARTS})
	add_executable(${PART} ${CMAKE_CURRENT_SOURCE_DIR}/test_${PART}.cpp)
	target_link_libraries(${PART} PUBLIC anyanylib)
	target_include_directories(${PART} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../examples)
	add_test(NAME test_${PART} COMMAND ${PART})
	set_target_properties(${PART} PROPERTIES
		CMAKE_CXX_EXTENSIONS OFF
		LINKER_LANGUAGE CXX
		CXX_STANDARD ${ANYANY_TESTS_CXX_STANDARD}
		CMAKE_CXX_STANDARD_REQUIRED ON
	)
	if(MSVC)
		target_compile_options(${PART} PUBLIC "/Zc:__cplusplus" "/Zc:preprocessor" "/permissive-" "/wd5051" "/wd4848")
	endif()
endforeach()

file(GLOB AA_TESTS REGULAR_EXPRESSION ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)
source_group(Tests FILES ${AA_TESTS})