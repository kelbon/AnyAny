cmake_minimum_required(VERSION 3.21)

set(AA_PARTS anyany data_parallel_vector variant_swarm)
foreach(PART ${AA_PARTS})
	add_executable(${PART} ${CMAKE_CURRENT_SOURCE_DIR}/test_${PART}.cpp)
	target_include_directories(${PART}
		PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../include
				${CMAKE_CURRENT_SOURCE_DIR}/../examples)
	add_test(NAME test_${PART} COMMAND ${PART})
	set_target_properties(${PART} PROPERTIES
		CMAKE_CXX_EXTENSIONS OFF
		LINKER_LANGUAGE CXX
		CMAKE_CXX_STANDARD_REQUIRED ON
		CXX_STANDARD 20
	)	
endforeach()

file(GLOB AA_TESTS REGULAR_EXPRESSION ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)
source_group(Tests FILES ${AA_TESTS})