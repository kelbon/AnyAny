cmake_minimum_required(VERSION 3.21)

### create tests ###

add_executable(anyany ${CMAKE_CURRENT_SOURCE_DIR}/test_anyany.cpp)
set(AA_TEST_INCLUDE ${CMAKE_CURRENT_SOURCE_DIR}/../include)

add_executable(data_parallel ${CMAKE_CURRENT_SOURCE_DIR}/test_data_parallel.cpp)

add_executable(variant_swarm ${CMAKE_CURRENT_SOURCE_DIR}/test_variant_swarm.cpp)

set_target_properties(anyany data_parallel variant_swarm PROPERTIES
	CMAKE_CXX_EXTENSIONS OFF
	LINKER_LANGUAGE CXX
	CMAKE_CXX_STANDARD_REQUIRED ON
	CXX_STANDARD 20
)

target_include_directories(anyany PRIVATE
    ${AA_TEST_INCLUDE}
	${CMAKE_CURRENT_SOURCE_DIR}/../examples)

target_include_directories(data_parallel PRIVATE ${AA_TEST_INCLUDE})
target_include_directories(variant_swarm PRIVATE ${AA_TEST_INCLUDE})

### visual ###

file(GLOB AA_TESTS REGULAR_EXPRESSION ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)
source_group(Tests FILES ${AA_TESTS})

### add tests ###

add_test(NAME test_anyany COMMAND anyany)
add_test(NAME test_data_parallel COMMAND data_parallel)
add_test(NAME test_variant_swarm COMMAND variant_swarm)