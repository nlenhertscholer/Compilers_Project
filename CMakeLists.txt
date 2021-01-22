cmake_minimum_required(VERSION 3.5.1)
project(compiler_project VERSION 1.0)
set(CMAKE_CXX_FLAGS "-std=c++17 -g3" )
set(CMAKE_GENERATOR "Unix Makefiles")

# Set binary folder for placing executable in
# NOTE: This assumes that the command "cmake .." is being executed in a subfolder of the root
#       project directory (EX: <path_to_Compilers_Project>/build)        
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/bin)

# Find Flex and Bison
find_package(BISON)
find_package(FLEX)
find_package(LLVM REQUIRED CONFIG)
if(BISON_FOUND)
    message(STATUS "Found Bison")
endif(BISON_FOUND)
if(FLEX_FOUND)
    message(STATUS "Found Flex")
endif(FLEX_FOUND)
if(LLVM_FOUND)
    message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
    message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
endif(LLVM_FOUND)

BISON_TARGET(ek_parser src/ek_parser.y ${CMAKE_CURRENT_BINARY_DIR}/ek_parser.cpp COMPILE_FLAGS "--debug")
FLEX_TARGET(ek_tokens src/ek_tokens.l ${CMAKE_CURRENT_BINARY_DIR}/ek_tokens.cpp COMPILE_FLAGS "--debug")
ADD_FLEX_BISON_DEPENDENCY(ek_tokens ek_parser)

# Add source files
file(GLOB c_files "src/*.cpp")
file(GLOB h_files "src/lib/*.hpp")

set(MY_HEADER_FILES ${h_files})
set(MY_SOURCE_FILES ${c_files})

# Include all header files
include_directories(
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR} 
    ${LLVM_INCLUDE_DIRS}
    src/lib
)

# Add compile time flags for llvm
add_definitions(${LLVM_DEFINITIONS})

add_executable(ekcc 
    ${MY_HEADER_FILES}
    ${MY_SOURCE_FILES} 
    ${BISON_ek_parser_OUTPUTS}
    ${FLEX_ek_tokens_OUTPUTS}    
)

# Map the components to a name so they can be linked
llvm_map_components_to_libnames(llvm_libs all)

# Link against LLVM libraries
target_link_libraries(ekcc ${llvm_libs})

