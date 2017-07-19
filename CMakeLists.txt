##########################################################################################
# CMake project file for DSSAT-CSM
##########################################################################################
# Define the project and the dependencies that it has
##########################################################################################

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.5)

if("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_BINARY_DIR}")
   message(FATAL_ERROR "DSSAT in-source builds are not permitted. \nMake a separate folder" 
   						"for building:\nmkdir build; cd build; cmake ..\nBefore that," 
   						"remove the files already created:\n"
   						"CMakeCache.txt and CMakeFiles\n")
endif("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_BINARY_DIR}")

PROJECT(DSSAT-CSM Fortran )

##########################################################################################
# DSSAT_VERSION from ModuleDefs.for
##########################################################################################

set(FILE_NAME "${CMAKE_SOURCE_DIR}/ModuleDefs.for")
file(READ ${FILE_NAME} File_Content)

# Take DSSAT_VERSION from ModulesDefs.for
STRING(REGEX REPLACE ".*Major = ([0-9]+).*" "\\1" MAJOR "${File_Content}")
STRING(REGEX REPLACE ".*Minor = ([0-9]+).*" "\\1" MINOR "${File_Content}")
STRING(REGEX REPLACE ".*Model = ([0-9]+).*" "\\1" MODEL "${File_Content}")
STRING(REGEX REPLACE ".*Build = ([0-9]+).*" "\\1" BUILD "${File_Content}")
MESSAGE(STATUS "MAJOR: ${MAJOR} MINOR: ${MINOR} MODEL: ${MODEL} BUILD: ${BUILD}")

# Set the version
SET(VERSION ${MAJOR}.${MINOR}.${MODEL})

##########################################################################################

# Add our local modlues to the module path
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Organize output files.  On Windows this also keeps .dll files next
# to the .exe files that need them, making tests easy to run.
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

# Keep Uncomment if Fortran 90 support is required
IF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)
    MESSAGE(FATAL_ERROR "Fortran compiler does not support F90")
ENDIF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)

# This INCLUDE statement executes code that sets the compile flags for DEBUG,
# RELEASE, and TESTING.  You should  review this file and make sure the flags 
# are to your liking.
INCLUDE(${CMAKE_MODULE_PATH}/SetFortranFlags.cmake) 

# There is an error in CMAKE with this flag for pgf90.  Unset it
GET_FILENAME_COMPONENT(FCNAME ${CMAKE_Fortran_COMPILER} NAME)
IF(FCNAME STREQUAL "pgf90")
    UNSET(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS)
ENDIF(FCNAME STREQUAL "pgf90")

##########################################################################################
# Define the actual files and folders that make the build
##########################################################################################

# Define the executable name
IF(WIN32)
    SET(EXECUTABLE_NAME dscsm0${MAJOR}${MINOR})
    add_definitions(-DWIN32=1})
ELSE()
    SET(EXECUTABLE_NAME dscsm0${MAJOR}${MINOR}.exe)
ENDIF(WIN32)

# Add the source files
file(GLOB CSM_FILES
    "${CMAKE_SOURCE_DIR}/CSVOUTS/*.f??"
    "${CMAKE_SOURCE_DIR}/CSVOUTS/*.F??"
    "${CMAKE_SOURCE_DIR}/*.f??"
    "${CMAKE_SOURCE_DIR}/*.F??"
    "${CMAKE_SOURCE_DIR}/ttutil/*.f??"
    "${CMAKE_SOURCE_DIR}/ttutil/*.F??"
    "${CMAKE_SOURCE_DIR}/SALUS/*.f??"
    "${CMAKE_SOURCE_DIR}/SALUS/*.F??"
    "${CMAKE_SOURCE_DIR}/OP_OBS/*.f??"
    "${CMAKE_SOURCE_DIR}/OP_OBS/*.F??"
    "${CMAKE_SOURCE_DIR}/ORYZA/*.f??"
    "${CMAKE_SOURCE_DIR}/ORYZA/*.F??"
	"${CMAKE_SOURCE_DIR}/YCA/*.f??"
    "${CMAKE_SOURCE_DIR}/YCA/*.F??"
)
ADD_EXECUTABLE(${EXECUTABLE_NAME} ${CSM_FILES})

#####################################
# Tell how to install this executable
#####################################

IF(WIN32)
    SET(CMAKE_INSTALL_PREFIX "C:\\DSSAT46")
ELSE()
    SET(CMAKE_INSTALL_PREFIX ~/DSSAT46)
ENDIF(WIN32)
INSTALL(TARGETS ${EXECUTABLE_NAME} RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX})

# Add a distclean target to the Makefile
ADD_CUSTOM_TARGET(distclean 
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_SOURCE_DIR}/distclean.cmake
)

message( "-- Flags" )
message( "   FFLAGS       ${CMAKE_Fortran_FLAGS}" )
message( "   RELEASE      ${CMAKE_Fortran_FLAGS_RELEASE}" )
message( "   DEBUG        ${CMAKE_Fortran_FLAGS_DEBUG}" )
message( "-- Build Info" )
message( "   BUILD TYPE   ${CMAKE_BUILD_TYPE}" )
message( "   VERSION      ${MAJOR}.${MINOR}.${MODEL}.${BUILD}" )
message( "   I. PREFIX    ${CMAKE_INSTALL_PREFIX}" )
message( "   Executable   ${EXECUTABLE_NAME}" )
