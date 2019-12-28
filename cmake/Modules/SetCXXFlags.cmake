####################################################################
INCLUDE(${CMAKE_MODULE_PATH}/SetCompileFlag.cmake)

# Make sure the build type is uppercase
STRING(TOUPPER "${CMAKE_BUILD_TYPE}" BT)

IF(BT STREQUAL "RELEASE")
    SET(CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are DEBUG, RELEASE, or TESTING."
      FORCE)
ELSEIF(BT STREQUAL "DEBUG")
    SET (CMAKE_BUILD_TYPE DEBUG CACHE STRING
      "Choose the type of build, options are DEBUG, RELEASE, or TESTING."
      FORCE)
ELSEIF(BT STREQUAL "TESTING")
    SET (CMAKE_BUILD_TYPE TESTING CACHE STRING
      "Choose the type of build, options are DEBUG, RELEASE, or TESTING."
      FORCE)
ELSEIF(NOT BT)
    SET(CMAKE_BUILD_TYPE DEBUG CACHE STRING
      "Choose the type of build, options are DEBUG, RELEASE, or TESTING."
      FORCE)
    MESSAGE(STATUS "CMAKE_BUILD_TYPE not given, defaulting to DEBUG")
ELSE()
    MESSAGE(FATAL_ERROR "CMAKE_BUILD_TYPE not valid, choices are DEBUG, RELEASE, or TESTING")
ENDIF(BT STREQUAL "RELEASE")

######################################################
# Determine and set the CXX compiler flags we want 
######################################################
SET_COMPILE_FLAG(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}"
                 CXX "/std:c++latest"  # MSVC
                     "-std=c++0x"      # GNU
                     "-std=gnu++0x"    # GNU
                     " -c"             # GNU
                     "-fPIC"           # GNU  
                )

SET_COMPILE_FLAG(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}"
                 CXX  "/MTd"          # MSVC
                      "-g"            # GNU

		)

SET_COMPILE_FLAG(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE}"
                 CXX  "/MT"          # MSVC

	        )
