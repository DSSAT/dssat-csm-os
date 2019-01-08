######################################################
# Determine and set the Fortran compiler flags we want 
######################################################

####################################################################
# Make sure that the default build type is RELEASE if not specified.
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

#########################################################
# If the compiler flags have already been set, return now
#########################################################

IF(CMAKE_Fortran_FLAGS_RELEASE AND CMAKE_Fortran_FLAGS_TESTING AND CMAKE_Fortran_FLAGS_DEBUG)
    RETURN ()
ENDIF(CMAKE_Fortran_FLAGS_RELEASE AND CMAKE_Fortran_FLAGS_TESTING AND CMAKE_Fortran_FLAGS_DEBUG)

########################################################################
# Determine the appropriate flags for this compiler for each build type.
# For each option type, a list of possible flags is given that work
# for various compilers.  The first flag that works is chosen.
# If none of the flags work, nothing is added (unless the REQUIRED 
# flag is given in the call).  This way unknown compiles are supported.
#######################################################################

#####################
### GENERAL FLAGS ###
#####################

# Don't add underscores in symbols for C-compatability
#SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}"
#                 Fortran "-fno-underscoring")

# There is some bug where -march=native doesn't work on Mac
IF(APPLE)
    SET(GNUNATIVE "-mtune=native")
ELSE()
    SET(GNUNATIVE "-march=native")
ENDIF()
# Optimize for the host's architecture
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}"
#                 Fortran "/QxHost"       # AMD Windows
#                         "-xHost"        # AMD
                         ${GNUNATIVE}    # GNU
                         "-ta=host"      # Portland Group
                )

# Enable special treatment for lines beginning with "d" or "D" in fixed form sources
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}"
                 Fortran "-fd-lines-as-comments"
                )

# Enable generation of run-time checks for array subscripts and against the declared
# minimum and maximum values.
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}"
                 Fortran "-fbounds-check"
                )

# Entire line is meaningful and that continued character constants never have implicit
# spaces appended to them to fill out the line (Intel uses only 72, 80, or 132)
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}"
                 Fortran "-132"                     # Intel Fortran
                         "-ffixed-line-length-none" # GNU
                )

# The entire line is meaningful
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}"
                 Fortran "-ffree-line-length-none"  # GNU
                )

# Initialize local character variables to string of 32 bytes
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}"
                 Fortran "-finit-character=32"
                )

# Causes variables to be placed in static memory
# Ref: https://software.intel.com/en-us/node/691772
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}"
                 Fortran "/Qsave" 			# Intel Windows
                 		 "-save"  			# Intel
                )

# Enable preprocessing
# Ref: http://fortranwiki.org/fortran/show/Predefined+preprocessor+macros
# Ref: https://software.intel.com/en-us/node/694581
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}"
                 Fortran "/fpp" # Intel Windows
				         "-fpp" # Intel
                 	     "-cpp"
                )
 
# 
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}"
                 Fortran "/MACHINE:IX86" # Intel 
                )

# Links to a single-threaded, static run-time library 
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}"
                 Fortran "/libs:static" # Intel 
                )
# Tells the linker to search for unresolved references in a multithreaded run-time library
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}"
                 Fortran "/threads" # Intel Windows
				         "-threads" # Intel Linux/Mac
                )
# Restricts floating-point exceptions by enabling the overflow, the divide-by-zero, and the invalid floating-point exceptions				
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}"
                 Fortran "/fpe:0" # Intel Windows
                         "-fpe0"  # Intel Linux/Mac		 
                )

###################
### DEBUG FLAGS ###
###################

# NOTE: debugging symbols (-g or /debug:full) are already on by default

# Disable optimizations
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG}"
                 Fortran REQUIRED "/Od" # Intel Windows
                                  "-O0" # All compilers not on Windows
                )

# Turn on all warnings 
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG}"
                 Fortran "/warn:all" # Intel Windows
                         "-warn all" # Intel
                         "-Wall"     # GNU
                                     # Portland Group (on by default)
                )

# Traceback
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG}"
                 Fortran "/traceback"   # Intel Windows
                         "-traceback"   # Intel/Portland Group
                         "-fbacktrace"  # GNU (gfortran)
                         "-ftrace=full" # GNU (g95)
                )

# Check array bounds
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG}"
                 Fortran "/check:bounds"  # Intel Windows
                         "-check bounds"  # Intel
                         "-fcheck=bounds" # GNU (New style)
                         "-fbounds-check" # GNU (Old style)
                         "-Mbounds"       # Portland Group
                )

#####################
### TESTING FLAGS ###
#####################

# Optimizations
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_TESTING "${CMAKE_Fortran_FLAGS_TESTING}"
                 Fortran REQUIRED "/O2" # Intel Windows
                                  "-O2" # All compilers not on Windows
                )

#####################
### RELEASE FLAGS ###
#####################

# NOTE: agressive optimizations (-O3) are already turned on by default

# Unroll loops
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE}"
                 Fortran "/unroll"        # Intel Windows
                         "-funroll-loops" # GNU
                         "-unroll"        # Intel
			             "-Munroll"       # Portland Group
                )

# Inline functions
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE}"
                 Fortran "/Qinline"           # Intel Windows
                         "-inline"            # Intel
                         "-finline-functions" # GNU
                         "-Minline"           # Portland Group
                )

# Interprocedural (link-time) optimizations
#SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE}"
#                 Fortran "/Qipo"    # Intel Windows
#                         "-ipo"     # Intel
#                         "-flto"    # GNU
#                         "-Mipa"    # Portland Group
#                )

# Single-file optimizations
#SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE}"
#                 Fortran "/Qip" # Intel Windows
#                         "-ip"  # Intel
#                )

## Vectorize code
#SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE}"
#                 Fortran "/Qvec-report0" # Intel Windows
#                         "-vec-report0"  # Intel
#                         "-Mvect"        # Portland Group
#                )
