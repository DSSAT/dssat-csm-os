######################################################
# Determine and set the Fortran compiler flags we want 
######################################################

####################################################################
# Make sure that the default build type is RELEASE if not specified.
####################################################################
INCLUDE(${CMAKE_MODULE_PATH}/SetCompileFlag.cmake)

# option to use dynamic link
OPTION(DYNAMIC_LINK "Use dynamic link" OFF)

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

# Supress warnings about external subroutines not declared
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}"
                 Fortran "-noexternal"
                )

# Enable preprocessing
# Ref: http://fortranwiki.org/fortran/show/Predefined+preprocessor+macros
# Ref: https://software.intel.com/en-us/node/694581
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}"
                 Fortran "/fpp" # Intel Windows
                         "-fpp" # Intel
                         "-cpp" # GNU
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
                 Fortran "/fpe:0"                           # Intel Windows
                         "-fpe0"                            # Intel Linux/Mac
                         "-ffpe-trap=invalid,zero,overflow" # GNU
                )

# Hack to make MacOS happy.
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}"
                 Fortran "-mmacosx-version-min=10.10.0"
                )

####################
### LINKER FLAGS ###
####################
IF(DYNAMIC_LINK STREQUAL "OFF")
    IF (APPLE)
        set(MAC_STATIC_LIBGFORTRAN_DIR "" CACHE PATH "Path to static gFortran libraries")
        set(MAC_STATIC_LIBGCC_DIR "" CACHE PATH "Path to libgcc library")
        IF (MAC_STATIC_LIBGFORTRAN_DIR AND MAC_STATIC_LIBGCC_DIR)
            message("Attempting partial static build for MacOS")
            set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -macosx_version_min 10.10 -lSystem ${MAC_STATIC_LIBGFORTRAN_DIR}/libgfortran.a ${MAC_STATIC_LIBGFORTRAN_DIR}/libquadmath.a ${MAC_STATIC_LIBGCC_DIR}/libgcc.a"
                            )
            set(CMAKE_Fortran_LINK_EXECUTABLE "ld ${CMAKE_EXE_LINKER_FLAGS} <OBJECTS> -o <TARGET> <LINK_LIBRARIES>")
        ENDIF()
    ELSE ()
            SET_COMPILE_FLAG(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS}"
                            Fortran "/FORCE"               # MSVC
                                    "-static"              # GNU
                            )
            SET_COMPILE_FLAG(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS}"
                            Fortran "-static-libgcc"       # GNU
                            )
            SET_COMPILE_FLAG(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS}"
                            Fortran "-static-libgfortran"  # GNU
                            )
    ENDIF(APPLE)
ENDIF(DYNAMIC_LINK STREQUAL "OFF")
###################
### DEBUG FLAGS ###
###################

# NOTE: debugging symbols (-g or /debug:full) are already on by default

# Disable optimizations
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG}"
                 Fortran REQUIRED "/Od" # Intel Windows
                                  "-Og" # GNU better debugging flag
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
