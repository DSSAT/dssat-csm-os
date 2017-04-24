!=======================================================================
!  Definition (UNIX conversion)
!  11/11/2005 LAH
!=======================================================================

      MODULE CRSIMDEF

      SAVE

!=======================================================================
! By Willingthon Pavan (2017-04-24):
! Intel ifort understands the C-style preprocessor directives, so it might
! be easiest to convert the files to that style. Then we would have a
! single code base that would work with both compilers (Intel & gfortran).
! For more info: https://software.intel.com/en-us/node/694581
!=======================================================================

#ifdef _WIN32
      CHARACTER(LEN=1),PARAMETER::SLASH = '\' !DOS, Windows
#else
      CHARACTER(LEN=1),PARAMETER::SLASH = '/' !Linux, Unix
#endif

      END MODULE CRSIMDEF
