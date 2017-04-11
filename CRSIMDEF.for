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
! * Using the fpp Preprocessor(INTEL): https://software.intel.com/en-us/node/694581
! * Microsoft Visual Studio IDE: set the Preprocess Source File option to Yes in 
!   the Fortran Preprocessor Option Category.
!=======================================================================

#if defined __APPLE__ || defined __linux__ || defined linux || defined __unix__
      CHARACTER(LEN=1),PARAMETER::SLASH = '/' !Linux, Unix
#else
      CHARACTER(LEN=1),PARAMETER::SLASH = '\' !DOS, Windows

      END MODULE CRSIMDEF
