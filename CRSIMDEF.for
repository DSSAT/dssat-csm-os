!=======================================================================
!  Definition (UNIX conversion)
!  11/11/2005 LAH
!=======================================================================

      MODULE CRSIMDEF

      SAVE

#ifdef _WIN32
      CHARACTER(LEN=1),PARAMETER::SLASH = '\' !DOS, Windows
#else
      CHARACTER(LEN=1),PARAMETER::SLASH = '/' !Linux, Unix
#endif
      END MODULE CRSIMDEF
