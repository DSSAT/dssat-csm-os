!=======================================================================
!  Definition (UNIX conversion)
!  11/11/2005 LAH
!=======================================================================

      MODULE CRSIMDEF

      SAVE

      !DEC$ IF DEFINED(__linux__)
          CHARACTER(LEN=1),PARAMETER::SLASH = '/' !Linux, Unix
      !DEC$ ELSE IF DEFINED (__APPLE__)
          CHARACTER(LEN=1),PARAMETER::SLASH = '/' !Linux, Unix
      !DEC$ ELSE
          CHARACTER(LEN=1),PARAMETER::SLASH = '\' !DOS, Windows
      !DEC$ END IF

      END MODULE CRSIMDEF
