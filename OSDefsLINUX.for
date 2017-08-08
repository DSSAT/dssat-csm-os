!=======================================================================
!  MODULE OSDefinitions
!  08/08/2017 WP Written
!=======================================================================

      MODULE OSDefinitions
!     Contains defintion for Windows Platform which are used throughout 
!     the model.

      SAVE

!=======================================================================

      CHARACTER(LEN=1), PARAMETER ::  SLASH = '/'
      character(len=3), PARAMETER ::  exe_string = '.so'
      CHARACTER(LEN=12), PARAMETER :: DSSATPRO = 'DSSATPRO.L46'
      CHARACTER(LEN=11), PARAMETER :: STDPATH = '/DSSAT46/'

!======================================================================
      END MODULE OSDefinitions
!======================================================================
