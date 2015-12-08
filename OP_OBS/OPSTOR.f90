      SUBROUTINE OPSTOR (VARNAM,VARVAL)
      IMPLICIT NONE

!     formal parameters

      CHARACTER (*) VARNAM
      REAL          VARVAL
!     local parameters
      INTEGER       ITASK, IUNIT, INCL
      REAL          VARMIN, VARMAX, XLAB, VAR2MN, VAR2MX, VAR2LB
      CHARACTER (1) VAR2NM
      SAVE

      ITASK  = 2
      IUNIT  = -99
      VARMIN = -99.
      VARMAX = -99.
      XLAB   = -99.
      VAR2NM = ' '
      VAR2MN = -99.
      VAR2MX = -99.
      VAR2LB = -99.
      INCL   = -99

      CALL OPSYS (ITASK,IUNIT,VARNAM,VARVAL,VARMIN,VARMAX,XLAB, &
                  VAR2NM,VAR2MN,VAR2MX,VAR2LB,INCL)

      RETURN
      END

