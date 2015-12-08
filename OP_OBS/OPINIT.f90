      SUBROUTINE OPINIT
      IMPLICIT NONE

!     formal parameters

!     local parameters
      INTEGER       ITASK, IUNIT, INCL
      REAL          VARVAL, VARMIN, VARMAX, XLAB, VAR2MN, VAR2MX, VAR2LB
      CHARACTER (1) VARNAM, VAR2NM
      SAVE

      ITASK = 1
      IUNIT  = -99
      VARNAM = ' '
      VARVAL = -99.
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

