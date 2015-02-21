      SUBROUTINE OPSC (IUNIT,VARNAM,VARMIN,VARMAX,XLAB, &
                       VAR2NM,VAR2MN,VAR2MX,VAR2LB)
!     generates scatter plot of data
      IMPLICIT NONE

!     formal parameters
      INTEGER       IUNIT
      REAL          VARMIN, VARMAX, XLAB, VAR2MN, VAR2MX, VAR2LB
      CHARACTER (*) VARNAM, VAR2NM

!     local parameters
      REAL    VARVAL
      INTEGER ITASK, INCL
      SAVE

      ITASK  = 7
      VARVAL = -99.
      INCL   = -99

      CALL OPSYS (ITASK,IUNIT,VARNAM,VARVAL,VARMIN,VARMAX,XLAB, &
                  VAR2NM,VAR2MN, VAR2MX, VAR2LB,INCL)
      RETURN
      END
