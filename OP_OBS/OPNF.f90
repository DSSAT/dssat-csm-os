      SUBROUTINE OPNF (IUNIT,VARNAM,VARMIN,VARMAX,INCL)
      IMPLICIT NONE
!     generates normal frequency plot of data

!     formal parameters
      INTEGER       INCL,IUNIT
      CHARACTER (*) VARNAM
      REAL          VARMIN,VARMAX

!     local parameters
      INTEGER       ITASK
      REAL          VARVAL, XLAB, VAR2MN, VAR2MX, VAR2LB
      CHARACTER (1) VAR2NM
      SAVE

      ITASK  = 6
      VARVAL = 0.
      XLAB   = -99.
      VAR2NM = ' '
      VAR2MN = -99.
      VAR2MX = -99.
      VAR2LB = -99.

      CALL OPSYS (ITASK,IUNIT,VARNAM,VARVAL,VARMIN,VARMAX,XLAB, &
                  VAR2NM,VAR2MN, VAR2MX, VAR2LB,INCL)

      RETURN
      END
