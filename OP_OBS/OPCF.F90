      SUBROUTINE OPCF (IUNIT,VARNAM,VARMIN,VARMAX,XLAB)
      IMPLICIT NONE

!     generates cumulative frequency plot of data

!     formal parameters
      INTEGER       INCL,IUNIT
      CHARACTER (*) VARNAM
      REAL          VARMIN,VARMAX,XLAB

!     local parameters
      INTEGER       ITASK
      REAL          VARVAL, VAR2MN, VAR2MX, VAR2LB
      CHARACTER (1) VAR2NM
      SAVE

      ITASK  = 5
      VARVAL = -99.
      INCL   = -99
      VAR2NM = ' '
      VAR2MN = -99.
      VAR2MX = -99.
      VAR2LB = -99.

      CALL OPSYS (ITASK,IUNIT,VARNAM,VARVAL,VARMIN,VARMAX,XLAB, &
                  VAR2NM,VAR2MN,VAR2MX,VAR2LB,INCL)

      RETURN
      END
