      SUBROUTINE WRSCHA (XNAME,X)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME,X

**    local variables ; dummy set
      INCLUDE 'wr_sys.inc'
      INTEGER X_L
      SAVE

      IF (.NOT.INIT) CALL FATALERR ('WRSCHA','system not initialized')

      X_L = MAX (1,LEN_TRIM (X))
      WRITE (UNIT,'(1X,5A)') XNAME,' = ',Q,X(1:X_L),Q

      RETURN
      END
