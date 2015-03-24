      SUBROUTINE WRSINT (XNAME,X)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME
      INTEGER X

**    local variables ; dummy set
      INCLUDE 'wr_sys.inc'
      SAVE

      IF (.NOT.INIT) CALL FATALERR ('WRSINT','system not initialized')
      WRITE (UNIT,'(1X,2A,I11)') XNAME,' = ',X

      RETURN
      END
