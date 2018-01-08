      SUBROUTINE WRSDOU (XNAME,X)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME
      DOUBLE PRECISION X

**    local variables ; dummy set
      INCLUDE 'wr_sys.inc'
      SAVE

      IF (.NOT.INIT) CALL FATALERR ('WRSDOU','system not initialized')
      WRITE (UNIT,'(1X,2A,1P,G23.16)') XNAME,' = ',X

      RETURN
      END
