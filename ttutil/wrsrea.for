      SUBROUTINE WRSREA (XNAME,X)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME
      REAL X

**    local variables ; dummy set
      INCLUDE 'wr_sys.inc'
      SAVE

      IF (.NOT.INIT) CALL FATALERR ('WRSREA','system not initialized')
      WRITE (UNIT,'(1X,2A,1P,G13.6)') XNAME,' = ',X

      RETURN
      END

