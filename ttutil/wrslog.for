      SUBROUTINE WRSLOG (XNAME,X)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME
      LOGICAL X

**    local variables ; dummy set
      INCLUDE 'wr_sys.inc'
      SAVE

      IF (.NOT.INIT) CALL FATALERR ('WRSLOG','system not initialized')

      IF (X) THEN
         WRITE (UNIT,'(1X,2A)') XNAME,' = .TRUE.'
      ELSE
         WRITE (UNIT,'(1X,2A)') XNAME,' = .FALSE.'
      END IF

      RETURN
      END
