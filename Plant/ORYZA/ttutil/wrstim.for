      SUBROUTINE WRSTIM (XNAME,X)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME
      DOUBLE PRECISION X

**    local variables ; dummy set
      INCLUDE 'wr_sys.inc'
      CHARACTER*80 STRING
      INTEGER STRING_L
      SAVE

      IF (.NOT.INIT) CALL FATALERR ('WRSTIM','system not initialized')

      CALL DTDPST ('YEAR-MONTHST-DAY_HOUR:MINUTE:SECONDS.FSECONDS',
     &              X,STRING)
      STRING_L = LEN_TRIM (STRING)
      WRITE (UNIT,'(1X,3A)') XNAME,' = ',STRING(1:STRING_L)

      RETURN
      END
