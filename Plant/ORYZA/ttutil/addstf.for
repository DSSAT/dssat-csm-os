      SUBROUTINE ADDSTF (STRING,SIGLEN,TMP)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) STRING,TMP
      INTEGER SIGLEN

**    local variables
      INTEGER L
      SAVE

      L = LEN (TMP)
      IF (L.EQ.0) CALL FATALERR ('ADDSTF','internal error')

      IF (SIGLEN+L.GT.LEN (STRING)) CALL FATALERR
     &   ('ADDSTF','internal error')

      STRING(SIGLEN+1:SIGLEN+L) = TMP
      SIGLEN = SIGLEN+L

      RETURN
      END
