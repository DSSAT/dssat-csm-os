      SUBROUTINE ADDINT (STRING,SIGLEN,I)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) STRING
      INTEGER SIGLEN,I

**    local variables
      CHARACTER*11 TMP
      INTEGER ISTART,IS,IL,L
      SAVE

      WRITE (TMP,'(I11)') I
      IS = ISTART (TMP)
      IL = LEN_TRIM (TMP)
      L  = IL-IS+1
      IF (SIGLEN+L.GT.LEN (STRING)) CALL FATALERR
     &   ('ADDINT','internal error')
      STRING(SIGLEN+1:SIGLEN+L) = TMP(IS:IL)
      SIGLEN = SIGLEN+L

      RETURN
      END
