      SUBROUTINE ADDSTR (STRING,SIGLEN,TMP)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) STRING,TMP
      INTEGER SIGLEN

**    local variables
      INTEGER ISTART,IS,IL,L
      SAVE

      IS = ISTART (TMP)
      IL = LEN_TRIM (TMP)

      IF (IL.GT.0) then
*        something to add
         L  = IL-IS+1
         IF (SIGLEN+L.GT.LEN (STRING)) then
            write (*,*)
     &       TRIM(STRING), '(LEN=',LEN(STRING),') add:',TRIM(TMP)
            CALL FATALERR ('ADDSTR','internal_1')
         END IF

         STRING(SIGLEN+1:SIGLEN+L) = TMP(IS:IL)
         SIGLEN = SIGLEN+L
      END IF

      RETURN
      END
