      SUBROUTINE LOWERC (STRING)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) STRING

**    local variables
      INTEGER I,IC,L
      SAVE

      L = LEN (STRING)
      DO 10 I=1,L
*        convert uppercase letters
         IC = ICHAR(STRING(I:I))
         IF (IC.GE.65.AND.IC.LE.90) STRING(I:I) = CHAR(IC+32)
10    CONTINUE

      RETURN
      END
