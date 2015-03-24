      INTEGER FUNCTION ISTART (STRING)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) STRING

**    local variables
      INTEGER I, L
      SAVE

      L = LEN (STRING)

      DO 10 I=1,L
         IF (STRING(I:I).NE.' ') THEN
            ISTART = I
            RETURN
         END IF
10    CONTINUE

      ISTART = 0

      RETURN
      END
