      SUBROUTINE REMOVE (STRING,CHR)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER STRING*(*),CHR*1

**    local variable
      INTEGER I
      SAVE


10    I = INDEX (STRING,CHR)
      IF (I.GT.0) THEN
         STRING(I:I) = ' '
         GOTO 10
      END IF

      RETURN
      END
