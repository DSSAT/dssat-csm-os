      REAL FUNCTION REANOR (X1, X2)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      REAL X1, X2

**    local variables
      SAVE

      IF (X1.LE.0. .AND. X2.LE.0.) THEN
         REANOR = 1.
      ELSE
         REANOR = 0.
      END IF

      RETURN
      END
