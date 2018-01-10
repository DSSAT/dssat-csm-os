      REAL FUNCTION REAAND (X1, X2)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      REAL X1, X2

**    local variables
      SAVE

      IF (X1.GT.0. .AND. X2.GT.0.) THEN
         REAAND = 1.
      ELSE
         REAAND = 0.
      END IF

      RETURN
      END
