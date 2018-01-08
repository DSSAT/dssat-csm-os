      REAL FUNCTION NOTNUL (X)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      REAL X

**    local variables
      SAVE

      IF (X.NE.0.) THEN
         NOTNUL = X
      ELSE
         NOTNUL = 1.
      END IF

      RETURN
      END
