      REAL FUNCTION INSW (X1,X2,X3)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      REAL X1,X2,X3
 
**    local variables
      SAVE
 
      IF (X1.LT.0.) THEN
         INSW = X2
      ELSE
         INSW = X3
      END IF
 
      RETURN
      END
