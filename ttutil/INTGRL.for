      REAL FUNCTION INTGRL (STATE, RATE, DELT)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      REAL STATE, RATE, DELT
 
**    local variables
      SAVE
 
      INTGRL = STATE + RATE * DELT
 
      RETURN
      END
 
