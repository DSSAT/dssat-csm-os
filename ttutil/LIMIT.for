      REAL FUNCTION LIMIT (MIN,MAX,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      REAL MIN,MAX,X
 
**    local parameters
      CHARACTER MESSAG*52
      SAVE
 
      IF (MAX.LT.MIN) THEN
*        minimum is larger than maximum, should generate an error
         WRITE (MESSAG, '(2(A,G11.5))')
     &   'argument error, MIN = ',MIN,', MAX = ',MAX
         CALL FATALERR ('LIMIT',MESSAG)
      END IF
 
      IF (X.LT.MIN) THEN
*        X below allowed range ; return lower bound
         LIMIT = MIN
 
      ELSE IF (X.LE.MAX) THEN
*        X in range ; return X
         LIMIT = X
 
      ELSE
*        X above allowed range ; return upper bound
         LIMIT = MAX
 
      END IF
 
      RETURN
      END
