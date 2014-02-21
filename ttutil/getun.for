      INTEGER FUNCTION GETUN (IST,IEND)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER IST,IEND

**    Local variables
      INTEGER I1
      LOGICAL OPEN
      SAVE

      IF (IST.GT.IEND.OR.
     &    IST.LT.10.OR.IST.GT.999.OR.IEND.LT.10.OR.IEND.GT.999)
     &   CALL FATALERR ('GETUN','unit number outside range')

      GETUN = -1
      I1    = IST
      OPEN  = .TRUE.

10    IF (OPEN.AND.I1.NE.IEND) THEN
         INQUIRE (UNIT=I1,OPENED=OPEN)
         IF (.NOT.OPEN) GETUN = I1
         I1 = I1+1
      GOTO 10
      END IF

      IF (OPEN) CALL FATALERR ('GETUN','cannot find free unit number')

      RETURN
      END
