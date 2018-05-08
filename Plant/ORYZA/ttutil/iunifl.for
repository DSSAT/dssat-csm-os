      INTEGER FUNCTION IUNIFL (LOWB,UPPB)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER LOWB, UPPB

**    Local variables
      REAL UNIFL, DIFF, RAN
      LOGICAL DRAW
      SAVE

      DIFF   = 1./(1.+REAL(UPPB)-REAL(LOWB))

      DRAW = .TRUE.
10    IF (DRAW) THEN
         RAN  = UNIFL()
         DRAW = RAN.EQ.1.
      GOTO 10
      END IF

      IUNIFL = LOWB+INT(RAN/DIFF)
c      write (*,*) lowb,uppb,iunifl
      RETURN
      END
