      LOGICAL FUNCTION RDINQR (XNAME)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME

**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM

*     other
      INTEGER IFLAG,IL
      SAVE

*     inquire call
      CALL RDDATA (8,'RDINQR',0,0,C(1),IFLAG,XNAME,' ',D,R,I,C,L,
     $             1,1,1,1,1,IL,DM,RM,IM,CM,LM)

*     set flag
      RDINQR = IFLAG.GT.0

      RETURN
      END
