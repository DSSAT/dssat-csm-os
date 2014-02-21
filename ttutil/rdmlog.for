      SUBROUTINE RDMLOG (X)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      LOGICAL X

**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM

*     other
      INTEGER IL,IS
      SAVE

      L(1) = X
      CALL RDDATA (10,'RDMLOG',0,0,' ',IS,' ','L',D,R,I,C,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)

      RETURN
      END
