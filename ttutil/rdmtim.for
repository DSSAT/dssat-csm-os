      SUBROUTINE RDMTIM (X)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      DOUBLE PRECISION X

**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM

*     other
      INTEGER IL,IS
      SAVE

      D(1) = X
      CALL RDDATA (10,'RDMTIM',0,0,' ',IS,' ','T',D,R,I,C,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)

      RETURN
      END
