      SUBROUTINE RDMCHA (X)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) X

**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80          CM
      LOGICAL          L(1),LM

*     other
      INTEGER IL,IS
      SAVE

      CALL RDDATA (10,'RDMCHA',0,0,' ',IS,' ','C',D,R,I,X,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)

      RETURN
      END
