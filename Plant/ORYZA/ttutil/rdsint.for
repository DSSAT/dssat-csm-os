      SUBROUTINE RDSINT (XNAME,X)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER X
      CHARACTER*(*) XNAME

**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM

*     other
      INTEGER IL,IS
      SAVE

*     read a single INTEGER number (value of IL is arbitrary) !!
      IL = 0
      CALL RDDATA (5,'RDSINT',0,0,' ',IS,XNAME,'I',D,R,I,C,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)

      X = I(1)

      RETURN
      END
