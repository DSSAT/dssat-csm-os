      SUBROUTINE RDSCHA (XNAME,X)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME,X

**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      LOGICAL          L(1),LM
      CHARACTER*80          CM

*     other
      INTEGER IL,IS
      SAVE

*     read a single CHARACTER value (value of IL is arbitrary) !!
      IL = 0
      CALL RDDATA (5,'RDSCHA',0,0,' ',IS,XNAME,'C',D,R,I,X,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)

      RETURN
      END
