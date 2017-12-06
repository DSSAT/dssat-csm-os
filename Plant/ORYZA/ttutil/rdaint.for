      SUBROUTINE RDAINT (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IFND,X
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)

**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER               IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM

*     other
      INTEGER IS
      SAVE

*     the number of values is set by RDDATA
      CALL RDDATA (6,'RDAINT',0,0,' ',IS,XNAME,'I',
     $             D,R,X,C,L,1,1,ILDEC,1,1,IFND,
     $             DM,RM,IM,CM,LM)

      RETURN
      END
