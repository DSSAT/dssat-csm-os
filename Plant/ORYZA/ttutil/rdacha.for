      SUBROUTINE RDACHA (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IFND
      CHARACTER*(*) XNAME,X
      DIMENSION X(ILDEC)

**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80          CM
      LOGICAL          L(1),LM

*     other
      INTEGER IS
      SAVE

*     the number of values is set by RDDATA
      CALL RDDATA (6,'RDACHA',0,0,' ',IS,XNAME,'C',
     $             D,R,I,X,L,1,1,1,ILDEC,1,IFND,
     $             DM,RM,IM,CM,LM)

      RETURN
      END
