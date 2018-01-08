      SUBROUTINE RDAREA (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IFND
      REAL X
      DIMENSION X(ILDEC)
      CHARACTER*(*) XNAME

**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL                  RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM

*     other
      INTEGER IS
      SAVE

*     the number of values is set by RDDATA
      CALL RDDATA (6,'RDAREA',0,0,' ',IS,XNAME,'R',
     $             D,X,I,C,L,1,ILDEC,1,1,1,IFND,
     $             DM,RM,IM,CM,LM)

      RETURN
      END
