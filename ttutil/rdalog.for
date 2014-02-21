      SUBROUTINE RDALOG (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IFND
      LOGICAL X
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)

**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL               LM

*     other
      INTEGER IS
      SAVE

*     the number of values is set by RDDATA
      CALL RDDATA (6,'RDALOG',0,0,' ',IS,XNAME,'L',
     $             D,R,I,C,X,1,1,1,1,ILDEC,IFND,
     $             DM,RM,IM,CM,LM)

      RETURN
      END
