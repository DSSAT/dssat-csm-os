      SUBROUTINE RDFINT (XNAME,X,ILDEC,IVALS)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IVALS,X
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

      CALL RDDATA (7,'RDFINT',0,0,' ',IS,XNAME,'I',
     $             D,R,X,C,L,1,1,ILDEC,1,1,IVALS,
     $             DM,RM,IM,CM,LM)

      RETURN
      END
