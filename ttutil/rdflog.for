      SUBROUTINE RDFLOG (XNAME,X,ILDEC,IVALS)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IVALS
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

      CALL RDDATA (7,'RDFLOG',0,0,' ',IS,XNAME,'L',
     $             D,R,I,C,X,1,1,1,1,ILDEC,IVALS,
     $             DM,RM,IM,CM,LM)

      RETURN
      END
