      SUBROUTINE RDFREA (XNAME,X,ILDEC,IVALS)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IVALS
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

      CALL RDDATA (7,'RDFREA',0,0,' ',IS,XNAME,'R',
     $             D,X,I,C,L,1,ILDEC,1,1,1,IVALS,
     $             DM,RM,IM,CM,LM)

      RETURN
      END
