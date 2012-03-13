      SUBROUTINE RDFCHA (XNAME,X,ILDEC,IVALS)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IVALS
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

      CALL RDDATA (7,'RDFCHA',0,0,' ',IS,XNAME,'C',
     $             D,R,I,X,L,1,1,1,ILDEC,1,IVALS,
     $             DM,RM,IM,CM,LM)

      RETURN
      END
