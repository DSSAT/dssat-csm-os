      SUBROUTINE RDFTIM (XNAME,X,ILDEC,IVALS)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IVALS
      DOUBLE PRECISION X
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)

**    local variables ; dummy set
      DOUBLE PRECISION      DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM

*     other
      INTEGER IS
      SAVE

      CALL RDDATA (7,'RDFTIM',0,0,' ',IS,XNAME,'T',
     $             X,R,I,C,L,ILDEC,1,1,1,1,IVALS,
     $             DM,RM,IM,CM,LM)

      RETURN
      END
