      SUBROUTINE RDINIT (IUNIT,IULOG,DATFIL)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER IUNIT,IULOG
      CHARACTER*(*) DATFIL

**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM

*     other
      INTEGER IL,IS
      SAVE

*     initialize
      IL = 0
      CALL RDDATA (3,'RDINIT',IUNIT,IULOG,DATFIL,IS,' ',' ',
     $             D,R,I,C,L,1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)

      RETURN
      END
