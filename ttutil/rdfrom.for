      SUBROUTINE RDFROM (IS,FATAL)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER IS
      LOGICAL FATAL

**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM

*     other
      INTEGER IFATAL
      SAVE

      IF (FATAL) THEN
         IFATAL = 1
      ELSE
         IFATAL = 0
      END IF

*     call for (new) set
      CALL RDDATA (2,'RDFROM',0,0,' ',IS,' ',' ',D,R,I,C,L,
     $             1,1,1,1,1,IFATAL,
     $             DM,RM,IM,CM,LM)

      RETURN
      END
