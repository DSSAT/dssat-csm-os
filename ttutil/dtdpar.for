      SUBROUTINE DTDPAR (DPDTTM,DATEA,FSEC)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER DATEA(6)
      REAL FSEC
      DOUBLE PRECISION DPDTTM

**    Local variables
      INTEGER ITASK,I1
      LOGICAL ERR
      CHARACTER*80 MESSAG
      SAVE

      ITASK = 2
      CALL DTSYS (ITASK,DATEA,FSEC,DPDTTM,ERR,MESSAG)

      IF (ERR) THEN
*        reset everything
         DO 10 I1=1,6
            DATEA(I1) = 0
10       CONTINUE
         FSEC = 0.
         CALL FATALERR ('DTDPAR',MESSAG)
      END IF

      RETURN
      END
