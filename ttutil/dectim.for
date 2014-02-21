      SUBROUTINE DECTIM (IWAR,STRING,VALUE)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER IWAR
      DOUBLE PRECISION VALUE
      CHARACTER*(*) STRING

**    local variables + functions used
      INCLUDE 'rdndec.gin'
      INCLUDE 'rddecinf.inc'
      INTEGER TTYPE, IB, IE, NERR, NWAR

      SAVE

      CALL RDERRI (' ',0,0,.FALSE.,.FALSE.)

      CALL PARSWORD (STRING,TTYPE,IB,IE,NERR,NWAR)

      IF (NERR.EQ.0 .AND. TTYPE.EQ.TTIME) THEN
         IWAR  = 0
         VALUE = VTIME
      ELSE
         IWAR  = 1
         VALUE = 0.0D0
      END IF

      RETURN
      END
