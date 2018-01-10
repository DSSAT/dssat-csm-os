      SUBROUTINE DECDOU (IWAR,STRING,VALUE)
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

      IF (NERR.EQ.0) THEN
         IF (TTYPE.EQ.TFLOAT) THEN
*           floating point value
            IWAR  = 0
            VALUE = VFLOAT
         ELSE IF (TTYPE.EQ.TINTEG) THEN
*           convert integer to real
            IWAR  = 0
            VALUE = DBLE(VINT)
         ELSE
            IWAR  = 1
            VALUE = 0.0D0
         END IF
      ELSE
         IWAR  = 1
         VALUE = 0.0D0
      END IF

      RETURN
      END
