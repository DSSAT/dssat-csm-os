      SUBROUTINE DECINT (IWAR,STRING,IVALUE)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER IWAR, IVALUE
      CHARACTER*(*) STRING

**    local variables + functions used
      INCLUDE 'rdndec.gin'
      INCLUDE 'rddecinf.inc'
      INTEGER TTYPE, IB, IE, NERR, NWAR

      SAVE

      CALL RDERRI (' ',0,0,.FALSE.,.FALSE.)

      CALL PARSWORD (STRING,TTYPE,IB,IE,NERR,NWAR)

      IF (NERR.EQ.0.AND.TTYPE.EQ.TINTEG) THEN
*        integer value
         IWAR  = 0
         IVALUE = VINT
      ELSE
         IWAR  = 1
         IVALUE = 0
      END IF

      RETURN
      END
