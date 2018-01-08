*     ---------------------------------------------
*     enter double precision
*     ---------------------------------------------
      SUBROUTINE ENTDOU (QUEST,X)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER QUEST*(*)
      DOUBLE PRECISION X

*     integer codes for tokens
      INCLUDE 'rdndec.gin'

*     value info
      INCLUDE 'rddecinf.inc'

**    local variables
      CHARACTER INPUT*40
      INTEGER TTYPE, IB, IE, NERR, NWAR
      SAVE

*     ask the question and read the answer
10    CONTINUE
      CALL RDERRI ('Keyboard',0,0,.TRUE.,.FALSE.)
      CALL ENTCHA (QUEST,INPUT)
      CALL PARSWORD (INPUT,TTYPE,IB,IE,NERR,NWAR)

      IF (NERR.EQ.0) THEN
         IF (TTYPE .EQ. TFLOAT) THEN
*           floating point value
            X = VFLOAT
         ELSE
*           other data type
            CALL RDERR (2,'Not a floating point value')
            GOTO 10
         END IF
      ELSE
         GOTO 10
      END IF

      RETURN
      END
