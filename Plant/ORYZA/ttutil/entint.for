*     ---------------------------------------------
*     enter integer
*     ---------------------------------------------
      SUBROUTINE ENTINT (QUEST,IX)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER QUEST*(*)
      INTEGER IX

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
         IF (TTYPE .EQ. TINTEG) THEN
*           integer
            IX = VINT
         ELSE
*           other data type
            CALL RDERR (2,'Not an INTEGER value')
            GOTO 10
         END IF
      ELSE
         GOTO 10
      END IF

      RETURN
      END
