*     ---------------------------------------------
*     enter integer with default
*     ---------------------------------------------
      SUBROUTINE ENTDIN (QUEST,IXDEF,IX)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER IXDEF,IX
      CHARACTER*(*) QUEST

*     integer codes for tokens
      INCLUDE 'rdndec.gin'

*     value info
      INCLUDE 'rddecinf.inc'

**    local variables + functions called
      CHARACTER INPUT*40, HULP*40
      INTEGER TTYPE, IB, IE, NERR, NWAR
      SAVE

*     write default to string
      WRITE (HULP,'(I16)') IXDEF

*     ask the question and read the answer
10    CONTINUE
      CALL RDERRI ('Keyboard',0,0,.TRUE.,.FALSE.)
      CALL ENTHLP (QUEST,HULP,INPUT)
      CALL PARSWORD (INPUT,TTYPE,IB,IE,NERR,NWAR)

      IF (NERR.EQ.0) THEN
         IF (TTYPE .EQ. TINTEG) THEN
*           integer
            IX = VINT
         ELSE IF (TTYPE .EQ. TSPACE) THEN
*           default
            IX = IXDEF
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
