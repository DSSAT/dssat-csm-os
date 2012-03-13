*     ---------------------------------------------
*     enter double precision with default
*     ---------------------------------------------
      SUBROUTINE ENTDDO (QUEST,XDEF,X)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      DOUBLE PRECISION XDEF,X
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
      WRITE (HULP,'(1P,G20.8)') XDEF

*     ask the question and read the answer
10    CONTINUE
      CALL RDERRI ('Keyboard',0,0,.TRUE.,.FALSE.)
      CALL ENTHLP (QUEST,HULP,INPUT)
      CALL PARSWORD (INPUT,TTYPE,IB,IE,NERR,NWAR)

      IF (NERR.EQ.0) THEN
         IF (TTYPE .EQ. TFLOAT) THEN
*           floating point value
            X = VFLOAT
         ELSE IF (TTYPE .EQ. TSPACE) THEN
*           default
            X = XDEF
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
