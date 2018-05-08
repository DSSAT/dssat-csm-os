*     ---------------------------------------------
*     enter Yes/No with default
*     ---------------------------------------------
      SUBROUTINE ENTDYN (QUEST,YNDEF,YN)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER QUEST*(*)
      LOGICAL YNDEF,YN

*     integer codes for tokens
      INCLUDE 'rdndec.gin'

**    local variables
      CHARACTER INPUT*40,HULP*1,YES*3,NO*3
      INTEGER TTYPE, IB, IE, NERR, NWAR, IL
      SAVE

      DATA YES/'YES'/, NO/'NO '/

*     write default to string
      IF (YNDEF) THEN
         HULP = 'Y'
      ELSE
         HULP = 'N'
      END IF

*     ask the question and read the answer
10    CONTINUE
      CALL RDERRI ('Keyboard',0,0,.TRUE.,.FALSE.)
      CALL ENTHLP (QUEST,HULP,INPUT)
      CALL PARSWORD (INPUT,TTYPE,IB,IE,NERR,NWAR)

      IF (NERR.EQ.0) THEN
         IL = IE-IB+1
         IF (TTYPE .EQ. TIDENT .AND. IL.LE.3) THEN
            CALL UPPERC (INPUT(IB:IE))
            IF (INPUT(IB:IE).EQ.YES(1:IL)) THEN
*              yes
               YN = .True.
            ELSE IF (INPUT(IB:IE).EQ.NO(1:IL)) THEN
*              no
               YN = .False.
            ELSE
*              wrong
               CALL RDERR (2,'This is not Yes or No')
               GOTO 10
            END IF
         ELSE IF (TTYPE .EQ. TSPACE) THEN
*           default
            YN = YNDEF
         ELSE
*           other data type
            CALL RDERR (2,'This is not Yes or No')
            GOTO 10
         END IF
      ELSE
         GOTO 10
      END IF

      RETURN
      END
