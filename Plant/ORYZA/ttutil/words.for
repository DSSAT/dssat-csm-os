      SUBROUTINE WORDS (RECORD,ILW,SEPARS,IWBEG,IWEND,IFND)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ILW,IWBEG,IWEND,IFND
      DIMENSION IWBEG(ILW),IWEND(ILW)
      CHARACTER*(*) RECORD,SEPARS

**    local variables
      INTEGER I,ICHR
      CHARACTER CHAR*1
      LOGICAL SEPAR,GOING
      SAVE


*     initial
      ICHR = LEN (RECORD)
      IFND = 0

      DO 10 I=1,ILW
         IWBEG(I) = 0
         IWEND(I) = 0
10    CONTINUE
      GOING = .FALSE.

*     search through record
      DO 20 I=1,ICHR
         CHAR  = RECORD(I:I)
         SEPAR = (INDEX(SEPARS,CHAR).NE.0)

         IF (.NOT.SEPAR .AND. .NOT.GOING) THEN
*           start new word
            IFND = IFND + 1
            IWBEG(IFND) = I
            GOING = .TRUE.
         END IF

         IF (SEPAR .AND. GOING) THEN
*           end of word found
            IWEND(IFND) = I - 1
            IF (IFND.EQ.ILW) RETURN
            GOING = .FALSE.
         END IF

20    CONTINUE
      IF (GOING) IWEND(IFND)=ICHR

      RETURN
      END
