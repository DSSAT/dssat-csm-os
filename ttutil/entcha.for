      SUBROUTINE ENTCHA (QUEST,X)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) QUEST,X

**    local variables
      INTEGER LQ,LX,IOS
      SAVE

*     string lengths
      LQ = LEN_TRIM (QUEST)
      LX = LEN (X)

*     ask the question and read the answer
10    CONTINUE
      WRITE (*,'(A50,A,$)') QUEST(1:LQ),': '
      READ (*,'(A)',IOSTAT=IOS) X(1:LX)

      IF (IOS.LT.0) THEN
         STOP ' End_Of_File detected ; program STOP'

      ELSE IF (IOS.GT.0) THEN
*        error during READ : give message and try again
         WRITE (*,'(/,A,/,A,/)')
     $      ' Enter a CHARACTER string !!',
     $      ' Use <CTRL> Z  to STOP (<Command>. on Mac)'
         GOTO 10
      END IF

      RETURN
      END
