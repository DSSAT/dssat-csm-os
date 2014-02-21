*     ---------------------------------------------
*     question with default string, returns typed string
*     ---------------------------------------------
      SUBROUTINE ENTHLP (QUEST,SDEF,INPUT)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) QUEST,SDEF,INPUT

**    local variables + functions called
      INTEGER IP1,IP2,ISTART,LQ
      CHARACTER RECORD*80,SDLOC*80,VRAAG*50
      SAVE

*     local copy default
      SDLOC = SDEF
      IP1 = ISTART (SDLOC)
      IP2 = LEN_TRIM (SDLOC)
      IP1 = MAX (1,IP1)
      IP2 = MAX (1,IP2)

*     question length limited by available space
      LQ = LEN_TRIM (QUEST)
      LQ = MAX (1,LQ)
      LQ = MIN (LQ,47)

*     ask the question and read the answer
      VRAAG(1:LQ) = QUEST(1:LQ)
*     concatenation with local string (IBM !!)
      IF (LQ+IP2-IP1+5.LE.50) THEN
*        question and default on one line
         RECORD = VRAAG(1:LQ)//' ['//SDLOC(IP1:IP2)//']'
      ELSE
*        question on separate line
         WRITE (*,'(A48)') VRAAG(1:LQ)
         RECORD = ' ['//SDLOC(IP1:IP2)//']'
      END IF
      CALL ENTCHA (RECORD,INPUT)

      RETURN
      END
