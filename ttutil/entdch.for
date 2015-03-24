*     ---------------------------------------------
*     enter string with default
*     ---------------------------------------------
      SUBROUTINE ENTDCH (QUEST,SDEF,S)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) QUEST,SDEF,S

**    local variables + functions called
      INTEGER IL
      CHARACTER INPUT*80
      SAVE

      CALL ENTHLP (QUEST,SDEF,INPUT)

*     length of input string
      IL = LEN_TRIM (INPUT)
      IF (IL.EQ.0) THEN
*        default
         S = SDEF
      ELSE
*        new string supplied
         S = INPUT
      END IF

      RETURN
      END
