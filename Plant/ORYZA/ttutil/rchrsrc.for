      INTEGER FUNCTION RCHRSRC (CHARS,STRING,POSBEG,POSEND)
      IMPLICIT NONE

*     this routine searches for the first occurrence of one of the
*     characters of CHARS in STRING in reverse order ! (Contrary to
*     the intrinsic function INDEX it does not look for a match of
*     the whole string.

*     formal parameters
      CHARACTER*(*) STRING, CHARS
      INTEGER POSBEG, POSEND

*     local parameters
      INTEGER POS
      CHARACTER*1 CHR
      LOGICAL FND
      SAVE

      POS = POSEND

      FND = .FALSE.
10    IF (.NOT.FND.AND.POS.GE.POSBEG) THEN
         CHR = STRING(POS:POS)
         FND = INDEX (CHARS,CHR).NE.0
         IF (.NOT.FND) POS = POS-1
      GOTO 10
      END IF

      RCHRSRC = POS

      RETURN
      END
