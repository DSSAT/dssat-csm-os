      LOGICAL FUNCTION DTLEAP (YEAR)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER YEAR

**    no local variables
      SAVE

      DTLEAP = MOD (YEAR,4).EQ.0
      IF (MOD (YEAR,100).EQ.0) DTLEAP = .FALSE.
      IF (MOD (YEAR,400).EQ.0) DTLEAP = .TRUE.

      RETURN
      END

