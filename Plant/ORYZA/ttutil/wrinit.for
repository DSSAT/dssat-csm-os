      SUBROUTINE WRINIT (UNIT_X,FILE)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER UNIT_X
      CHARACTER*(*) FILE

**    Include files
      INCLUDE 'wr_sys.inc'

*     Local variables
      LOGICAL OPEN
      INTEGER IL
      SAVE

      Q    = CHAR (39)
      UNIT = UNIT_X

      IL   = LEN_TRIM (FILE)
      IF (IL.EQ.0) CALL FATALERR ('WRINIT','empty file name')

      INQUIRE (UNIT=UNIT,OPENED=OPEN)
      IF (.NOT.OPEN) CALL FOPENS (UNIT,FILE,'NEW','DEL')
      INIT = .TRUE.

      RETURN
      END

      BLOCK DATA WRINIT_DATA
      IMPLICIT NONE

      INCLUDE 'wr_sys.inc'
      SAVE
      DATA INIT /.FALSE./
      DATA SPACE /' '/
      END

