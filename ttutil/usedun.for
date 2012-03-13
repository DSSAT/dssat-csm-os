      SUBROUTINE USEDUN (IST,IEND)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER IST,IEND

**    Local variables
      INTEGER ISTEP,UNIT,IL,UNLOG
      LOGICAL OPEN,FOUND,TOSCR, TOLOG
      CHARACTER*80 FILE
      SAVE

      ISTEP = 1
      IF (IST.GT.IEND) ISTEP = -1
      IF (IST.LT.10.OR.IST.GT.99.OR.IEND.LT.10.OR.IEND.GT.99)
     &   CALL FATALERR ('GETUN','unit number outside range')

*     desired message output
      CALL MESSINQ (TOSCR, TOLOG, UNLOG)

      FOUND = .FALSE.
      DO UNIT=IST,IEND,ISTEP
         INQUIRE (UNIT=UNIT,OPENED=OPEN)
         IF (OPEN) THEN
            FILE = ' '
            INQUIRE (UNIT=UNIT,NAME=FILE)
            IL = LEN_TRIM (FILE)
            IF (TOSCR) WRITE (*,'(A,I3,2A)')
     &        ' WARNING from USEDUN: Unit: ',UNIT,
     &        ' is in use for file: ',FILE(1:IL)
            IF (TOLOG) WRITE (UNLOG,'(A,I3,2A)')
     &        ' WARNING from USEDUN: Unit: ',UNIT,
     &        ' is in use for file: ',FILE(1:IL)
            FOUND = .TRUE.
         END IF
      END DO

      IF (.NOT.FOUND) CALL MESSWRT ('USEDUN','no open units found')

      RETURN
      END
