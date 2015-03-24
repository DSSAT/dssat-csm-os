      SUBROUTINE GETREC (IUNIT,RECORD,EOF)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER IUNIT
      CHARACTER RECORD*(*)
      LOGICAL EOF

**    local variables
      INTEGER ISTAT
      LOGICAL STAR
      SAVE

*     repeat until non-comment record or EOF is found
      STAR = .TRUE.
      EOF = .FALSE.
10    IF (STAR .AND. .NOT.EOF) THEN

         READ (IUNIT,'(A)',IOSTAT=ISTAT) RECORD
         EOF = ISTAT.LT.0 .OR. RECORD(1:1).EQ.CHAR(26)
         STAR = RECORD(1:1).EQ.'*' .OR. RECORD(1:2).EQ.' *'

      GOTO 10
      END IF

      RETURN
      END
