      SUBROUTINE COPFL2 (IIN, FILE, IOUT, HEADER)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER IIN, IOUT
      CHARACTER*(*) FILE
      LOGICAL HEADER

**    local variables
      INTEGER IOS, I1, IL
      LOGICAL LOGTMP, FLEXIST
      CHARACTER LINE*255, LINE2*72
      SAVE


*     check on output file
      INQUIRE (UNIT=IOUT,OPENED=LOGTMP)
      IF (.NOT.LOGTMP) CALL FATALERR ('COPFL2','output file not open')

*     check on input file name
      IL = LEN_TRIM (FILE)
      IF (IL.EQ.0) CALL FATALERR ('COPFL2','no input file name')

      LOGTMP = FLEXIST (FILE(1:IL))
      IF (LOGTMP) THEN
         CALL FOPENG (IIN, FILE(1:IL), 'OLD','FS',0,' ')

         IF (HEADER) THEN
            DO I1=2,71
               LINE2(I1:I1) = '-'
            END DO
            LINE2(1:1) = '*'
            LINE2(72:72) = '*'

            WRITE (IOUT, '(A)')   LINE2
            WRITE (IOUT, '(2A,T72,A)')
     &        '* Contents of input file: ',FILE(1:IL),'*'
            WRITE (IOUT, '(A,/)') LINE2
         END IF

         IOS = 0
10       IF (IOS.EQ.0) THEN
            READ (IIN, '(A)', IOSTAT=IOS) LINE
            IF (IOS.EQ.0) THEN
               I1 = MAX (1, LEN_TRIM (LINE))
               WRITE (IOUT, '(A)') LINE(1:I1)
            END IF
         GOTO 10
         END IF

         CLOSE (IIN)
      END IF

      RETURN
      END
