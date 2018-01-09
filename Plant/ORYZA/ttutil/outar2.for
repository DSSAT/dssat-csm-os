      SUBROUTINE OUTAR2 (NAME, ARRAY, LDEC, UDEC, I1, I2)
      use Module_OutDat
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) NAME
      INTEGER LDEC,UDEC,I1,I2
      REAL ARRAY(LDEC:UDEC)

**    local variables
      CHARACTER IND(0:99)*5, TEMP*36
      INTEGER I3, IL, STEP, UNLOG
      LOGICAL INIT, TOSCR, TOLOG
      SAVE

      DATA INIT /.FALSE./

*     desired message output
      CALL MESSINQ (TOSCR, TOLOG, UNLOG)

*     initialize character array of subscripts
      IF (.NOT.INIT) THEN
         DO 10 I3=0,99
            IF (I3.GE.0.AND.I3.LE.9)
     &          WRITE (IND(I3),'(A,I1,A)') '(',I3,')'
            IF (I3.GT.9.AND.I3.LE.99)
     &          WRITE (IND(I3),'(A,I2,A)') '(',I3,')'
10       CONTINUE
         INIT = .TRUE.
      END IF

      IF (I1.LT.LDEC.OR.I1.GT.UDEC.OR.
     &    I2.LT.LDEC.OR.I2.GT.UDEC) THEN
         IF (TOSCR) WRITE (*,*) 'ERROR in OUTAR2: ',
     &               'output range of ',NAME,' outside declared size'
         IF (TOLOG) WRITE (UNLOG,*) 'ERROR in OUTAR2: ',
     &               'output range of ',NAME,' outside declared size'
         CALL FATALERR (' ',' ')
      END IF

      STEP = 1
      IF (I2.LT.I1) STEP = -1

      IL = LEN_TRIM (NAME)

*     dump names and values from start element to finish element

      DO 20 I3=I1,I2,STEP

         IF (I3.GE.0.AND.I3.LE.9) THEN
*           index was created in local character array,
*           concatenation with name
            TEMP = NAME(1:IL)//IND(I3)
         ELSE IF (I3.GE.10.AND.I3.LE.99) THEN
*           index was created in local character array,
*           concatenation with name
            TEMP = NAME(1:IL)//IND(I3)
         ELSE
*           index was not created in local character array,
*           index should be prepared
            IF (I3.GE.100.AND.I3.LE.999) THEN
               WRITE (TEMP,'(2A,I3,A)') NAME(1:IL),'(',I3,')'
            ELSE IF (I3.GE.-99.AND.I3.LE.-10) THEN
               WRITE (TEMP,'(2A,I3,A)') NAME(1:IL),'(',I3,')'
            ELSE IF (I3.GE.-9.AND.I3.LE.-1) THEN
               WRITE (TEMP,'(2A,I2,A)') NAME(1:IL),'(',I3,')'
            ELSE
               CALL FATALERR ('OUTAR2', 'subscript out of range')
            END IF
         END IF

         IF (I1.NE.I2) THEN
*           range of array is wanted (not a single element)
*           add symbol to make clear begin and end of array
            IF (TEMP(36:36).NE.' ') CALL FATALERR
     &         ('OUTAR2','string too long')
            IF (I3.EQ.I1) THEN
*              element is beginning of array
               TEMP(36:36) = 'B'
            ELSE IF (I3.EQ.I2) THEN
*              element is end of array
               TEMP(36:36) = 'E'
            ELSE
*              element is somewhere in array
               TEMP(36:36) = '.'
            END IF
         END IF

*        send array element to OUTDAT
         CALL OUTDAT (2, 0, TEMP, ARRAY(I3))

20    CONTINUE

      RETURN
      END
