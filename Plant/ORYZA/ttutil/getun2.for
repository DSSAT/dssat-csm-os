      INTEGER FUNCTION GETUN2 (IST,IEND,NUM)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER IST,IEND,NUM

**    Local variables
      INTEGER UNIT1,UNIT2
      LOGICAL OPEN_FIRST,FOUND,TMP,OPEN_REST
      SAVE

      IF (IST.GT.IEND.OR.
     &    IST.LT.10.OR.IST.GT.999.OR.IEND.LT.10.OR.IEND.GT.999)
     &   CALL FATALERR ('GETUN2','unit number range invalid')
      IF (NUM.LT.1) CALL FATALERR ('GETUN2','invalid value of NUM')

      GETUN2 = -1
      UNIT1  = IST
      FOUND  = .FALSE.

10    IF (.NOT.FOUND.AND.UNIT1.NE.IEND) THEN
         INQUIRE (UNIT=UNIT1,OPENED=OPEN_FIRST)
C        OPEN_FIRST = TEST (UNIT1)
         IF (.NOT.OPEN_FIRST) THEN
*           first file in requested range is not open, see if
*           rest is open
            IF (NUM.GE.2) THEN
*              search for remaining open units
               OPEN_REST = .FALSE.
               DO UNIT2=UNIT1+1,UNIT1+NUM-1
                  INQUIRE (UNIT=UNIT2,OPENED=TMP)
C                 TMP = TEST (UNIT2)
                  IF (TMP) OPEN_REST = .TRUE.
               END DO

               IF (OPEN_REST) THEN
*                 there is a file open in the rest of the range
                  UNIT1 = UNIT1+NUM
               ELSE
*                 no open file found, routine was succesfull
                  FOUND = .TRUE.
               END IF
            ELSE
               FOUND = .TRUE.
            END IF
         ELSE
            UNIT1 = UNIT1+1
         END IF
      GOTO 10
      END IF

      IF (.NOT.FOUND) CALL FATALERR
     &   ('GETUN2','cannot find free unit number')

      GETUN2 = UNIT1

      RETURN
      END
