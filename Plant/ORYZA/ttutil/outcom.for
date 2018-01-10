      SUBROUTINE OUTCOM (STR)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) STR

**    local variables and used functions
      INTEGER ICOM, NCOM, I1, IL, ILU2, IFINDC
      PARAMETER (NCOM=25)
      CHARACTER*80 COMMNT(NCOM), TMP
      LOGICAL OPEN
      SAVE

      DATA ICOM /0/, ILU2 /0/

      IF (STR.EQ.'<INIT$$$>') THEN
         ICOM = 0
      ELSE IF (STR.EQ.'<PRINT$$$>') THEN
         IF (ICOM.GT.0) THEN
            CALL AMBUSY (2,'OUTDAT',ILU2)
            IF (ILU2.EQ.0) CALL FATALERR ('OUTCOM',
     &         'No unit number for output file')
            INQUIRE (UNIT=ILU2, OPENED=OPEN)
            IF (.NOT.OPEN) CALL FATALERR
     &         ('OUTCOM','Output file not open')
            DO 10 I1=1,ICOM
               IL = LEN_TRIM (COMMNT(I1))
               WRITE (ILU2,'(2A)') '* ',COMMNT(I1)(1:IL)
10          CONTINUE
         END IF
      ELSE
         IF (ICOM.LT.NCOM) THEN
            IL  = LEN_TRIM (STR)
            TMP = STR
            I1  = IFINDC (COMMNT, NCOM, 1, ICOM, TMP)
            IF (IL.GT.0 .AND. I1.EQ.0) THEN
               ICOM = ICOM+1
               COMMNT(ICOM) = STR
            END IF
         ELSE
            CALL FATALERR ('OUTCOM','Too many comment lines')
         END IF
      END IF

      RETURN
      END
