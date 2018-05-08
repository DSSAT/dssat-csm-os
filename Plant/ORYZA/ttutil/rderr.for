      SUBROUTINE RDERR (ITASK,MESSAG)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ITASK
      CHARACTER*(*) MESSAG

**    Include files
      INCLUDE 'rdrecinf.inc'
      INCLUDE 'rdtblinf.inc'
      INCLUDE 'rdfilinf.inc'
      INCLUDE 'rdstainf.inc'
      INCLUDE 'rderrinf.inc'
      INCLUDE 'lextokin.inc'

*     local variables
      CHARACTER*30 PTRSTR, ERBUF
      CHARACTER LMESS*40, FORM1*26
      PARAMETER (FORM1='(1X,I4,1X,A,1X,3A,/,T49,A)')
      INTEGER I, I1, I2, IH
      LOGICAL THERE

      SAVE

      IF (INERR.EQ.0 .AND. INWAR.EQ.0) THEN
*        first message received ; file handling
         IF (TOSCR) WRITE (*,'(/,1X,2A,/,2(/,T2,A,T7,A,T48,A))')
     $    'Error(s) or Warning(s) while reading from ',
     $     FILEIN(1:ILFILE),
     $    'Line','Error text','Somewhere here',
     $    '====','==========','=============='
         IF (TOLOG) THEN
*           logfile present ?
            INQUIRE (UNIT=IULOG,OPENED=THERE)
            IF (.NOT.THERE)
     $       CALL FOPENS (IULOG,'RDINDX.LOG','NEW','DEL')
            WRITE (IULOG,'(/,1X,2A,/,2(/,T2,A,T7,A,T48,A))')
     $        'Error(s) or Warning(s) while reading from ',
     $         FILEIN(1:ILFILE),
     $        'Line','Error text','Somewhere here',
     $        '====','==========','=============='
         END IF
      END IF

      IF (ITASK.EQ.2 .OR. ITASK.EQ.3) THEN
*        write message with pointer string
         IF (ITASK.EQ.2) INERR = INERR + 1
         IF (ITASK.EQ.3) INWAR = INWAR + 1

*        truncate message
         LMESS = MESSAG
         CALL UPPERC (LMESS(1:1))

*        display line with pointer string
         I2 = MIN (IP+5,STBLEN-1)

         IF (I2.GE.1) THEN
            I1     = MAX (I2-(LEN(ERBUF)-1),1)
            ERBUF  = STBUF(I1:I2)
            PTRSTR = ' '
            DO 10 I=I1,I2
               IH = I-I1+1
               IF (I.EQ.ISP .OR. I.EQ.IP) THEN
                  PTRSTR(IH:IH) = '^'
               ELSE IF (I.GT.ISP .AND. I.LT.IP) THEN
                  IF (IH.GT.1) THEN
                     PTRSTR(IH:IH) = '-'
                  ELSE
                     PTRSTR(IH:IH) = '<'
                  END IF
               END IF
10          CONTINUE
            IF (TOSCR) WRITE (*,FORM1)
     &          RECNO,LMESS,'[',ERBUF(1:I2-I1+1),']',PTRSTR(1:I2-I1+1)
            IF (TOLOG) WRITE (IULOG,FORM1)
     &          RECNO,LMESS,'[',ERBUF(1:I2-I1+1),']',PTRSTR(1:I2-I1+1)
         ELSE
            IF (TOSCR) WRITE (*    ,FORM1) RECNO,LMESS
            IF (TOLOG) WRITE (IULOG,FORM1) RECNO,LMESS
         END IF

      ELSE IF (ITASK.EQ.4) THEN
*        global error ; message includes line number
         INERR = INERR + 1

         I1 = LEN_TRIM (TBLNAM(ICOL))
         I2 = LEN_TRIM (MESSAG)

         IF (TOSCR) WRITE (*,'(1X,I5,4A,/)')
     $      TBLLIN(ICOL),'  Variable ',
     $      TBLNAM(ICOL)(1:I1),': ',MESSAG(1:I2)
         IF (TOLOG) WRITE (IULOG,'(1X,I5,4A,/)')
     $      TBLLIN(ICOL),'  Variable ',
     $      TBLNAM(ICOL)(1:I1),': ',MESSAG(1:I2)

      ELSE
         CALL FATALERR ('RDERR','internal error')
      END IF

      RETURN
      END
