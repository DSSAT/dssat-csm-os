      SUBROUTINE DTDPST (FORM,DPDTTM,STRNG)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) FORM, STRNG
      DOUBLE PRECISION DPDTTM

**    Local variables
      INTEGER DATEA(6)
      REAL FSEC,FSEC2

      INTEGER MNW
      PARAMETER (MNW=20)
      INTEGER IWBEG(MNW),IWEND(MNW)

      INTEGER I1,I2,IFNDO,IS,IE,ILTMPS,ILFORM,IC,IFND
      INTEGER IFINDG,ILSTR,IDUM
      LOGICAL CHRNOW,WASCHR,FSFND
      CHARACTER LFORM*80

*     declaration of strings to hold formatted values, note
*     the string STR should be declared at the maximum length of
*     YR, MO etc. and MONN
      CHARACTER YR*4,MO*2,DAY*2,HR*2,MN*2,SEC*2,FS*8
      CHARACTER TMPS*9

*     declare edit names
      INTEGER NEDNAM
      PARAMETER (NEDNAM=9)
      CHARACTER EDNAM(NEDNAM)*8

*     declare month names
      CHARACTER MONN(12)*9
      SAVE

      DATA  EDNAM /'YEAR','MONTH','DAY',
     &             'HOUR','MINUTE','SECONDS','FSECONDS',
     &             'MONTHST','MONTHLT'/

      DATA MONN /'January','February','March','April',
     &           'May','June','July','August',
     &           'September','October','November','December'/

*     convert double precision time to array
      CALL DTDPAR (DPDTTM,DATEA,FSEC)

*     initial
      DO 10 I1=1,MNW
         IWBEG(I1) = 0
         IWEND(I1) = 0
10    CONTINUE

*     make format string local and convert to uppercase
      ILFORM = LEN_TRIM (FORM)
      IF (ILFORM.EQ.0) CALL FATALERR ('DTDPST','empty format string')
      IF (ILFORM.GT.LEN(FORM)) CALL FATALERR ('DTDPST',
     &    'format string too long')
      LFORM = ' '
      LFORM = FORM(1:ILFORM)
      CALL UPPERC (LFORM)

*     search through format specification
      IFND   = 0
      WASCHR = .FALSE.
      DO 20 I1=1,ILFORM
         IC = ICHAR (LFORM(I1:I1))
*        all letters A-Z and a-z are valid
         CHRNOW = (IC.GE.65.AND.IC.LE.90).OR.(IC.GE.97.AND.IC.LE.122)

         IF (.NOT.WASCHR .AND. CHRNOW) THEN
*           start new edit descriptor
            IFND = IFND + 1
            IWBEG(IFND) = I1
         ELSE IF (WASCHR.AND..NOT.CHRNOW) THEN
*           end of edit descriptor found
            IWEND(IFND) = I1 - 1
            IF (IFND.EQ.MNW) CALL FATALERR ('DTDPST',
     &         'date/time format too long')
         END IF

         WASCHR = CHRNOW

20    CONTINUE
      IF (WASCHR) IWEND(IFND) = ILFORM

*     search string for FSEC edit descriptor, set FSFND to .true. when
*     found
      FSFND = .FALSE.
      DO 30 I1=1,IFND
         CALL SFINDG (EDNAM,NEDNAM,1,NEDNAM,
     &                LFORM(IWBEG(I1):IWEND(I1)),2,
     &                IFINDG,IDUM)
         IF (IFINDG.EQ.7) FSFND = .TRUE.
30    CONTINUE

*     write date values on strings
      WRITE (YR ,'(I4.4)') DATEA(1)
      WRITE (MO ,'(I2.2)') DATEA(2)
      WRITE (DAY,'(I2.2)') DATEA(3)
      WRITE (HR ,'(I2.2)') DATEA(4)
      WRITE (MN ,'(I2.2)') DATEA(5)
      IF (FSFND) THEN
*        fractional seconds found in format string, do not round
         WRITE (SEC,'(I2.2)') DATEA(6)

*        round to six digit accuracy
         FSEC2 = REAL (INT (FSEC*1000000.))/1000000.
         WRITE (FS ,'(F8.6)') FSEC2
         IF (FS(1:1).EQ.' ') FS(1:1) = '0'
      ELSE
*        fractional seconds not found in format string,
*        round seconds off
         I2 = DATEA(6)
         IF (FSEC.GE.0.5) I2 = I2+1
         WRITE (SEC,'(I2.2)') I2
         FS = ' '
      END IF

      STRNG  = ' '
      ILSTR  = LEN (STRNG)
      ILTMPS = 0
      IS     = 0
      IE     = 0
      IF (IWBEG(1).GT.1) THEN
*        separators present at beginning of format string
         IS = IE+1
         IE = IS+(IWBEG(1)-1)-1
         IF (IE.GT.ILSTR) CALL FATALERR
     &      ('DTDPST','output string too small')
         STRNG(IS:IE) = LFORM(1:IWBEG(1)-1)
      END IF

*     go through edit descriptors, look them up in the list of valid
*     descriptors write values onto output string and add separators in
*     between
      IFINDG = 6
      DO 40 I1=1,IFND

*        search in list of edit descriptors
         IFNDO = IFINDG
         CALL SFINDG (EDNAM,NEDNAM,1,NEDNAM,
     &                LFORM(IWBEG(I1):IWEND(I1)),2,
     &                IFINDG,IDUM)

         IF (IFINDG.EQ.1) THEN
*           year descriptor found
            TMPS = YR
         ELSE IF (IFINDG.EQ.2) THEN
*           month descriptor found
            TMPS = MO
         ELSE IF (IFINDG.EQ.3) THEN
*           day descriptor found
            TMPS = DAY
         ELSE IF (IFINDG.EQ.4) THEN
*           hour descriptor found
            TMPS = HR
         ELSE IF (IFINDG.EQ.5) THEN
*           minute descriptor found
            TMPS = MN
         ELSE IF (IFINDG.EQ.6) THEN
*           seconds descriptor found
            TMPS = SEC
         ELSE IF (IFINDG.EQ.7) THEN
*           fractional seconds descriptor found

*           check if FSEC was preceded by nothing or a SEC descriptor
            IF (IFNDO.NE.6) CALL FATALERR ('DTDPST',
     &      'Fractional seconds not allowed after previous descriptor')

*           remove zero and dot when the FSEC edit descriptor was
*           preceded by a dot

            I2 = LEN_TRIM (FS)
            IF (IE.GE.1) THEN
               IF (STRNG(IE:IE).EQ.'.') THEN
                  TMPS = FS(3:I2)
               ELSE
                  TMPS = FS(1:I2)
               END IF
            ELSE
               TMPS = FS(1:I2)
            END IF

         ELSE IF (IFINDG.EQ.8) THEN
*           MON_ST found, use month in short text
*           notation (JAN, FEB etc.)
            TMPS = MONN(DATEA(2))(1:3)
         ELSE IF (IFINDG.EQ.9) THEN
*           MON_ST found, use month in long text notation (JANUARY etc.)
            TMPS = MONN(DATEA(2))
         ELSE
            CALL MESSWRT ('Edit descriptor',LFORM(IWBEG(I1):IWEND(I1)))
            CALL FATALERR ('DTDPST','Date/time edit descriptor unknown')
         END IF

*        write to output string
         ILTMPS = LEN_TRIM (TMPS)
         IS     = IE+1
         IE     = IS+ILTMPS-1
         IF (IE.GT.ILSTR) CALL FATALERR
     &      ('DTDPST','output string too small')
         STRNG(IS:IE) = TMPS(1:ILTMPS)

         IF (I1.LT.IFND) THEN
*           add separators
            IS = IE+1
            IE = IS+(IWBEG(I1+1)-IWEND(I1)-1)-1
            IF (IE.GT.ILSTR) CALL FATALERR
     &         ('DTDPST','output string too small')
            STRNG(IS:IE) = LFORM(IWEND(I1)+1:IWBEG(I1+1)-1)
         ELSE IF (IWEND(IFND).LT.ILFORM) THEN
*           add separators if format string does not end
*           at last separator
            IS = IE+1
            IE = IS+(ILFORM+1-IWEND(I1)-1)-1
            IF (IE.GT.ILSTR) CALL FATALERR
     &         ('DTDPST','output string too small')
            STRNG(IS:IE) = LFORM(IWEND(I1)+1:ILFORM)
         END IF
40    CONTINUE

      RETURN
      END
