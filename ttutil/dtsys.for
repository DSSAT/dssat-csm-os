      SUBROUTINE DTSYS (ITASK,DATEA,FSEC,DPDTTM,ERR,MESSAG)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ITASK, DATEA(6)
      LOGICAL ERR
      REAL FSEC
      DOUBLE PRECISION DPDTTM
      CHARACTER*(*) MESSAG

**    Local variables
      INTEGER I1, NZERO, DAYMAX, NDAYS
      INTEGER IMOTB1(12),IMOTB2(12)
      DOUBLE PRECISION DTMP, DTMAX
      LOGICAL DTLEAP
      INTEGER INDAY, ISECS
      DOUBLE PRECISION FRDAY, DSECS, FSECS

*     The absolute day number belonging to a certain day is
*     the number of days passed since 1-jan-0001. Hence, the
*     absolute day number of 1-jan-0001 is 0. The absolute
*     day number belonging to 1-jan-0002 is 365.
*     The number of days N calculated for a certain year Y is
*     given by N = Y*365 + Y/4 - Y/100 + Y/400 - 1.
*     The 1 subtracted simplifies the use of DOY numbers.
*     For 1-jan-0002 for instance DOY=1. If this number is added
*     to the N = 364 calculated for the passed year, year 1, we
*     have again the absolute day number belonging to 1-jan-0002.
*     The offset value used in the TTUTIL day counting should
*     give the number 1 to 1-jan-1900, which implies:
*     ttutiloffset = 1899 * 365 + 1899/4 - 1899/100 + 1899/400 - 1.
*     Dates below 1-jan-0001 and above 31-dec-9999 are illegal.

      INTEGER OFFSET
      PARAMETER (OFFSET = 693594)
      INTEGER N400, N100, N4, N1, LEFT, LASTY, DLAST
      SAVE

      DATA IMOTB1 /0,31,59,90,120,151,181,212,243,273,304,334/
      DATA IMOTB2 /31,28,31,30,31,30,31,31,30,31,30,31/

      ERR    = .FALSE.
      MESSAG = ' '

*     value corresponding to 31-dec-9999 24:00
      DTMAX = DBLE (9999*365 + 9999/4 - 9999/100 + 9999/400 - OFFSET)

      IF (ITASK.EQ.1) THEN

*        convert from array to double precision real
         NZERO  = 0
         DO 10 I1=1,3
            IF (DATEA(I1).EQ.0) NZERO = NZERO+1
10       CONTINUE

         IF (NZERO.GT.0.AND.NZERO.LT.3) THEN
            ERR    = .TRUE.
            MESSAG = 'year 0 or partial date not allowed'
            RETURN
         END IF

*        days for the year
         IF (DATEA(1).GE.1) THEN
*           absolute day number belonging to 31 december previous year
            LASTY = DATEA(1) - 1
            INDAY = LASTY*365 + LASTY/4 - LASTY/100 + LASTY/400 - 1
         ELSE IF (DATEA(1).EQ.0) THEN
*           set date to TTUTIL offset value
            INDAY = OFFSET
         ELSE
            ERR    = .TRUE.
            MESSAG = 'illegal year'
            RETURN
         END IF

*        count days in month
         IF (DATEA(2).NE.0) THEN
            IF (DATEA(2).GE.1.AND.DATEA(2).LE.12) THEN
               IF (DATEA(2).GE.2) THEN
                  INDAY = INDAY+IMOTB1(DATEA(2))
                  IF (DTLEAP(DATEA(1)).AND.DATEA(2).GT.2)
     &                INDAY = INDAY + 1
               END IF
            ELSE
               ERR    = .TRUE.
               MESSAG = 'month invalid'
               RETURN
            END IF
         END IF

*        count days in day
         IF (DATEA(3).NE.0) THEN
            DAYMAX = IMOTB2(DATEA(2))
            IF (DTLEAP (DATEA(1)).AND.DATEA(2).EQ.2) DAYMAX = DAYMAX+1

            IF (DATEA(3).GE.1.AND.DATEA(3).LE.DAYMAX) THEN
               INDAY = INDAY + DATEA(3)
            ELSE
               ERR    = .TRUE.
               MESSAG = 'day invalid'
               RETURN
            END IF
         END IF

*        subtract TTUTIL offset value and convert
         DTMP = DBLE(INDAY-OFFSET)

         IF (DATEA(4).GE.0) THEN
*           important: hours can be more than 24 !!
            DTMP = DTMP+DBLE (DATEA(4))/24.D0
         ELSE
            ERR    = .TRUE.
            MESSAG = 'hour invalid'
            RETURN
         END IF

         IF (DATEA(5).GE.0.AND.DATEA(5).LE.59) THEN
            DTMP = DTMP+DBLE (DATEA(5))/1440.D0
         ELSE
            ERR    = .TRUE.
            MESSAG = 'minute invalid'
            RETURN
         END IF

         IF (DATEA(6).GE.0.AND.DATEA(6).LE.59) THEN
            DTMP = DTMP+DBLE (DATEA(6))/86400.D0
         ELSE
            ERR    = .TRUE.
            MESSAG = 'seconds invalid'
            RETURN
         END IF

         IF (FSEC.GE.0.AND.FSEC.LT.1.) THEN
            DTMP = DTMP+DBLE (FSEC)/86400.D0
         ELSE
            ERR    = .TRUE.
            MESSAG = 'fractional seconds invalid'
            RETURN
         END IF

         IF (DTMP .GE. DTMAX) THEN
            ERR    = .TRUE.
            MESSAG = 'year above 9999'
            RETURN
         END IF

         DPDTTM = DTMP

      ELSE IF (ITASK.EQ.2) THEN

*        convert from double precision real to date array and
*        fractional seconds

         IF (DPDTTM .GE. DTMAX) THEN
            ERR    = .TRUE.
            MESSAG = 'year above 9999'
            RETURN
         END IF

*        get integer and fractional day
         IF (DPDTTM .GE. 0.0D0) THEN
            INDAY = INT(DPDTTM)
            FRDAY = DPDTTM - DBLE(INDAY)
         ELSE
            INDAY = -INT(ABS(DPDTTM))
            FRDAY = ABS(DPDTTM) - DBLE(ABS(INDAY))
            IF (FRDAY .GT. 0.0D0) THEN
*              there is a fractional day ; get it right
               FRDAY = 1.0D0 - FRDAY
               INDAY = INDAY - 1
            END IF
         END IF

*        get fractional day in seconds
         DSECS = 86400.D0 * FRDAY
         ISECS = INT(DSECS)
         FSECS = DSECS - DBLE(ISECS)

*        if the fractional seconds are close to 1.0 ?
         IF (FSECS .GT. 0.999999D0) THEN
*           regard as 1.0
            FSECS = 0.0D0
            ISECS = ISECS + 1
            IF (ISECS.EQ.86400) THEN
*              regard as a day
               ISECS = 0
               INDAY = INDAY + 1
            END IF
         END IF

*        now we have integer days and integer seconds for the day
*        corrected for a nearly finished second
*        now the date calculation can be done in integer arithmetic

*        alogorithm from FSE 4 calendar routine
         INDAY = INDAY + OFFSET

         IF (INDAY.LT.0) THEN
            ERR    = .TRUE.
            MESSAG = 'date before 1-jan-0001'
            RETURN
         END IF

*        number of 400 year periods and number of days left
         N400 = INDAY / 146097
         LEFT = INDAY - N400 * 146097

*        number of 100 year periods (max 3) and the number of days left
         N100 = MIN (3, LEFT / 36524)
         LEFT = LEFT - N100 * 36524

*        number of 4 year periods and the number of days left
         N4   = LEFT / 1461
         LEFT = LEFT - N4 * 1461

*        number of 1 year periods (max 3) and the number of days left
         N1   = MIN (3, LEFT / 365)
         LEFT = LEFT - N1 * 365

*        current year number is all years passed plus 1
         DATEA(1) = n400*400 + n100*100 + n4*4 + n1 + 1

*        find day number in year (DOY) by subtracting last years
         LASTY = DATEA(1) - 1
         DLAST = LASTY*365 + LASTY/4 - LASTY/100 + LASTY/400 - 1
         INDAY = INDAY - DLAST

*        find out month number
         DATEA(2) = 1
         NDAYS = IMOTB2(DATEA(2))
         IF (DTLEAP (DATEA(1)).AND.DATEA(2).EQ.2) NDAYS = NDAYS+1

20       IF (INDAY.GT.NDAYS) THEN
            INDAY    = INDAY-NDAYS
            DATEA(2) = DATEA(2)+1
            NDAYS    = IMOTB2(DATEA(2))
            IF (DTLEAP (DATEA(1)).AND.DATEA(2).EQ.2) NDAYS = NDAYS+1
         GOTO 20
         END IF

*        day number in month
         DATEA(3) = INDAY

*        hh:mm:ss
         DATEA(4) = MOD(ISECS,86400) / 3600
         DATEA(5) = MOD(ISECS,3600) / 60
         DATEA(6) = MOD(ISECS,60)

*        fractional seconds
         FSEC = REAL (FSECS)

      ELSE
         CALL FATALERR ('DTSYS','unknown task')
      END IF

      RETURN
      END
