      SUBROUTINE TIMER2 (ITASK, STTIME, DELT, PRDEL , FINTIM, IYEAR,
     &                   TIME , DOY   , IDOY, TERMNL, OUTPUT)
 
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ITASK, IYEAR, IDOY
      REAL STTIME, DELT, PRDEL, FINTIM, TIME, DOY
      LOGICAL TERMNL, OUTPUT
 
**    local variables
      INTEGER I, ITOLD, ICOUNT, IT, IS, IFINT, IPRINT, ILDOY, ILYEAR
      REAL LDELT, LTIME, LSTTIM, TINY, R1, DIFF
      PARAMETER (TINY=5.E-5)
      LOGICAL PROUT
      SAVE
 
      DATA ITOLD /4/
 
      IF (ITASK.EQ.1) THEN
 
*        test absolute values
 
         IF (IYEAR.LT.1000.OR.IYEAR.GT.2100)
     &      CALL FATALERR ('TIMER2', 'IYEAR < 1000 or IYEAR > 2100')
 
         IF (MOD (IYEAR,4).NE.0) THEN
            IF (STTIME.LT.(1.-TINY).OR.STTIME.GT.(365.+TINY))
     &         CALL FATALERR ('TIMER2', 'STTIME < 1 or > 365')
         ELSE
            IF (STTIME.LT.(1.-TINY).OR.STTIME.GT.(366.+TINY))
     &         CALL FATALERR ('TIMER2', 'STTIME < 1 or > 366')
         END IF
 
         IF (FINTIM.LT.STTIME) CALL FATALERR
     &      ('TIMER2', 'FINTIM < STTIME')
 
         IF (DELT.LE.0.) THEN
            CALL FATALERR ('TIMER2', 'DELT <= 0')
         ELSE IF (DELT.GT.0..AND.DELT.LT.1.) THEN
            R1 = 1./DELT
            IT = NINT (1./DELT)
            IS = 1
         ELSE IF (DELT.EQ.1.) THEN
            R1 = 1.
            IT = 1
            IS = 1
         ELSE IF (DELT.GT.1.) THEN
            R1 = DELT
            IT = 1
            IS = NINT (DELT)
         END IF
 
         IF (PRDEL.LT.0.) THEN
            CALL FATALERR ('TIMER2', 'PRDEL <= 0')
         ELSE IF (PRDEL.EQ.0.) THEN
*           suppress output when prdel = 0
            PROUT = .FALSE.
         ELSE
            PROUT = .TRUE.
         END IF
 
*        check multiples
         IF (ABS (R1-NINT (R1)).GT.TINY) CALL FATALERR
     &      ('TIMER2', 'DELT incorrect')
         IF (PROUT.AND.ABS (PRDEL-DELT*NINT (PRDEL/DELT)).GT.TINY)
     &      CALL FATALERR ('TIMER2', 'PRDEL not a multiple of DELT')
         IF (ABS (STTIME-NINT (STTIME)).GT.TINY) CALL FATALERR
     &      ('TIMER2', 'STTIME not an integer value')
         IF (PROUT) IPRINT = NINT (PRDEL/DELT)
         IFINT  = NINT ((FINTIM-STTIME)/DELT)
         DIFF   = (FINTIM-(STTIME+REAL (IFINT)*DELT))/DELT
         IF (DIFF.GT.0.01) IFINT = IFINT+1
 
         ICOUNT = 0
 
*        assign to local variables
 
         LTIME  = STTIME
         LSTTIM = STTIME
         LDELT  = DELT
         ILDOY  = NINT (STTIME)
         ILYEAR = IYEAR
 
*        global variables
 
         TIME   = STTIME
         IDOY   = ILDOY
         DOY    = REAL (ILDOY)
         TERMNL = .FALSE.
         IF (PROUT) THEN
            OUTPUT = .TRUE.
         ELSE
            OUTPUT = .FALSE.
         END IF
 
      ELSE IF (ITASK.EQ.2) THEN
 
         IF (ITOLD.EQ.4) CALL FATALERR
     &      ('TIMER2','initialization required')
 
         IF (TIME.NE.LTIME) CALL FATALERR
     &      ('TIMER2', 'TIME was changed illegally')
         IF (IDOY.NE.ILDOY) CALL FATALERR
     &      ('TIMER2', 'IDOY was changed illegally')
 
         IF (ICOUNT.LT.IFINT.AND..NOT.TERMNL) THEN
 
            ICOUNT = ICOUNT+1
            LTIME  = LSTTIM+REAL (ICOUNT)*LDELT
 
            IF (MOD (ICOUNT, IT).EQ.0) THEN
               DO 10 I=1,IS
                  ILDOY = ILDOY+1
                  IF (ILDOY.EQ.366) THEN
                     IF (ILYEAR.LT.1500) THEN
                        ILDOY  = 1
                     ELSE
                        IF (MOD (ILYEAR,4).NE.0) THEN
                           ILDOY = 1
                           ILYEAR = ILYEAR+1
                        END IF
                     END IF
                  ELSE IF (ILDOY.EQ.367) THEN
                     ILYEAR = ILYEAR+1
                     ILDOY  = 1
                  END IF
10             CONTINUE
               DOY   = REAL (ILDOY)
            ELSE
               DOY   = REAL (ILDOY)+MOD (LTIME, 1.)
            END IF
 
            OUTPUT = .FALSE.
            IF (PROUT) THEN
               IF (MOD(ICOUNT,IPRINT).EQ.0.OR.ICOUNT.GE.IFINT)
     &              OUTPUT = .TRUE.
            END IF
 
            TIME  = LTIME
            IDOY  = ILDOY
            IYEAR = ILYEAR
         ELSE
            TERMNL = .TRUE.
            IF (PROUT) OUTPUT = .TRUE.
         END IF
 
      ELSE
         CALL FATALERR ('TIMER2','wrong ITASK')
      END IF
 
      ITOLD = ITASK
 
      RETURN
      END
