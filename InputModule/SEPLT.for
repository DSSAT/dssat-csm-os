C=======================================================================
C  SEPLT, Subroutine
C
C  Planting timing and control sensitivity analysis
C-----------------------------------------------------------------------
C  Revision history
C
C  06/01/1992 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C  07/12/1996 GH  Added tw new planting methods
C  06/09/2002 GH  Modified for Y2K
C-----------------------------------------------------------------------
C  INPUT  : PLTPOP,ROWSPC,AZIR,BETN,SDEPTH,SDWTPL,YRSIM,YRPLT,IPLT,
C           IPLTI,YEAR,PWDINF,PWDINL,SWPLTL,SWPLTH,SWPLTD,PTX,PTTN
C
C  LOCAL  : LINE,MSPLT,ERRKEY,PLTMAN,NLOOP,MENU,IDUMM,DSPLT,JULIAN,IPYRP,
C           FLAG,EFF,SWPLTL,SWPLTH,SWPLTD,PTX,PTTN
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SENS
C
C  Calls  : YR_DOY ERROR NAILUJ CLEAR VERIFY SWPLT
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SEPLT (RNMODE,PLTPOP,ROWSPC,AZIR,SDEPTH,SDWTPL,YRSIM,
     &           YRPLT,IPLT,IPLTI,YEAR,PWDINF,PWDINL,SWPLTL,SWPLTH,
     &           SWPLTD,PTX,PTTN,PLME,SDAGE,ATEMP)

      IMPLICIT     NONE
      EXTERNAL CLEAR, ERROR, JULIAN, MNPLT, NAILUJ, SELPLT, SWPLT, 
     &  VERIFY, YDOY, YR_DOY

      CHARACTER*1  RNMODE,LINE(80),IPLTI,PLME
      CHARACTER*3  MSPLT
      CHARACTER*6  ERRKEY
      CHARACTER*16 PLTMAN,MTHMAN

      INTEGER      NLOOP,MENU,IDUMM,YEAR,YDOY
      INTEGER      DSPLT,JULIAN,YRSIM,YRPLT,IPLT,IPYRP,PWDINF,PWDINL
      REAL         FLAG,EFF,PLTPOP,ROWSPC,AZIR,SDEPTH,SDWTPL,SDAGE
      REAL         SWPLTL,SWPLTH,SWPLTD,PTX,PTTN,ATEMP

      PARAMETER (ERRKEY = 'SEPLT ')

      NLOOP  = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      CALL YR_DOY (YRPLT,IPYRP,IPLT)
      CALL NAILUJ (IPLT,IPYRP,MSPLT,DSPLT)

      IF (INDEX('IE',RNMODE) .GT. 0) THEN
         IF (IPLTI .EQ. 'A') THEN
            PLTMAN = 'AUTOMATIC       '
          ELSE IF (IPLTI .EQ. 'R') THEN
            PLTMAN = 'ON REPORTED DATE'
         ENDIF

         SELECT CASE (PLME)
           CASE('T'); MTHMAN = 'TRANSPLANT      '
           CASE('S'); MTHMAN = 'SEED            '
           CASE('P'); MTHMAN = 'PREGERMIN. SEED '
           CASE('N'); MTHMAN = 'NURSERY         '
           CASE('R'); MTHMAN = 'RATOON          '
           CASE('C'); MTHMAN = 'CUTTING         '
           CASE('H'); MTHMAN = 'HORIZONTAL STICK' !cassava
           CASE('V'); MTHMAN = 'VERTICAL STICK  ' !cassava
           CASE('I'); MTHMAN = 'INCLINED STICK  ' !cassava
           CASE('B'); MTHMAN = 'BEDDED ROWS     ' !Used in potato model
         END SELECT

         CALL CLEAR
         WRITE (*,200) MSPLT,DSPLT,IPYRP,PLTPOP,ROWSPC,AZIR,SDEPTH,
     &                 SDWTPL,MTHMAN,PLTMAN,SDAGE,ATEMP
         IF (YRPLT .LT. YRSIM) WRITE (*,300)
         WRITE (*,400)
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 100) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 0) THEN
          RETURN
      ELSE IF (MENU .EQ. 1) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) THEN
		  WRITE (*,600) MSPLT,DSPLT
          ENDIF
		READ (5,650,ERR = 100) MSPLT,DSPLT
          IF (DSPLT .GT. 0 .AND. DSPLT .LE. 31) THEN
             IDUMM = JULIAN (DSPLT,MSPLT,IPYRP)
          ENDIF
          IF (IDUMM .GE. 1 .AND. IDUMM .LE. 366) THEN
             IPLT  = IDUMM
             YRPLT = IPYRP*1000 + IPLT
          ENDIF
          IF (YRSIM .GT. YRPLT) YRSIM = YRPLT
      ELSE IF (MENU .EQ. 2) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) THEN
	      WRITE (*,700) IPYRP
	    ENDIF
          READ (5,800) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 3000.0 .AND. FLAG .LE. 0) THEN
             IPYRP = NINT(EFF)
          ENDIF
	    IF (IPYRP.LT. 100) THEN
	      IF (IPYRP .LE. 10) THEN
              IPYRP = 2000 + IPYRP
	      ELSE
	        IPYRP = 1900 + IPYRP
            ENDIF
	    ENDIF
          YRPLT  = YDOY(IPYRP,IPLT)
      ELSE IF (MENU .EQ. 3) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,900) PLTPOP
          READ (5,800) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 1000.0 .AND. FLAG .LE. 0) THEN
             PLTPOP = EFF
          ENDIF
      ELSE IF (MENU .EQ. 4) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,1000) ROWSPC
          READ (5,800) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 1000.0 .AND. FLAG .LE. 0) THEN
             ROWSPC = EFF
          ENDIF
      ELSE IF (MENU .EQ. 5) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,1100) AZIR
          READ (5,800) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 90.0 .AND. FLAG .LE. 0) THEN
             AZIR = EFF
          ENDIF
      ELSE IF (MENU .EQ. 6) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,1200) SDEPTH
          READ (5,800) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 150.0 .AND. FLAG .LE. 0) THEN
              SDEPTH = EFF
          ENDIF
      ELSE IF (MENU .EQ. 7) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,1300) SDWTPL
          READ (5,800) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 10000. .AND. FLAG .LE. 0) THEN
              SDWTPL = EFF
          ENDIF
      ELSE IF (MENU .EQ. 8) THEN
          CALL MNPLT (RNMODE,PLME,MTHMAN)
      ELSE IF (MENU .EQ. 9) THEN
          CALL SWPLT (RNMODE,IPLTI,PLTMAN)
      ELSE IF (MENU .EQ. 10) THEN
          CALL SELPLT (PWDINF,PWDINL,SWPLTL,SWPLTH,SWPLTD,PTX,PTTN,
     &                 RNMODE,YEAR,YRSIM,YRPLT)
      ELSE IF (MENU .EQ. 11) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,1400) SDAGE
          READ (5,800) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GE. 0.0 .AND. EFF .LE. 10000. .AND. FLAG .LE. 0) THEN
              SDAGE = EFF
          ENDIF
      ELSE IF (MENU .EQ. 12) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,1500) ATEMP
          READ (5,800) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GE. 10.0 .AND. EFF .LE. 50. .AND. FLAG .LE. 0) THEN
              ATEMP = EFF
          ENDIF
      ENDIF

      GO TO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  200 FORMAT (9X,'PLANTING TIMING AND CONTROL',/,
     &        9X,'===========================',//,
     & 5X,' 0. Return to Main Menu ',//
     & 5X,' 1. Planting Date ......................]',6X,A3,1X,I2,/,
     & 5X,' 2. Year of Planting ...................]',8X,I4,/,
     & 5X,' 3. Plant Population ...................]',3X,F9.3,/,
     & 5X,' 4. Row Spacing ........................]',3X,F9.3,/,
     & 5X,' 5. Row Direction ......................]',3X,F9.3,/,
     & 5X,' 6. Planting Depth......................]',3X,F9.3,/,
     & 5X,' 7. Planting Material Dry Weight .......]',3X,F9.3,/,
     & 5X,' 8. Planting Method ....................]',3X,A16,/,
     & 5X,' 9. Planting Management ................]',3X,A16,/,
     & 5X,'10. Automatic Planting Options .........|',/,
     & 5X,'11. Transplant age......................]',3X,F8.0,/,
     & 5X,'12. Nursery Average Temperature ........]',3X,F8.1,/)
  300 FORMAT (/,
     &        9X,'Planting date is before start of simulation.',/,
     &        9X,'Modify start of simulation date or be terminated!')
  400 FORMAT (//,9X,'SELECTION (#) ? [ Default = 0 ] ---> ',$)
  600 FORMAT (9X,'SELECTED PLANTING DATE       ===> ',A3,' ',I2,
     &      /,9X,'NEW DATE ? (Ex. JUN 15)      ---> ',$)
  650 FORMAT (A3,1X,I3)
  700 FORMAT (//,9X,'CURRENT PLANTING YEAR     ===>',1X,I4,/,
     &           9X,'NEW PLANTING YEAR ?       ---> ',$)
  800 FORMAT (80A1)
  900 FORMAT (//,9X,'CURRENT PLANT POPULATION  ===>',1X,F9.3,' #/m2',
     &         /,9X,'NEW POPULATION ?          --->',3X,' ',$)
 1000 FORMAT (//,9X,'CURRENT ROW SPACING       ===>',1X,F9.3,
     & ' cm  ',/,9X,'NEW ROW SPACING ?         --->',3X,' ',$)
 1100 FORMAT (//,9X,'CURRENT ROW DIRECTION     ===>',1X,F9.3,
     &              ' degrees from North',/,
     &           9X,'NEW ROW DIRECTION ?       --->',3X,' ',$)
 1200 FORMAT (//,9X,'CURRENT PLANTING DEPTH    ===>',1X,F9.3,' cm  ',
     &         /,9X,'NEW PLANTING DEPTH ?      --->',3X,' ',$)
 1300 FORMAT (//,9X,'CURRENT PLANTING MATERIAL WEIGHT ===>',1X,F9.3,
     &' kg/ha',/,9X,'NEW PLANTING DRY WEIGHT ?        --->',3X,' ',$)
 1400 FORMAT (//,9X,'TRANSPLANT AGE (DAYS)     ===>',1X,F5.0,
     &'      ',/,9X,'NEW TRANSPLANT AGE ?      --->',2X,' ',$)
 1500 FORMAT (//,9X,'NURSERY AVERAGE TEMPERATURE (oC)  ===>',1X,F6.1,
     &'      ',/,9X,'NEW AVERAGE TEMPERATURE (oC)      --->',2X,' ',$)

      END SUBROUTINE SEPLT

C=======================================================================
C  SWPLT, Subroutine
C
C  Determines planting management strategy in sensitivity analysis
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : RNMODE,IPLTI,PLTMAN
C
C  LOCAL  : ERRKEY,NLOOP,MENU
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SEPLT
C
C  Calls  : CLEAR ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SWPLT (RNMODE,IPLTI,PLTMAN)

      IMPLICIT     NONE
      EXTERNAL CLEAR, ERROR

      CHARACTER*1  RNMODE,IPLTI
      CHARACTER*6  ERRKEY
      CHARACTER*16 PLTMAN

      INTEGER      NLOOP,MENU

      PARAMETER (ERRKEY = 'SWPLT ')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (INDEX('IE',RNMODE) .GT. 0) THEN
         CALL CLEAR
         WRITE (*,3400)
         WRITE (*, 290) PLTMAN
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 100) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 1) THEN
          IPLTI  = 'R'
      ELSE IF (MENU .EQ. 2) THEN
          IPLTI  = 'A'
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

 290  FORMAT (//,9X,'CURRENT PLANTING MANAGEMENT     ===> ',A16,/
     &           9X,'SELECTION (#) ? [ Default = 0 ] ---> ',$)
 3400 FORMAT (9X,'PLANTING MANAGEMENT STRATEGY',/,9X,36('='),//,
     & 5X,' 0. Return to Previous Menu ',//
     & 5X,' 1. On Reported Date of Experiment .................|',/,
     & 5X,' 2. Automatic When Conditions are Satisfactory .....|')

      END SUBROUTINE SWPLT

C=======================================================================
C  MNPLT, Subroutine
C
C  Determines planting method strategy in sensitivity analysis
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                       G.H.        1-11-94
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : RNMODE,PLME,MTHMAN
C
C  LOCAL  : ERRKEY,NLOOP,MENU
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SEPLT
C
C  Calls  : CLEAR ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE MNPLT (RNMODE,PLME,MTHMAN)

      IMPLICIT     NONE
      EXTERNAL CLEAR, ERROR

      CHARACTER*1  RNMODE,PLME
      CHARACTER*6  ERRKEY
      CHARACTER*16 MTHMAN

      INTEGER      NLOOP,MENU

      PARAMETER (ERRKEY = 'MNPLT ')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (INDEX('IE',RNMODE) .GT. 0) THEN  
	   CALL CLEAR
         WRITE (*,3400)
         WRITE (*, 290) MTHMAN
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 100) MENU
C
C     Branch to menu choice
C
      SELECT CASE (MENU)
        CASE (1); PLME  = 'T'
        CASE (2); PLME  = 'S'
        CASE (3); PLME  = 'P'
        CASE (4); PLME  = 'N'
        CASE (5); PLME  = 'R'
        CASE (6); PLME  = 'C'
        CASE (7); PLME  = 'H'
        CASE (8); PLME  = 'V'
        CASE (9); PLME  = 'I'
        CASE(10); PLME  = 'B'
      END SELECT

      RETURN
C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

 290  FORMAT (//,9X,'CURRENT PLANTING METHOD         ===> ',A16,/
     &           9X,'SELECTION (#) ? [ Default = 0 ] ---> ',$)
 3400 FORMAT (9X,'PLANTING METHOD STRATEGY    ',/,9X,36('='),//,
     & 5X,' 0. Return to Previous Menu ',//
     & 5X,' 1. Transplant .....................................|',/,
     & 5X,' 2. Seed ...........................................|',/,
     & 5X,' 3. Pregerminated Seed .............................|',/,
     & 5X,' 4. Nursery ........................................|',/,
     & 5X,' 5. Ratoon .........................................|',/,
     & 5X,' 6. Cutting ........................................|',/,
     & 5X,' 7. Horizontally planted cutting....................|',/,
     & 5X,' 8. Inclined cutting................................|',/,
     & 5X,' 9. Vertically planted cutting......................|',/,
     & 5X,'10. Bedded rows.....................................|')


      END SUBROUTINE MNPLT

C=======================================================================
C  SELPLT, Subroutine
C
C  Determines automatic planting management
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : PWDINF,PWDINL,SWPLTL,SWPLTH,SWPLTD,PTX,PTTN,YEAR,YRSIM
C
C  LOCAL  : LINE,MFPLT,MLPLT,ERRKEY,NLOOP,MENU,IFYRP,IFPLT,DFPLT,IDUMM,
C           JULIAN,ILYRP,ILPLT,DLPLT,FLAG,EFF
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SEPLT
C
C  Calls  : ERROR NAILUJ CLEAR VERIFY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SELPLT (PWDINF,PWDINL,SWPLTL,SWPLTH,SWPLTD,PTX,PTTN,
     &                   RNMODE,YEAR,YRSIM,YRPLT)

      IMPLICIT    NONE
      EXTERNAL CLEAR, ERROR, INCYD, JULIAN, NAILUJ, VERIFY, YDOY

      CHARACTER*1 RNMODE,LINE(80)
      CHARACTER*3 MFPLT,MLPLT
      CHARACTER*6 ERRKEY

      INTEGER     NLOOP,MENU,PWDINF,PWDINL,YEAR,IFYRP,IFPLT,DFPLT
      INTEGER     IDUMM,JULIAN,ILYRP,ILPLT,DLPLT,YRSIM,YRPLT,INCYD,YDOY

      REAL        FLAG,EFF,SWPLTL,SWPLTH,SWPLTD,PTX,PTTN

      PARAMETER (ERRKEY = 'SELPLT')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

C
C    Determine first date of planting window
C
      IF (PWDINF .EQ. 155 .OR. PWDINF .LE. 0) THEN
         PWDINF = INCYD(YRPLT,-7)
      ENDIF
      IF (PWDINF .GT. 1000) THEN
         IFYRP = INT (PWDINF/1000.0)
         IFPLT = PWDINF - IFYRP * 1000
       ELSE
         IFPLT = PWDINF
         IFYRP = YEAR
      ENDIF
      CALL NAILUJ (IFPLT,IFYRP,MFPLT,DFPLT)

C
C    Determine last date of planting window
C
      IF (PWDINL .EQ. 155 .OR. PWDINL .LE. 0) THEN
         PWDINL = INCYD(YRPLT,-21)
      ENDIF
      IF (PWDINL .GT. 1000) THEN
         ILYRP = INT (PWDINL/1000.0)
         ILPLT = PWDINL - ILYRP * 1000
       ELSE
         ILPLT = PWDINL
         ILYRP = YEAR
      ENDIF
      CALL NAILUJ (ILPLT,ILYRP,MLPLT,DLPLT)

      IF (INDEX('IE',RNMODE) .GT. 0) THEN
         CALL CLEAR
         WRITE (*,3900) MFPLT,DFPLT,IFYRP,MLPLT,DLPLT,ILYRP,
     &                  SWPLTL,SWPLTH,SWPLTD,PTX,PTTN
         IF (PWDINF .LT. YRSIM) WRITE (*,300)
         WRITE (*,4050)
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 100) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 0) THEN
          RETURN
      ELSE IF (MENU .EQ. 1) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) THEN
		  WRITE (*,4500) MFPLT,DFPLT
	    ENDIF
          READ (5,4550,ERR = 100) MFPLT,DFPLT
          IF (DFPLT .GT. 0 .AND. DFPLT .LE. 31) THEN
             IDUMM = JULIAN (DFPLT,MFPLT,IFYRP)
          ENDIF
          IF (IDUMM .GE. 1 .AND. IDUMM .LE. 366) IFPLT = IDUMM
          IF (INDEX('IE',RNMODE) .GT. 0) THEN
		  WRITE (*,4575) IFYRP
	    ENDIF
          READ (5,5600) LINE
          CALL VERIFY (LINE,EFF,FLAG)

          IF (EFF .GT. 0.0 .AND. EFF .LE. 3000. .AND. FLAG .LE. 0) THEN
             IFYRP = NINT(EFF)
          ENDIF

          PWDINF = YDOY(IFYRP,IFPLT)
          IF (YRSIM .GT. PWDINF) YRSIM = PWDINF
      ELSE IF (MENU .EQ. 2) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) THEN
            WRITE (*,4650) MLPLT,DLPLT
          ENDIF
          READ (5,4550,ERR = 100) MLPLT,DLPLT
          IF (DLPLT .GT. 0 .AND. DLPLT .LE. 31) THEN
             IDUMM = JULIAN (DLPLT,MLPLT,ILYRP)
          ENDIF
          IF (IDUMM .GE. 1 .AND. IDUMM .LE. 366) ILPLT = IDUMM
          IF (INDEX('IE',RNMODE) .GT. 0) THEN
		  WRITE (*,4700) ILYRP
          ENDIF
		READ (5,5600) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 3000.0 .AND. FLAG .LE. 0) THEN
             ILYRP = NINT(EFF)
          ENDIF
          PWDINL = YDOY(ILYRP,ILPLT)
      ELSE IF (MENU .EQ. 3) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) THEN
		  WRITE (*,4800) SWPLTL
          ENDIF
		READ (5,5600) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 100.0 .AND. FLAG .LE. 0) THEN
              SWPLTL = EFF
          ENDIF
      ELSE IF (MENU .EQ. 4) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) THEN
		  WRITE (*,4900) SWPLTH
          ENDIF
		READ (5,5600) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 100.0 .AND. FLAG .LE. 0) THEN
              SWPLTH = EFF
          ENDIF
      ELSE IF (MENU .EQ. 5) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) THEN
		  WRITE (*,5000) SWPLTD
          ENDIF
		READ (5,5600) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GE. 0.0 .AND. EFF .LT. 200.0 .AND. FLAG .LE. 0) THEN
              SWPLTD = EFF
          ENDIF
      ELSE IF (MENU .EQ. 6) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) THEN
		  WRITE (*,5100) PTX
          ENDIF
		READ (5,5600) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GE. -5.0 .AND. EFF .LT. 50.0 .AND. FLAG .LE. 0) THEN
              PTX = EFF
          ENDIF
      ELSE IF (MENU .EQ. 7) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) THEN
		  WRITE (*,5200) PTTN
          ENDIF
		READ (5,5600) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GE. -5.0 .AND. EFF .LT. 50.0 .AND. FLAG .LE. 0) THEN
              PTTN = EFF
          ENDIF
      ENDIF

      GO TO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  300 FORMAT (/,9X,'START OF SIMULATION DATE IS AFTER THE START OF',
     &        /,9X,'THE AUTOMATIC PLANTING WINDOW.',/,
     &          9X,'PLEASE MODIFY PLANTING DATE OR SIMULATION WILL ',
     &             'TERMINATE !')
 3900 FORMAT (9X,'AUTOMATIC PLANTING MANAGEMENT',/,
     &        9X,'=============================',//,
     & 6X,'0. Return to Previous Menu ',//
     & 6X,'1. Earliest Planting Date ......................] ',
     &        A3,1X,I2,1X,I4,/,
     & 6X,'2. Latest Planting Date ........................] ',
     &        A3,1X,I2,1X,I4,/,
     & 6X,'3. Lowermost Soil Water Content .............(%)]',3X,F5.0,/,
     & 6X,'4. Uppermost Soil Water Content .............(%)]',3X,F5.0,/,
     & 6X,'5. Management Depth For Soil Water ..........cm)]',3X,F5.0,/,
     & 6X,'6. Maximum Soil Temperature (Top 10 cm Avg).(øC)]',3X,F5.0,/,
     & 6X,'7. Minimum Soil Temperature (Top 10 cm Avg).(øC)]',3X,F5.0,/)
 4050 FORMAT (//,9X,'SELECTION (#) ? [ Default = 0 ] ---> ',$)
 4500 FORMAT (9X,'EARLIEST PLANTING DATE           ===> ',A3,' ',I2,
     &      /,9X,'NEW DATE ? (Ex. JUN 15)          ---> ',$)
 4550 FORMAT (A3,1X,I3)
 4575 FORMAT (/,9X,'CURRENT EARLIEST PLANTING YEAR ===>',1X,I4,
     &        /,9X,'NEW PLANTING YEAR ?            ---> ',$)
 4650 FORMAT (   9X,'LATEST PLANTING DATE          ===> ',A3,' ',I2,
     &         /,9X,'NEW DATE ? (Ex. JUN 15)       ---> ',$)
 4700 FORMAT (/, 9X,'CURRENT LATEST PLANTING YEAR  ===>',1X,I4,
     &        /, 9X,'NEW PLANTING YEAR ?           ---> ',$)
 4800 FORMAT (//,9X,'LOWERMOST SOIL WATER CONTENT  ===>',1X,F5.0,
     &    ' %',/,9X,'NEW SOIL WATER CONTENT ?      --->  ',$)
 4900 FORMAT (//,9X,'UPPERMOST SOIL WATER CONTENT  ===>',1X,F5.0,
     &    ' %',/,9X,'NEW SOIL WATER ?              --->  ',$)
 5000 FORMAT (//,9X,'WATER MANAGEMENT DEPTH        ===>',1X,F5.0,
     &   ' cm',/,9X,'NEW DEPTH ?                   --->  ',$)
 5100 FORMAT (//,9X,'MAXIMUM SOIL TEMPERATURE      ===>',1X,F5.0,
     &   ' oC',/,9X,'NEW SOIL TEMPERATURE ?        --->   ',$)
 5200 FORMAT (//,9X,'MINIMUM SOIL TEMPERATURE      ===>',1X,F5.0,
     &   ' oC',/,9X,'NEW SOIL TEMPERATURE ?        --->   ',$)
 5600 FORMAT (80A1)

      END SUBROUTINE SELPLT
