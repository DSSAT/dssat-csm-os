C=======================================================================
C  SEIRR, Subroutine
C
C  Determines irrigation sensitivity analysis options
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  05/01/1989 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C  05/18/1999 GH  Fix irrigation efficieny
C  06/09/2002 GH  Modified for Y2K
C-----------------------------------------------------------------------
C  INPUT  : IDETW,ISWWAT,IIRRI,IRMANT,EFFIRR,ISWNIT,AIRAMT,NAPW,
C           IDLAPL,AMT,TOTAPW,NIRR,IRRCOD,DSOIL,THETAC
C
C  LOCAL  : LINE,ERRKEY,MENU,NLOOP,FLAG,EFF,DSOIL,THETAC,AIRAM,TOTAPW
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SENS
C
C  Calls  : ERROR CLEAR SWIRR AUTIRR VERIFY ENTIRR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SEIRR (IDETW,RNMODE,ISWWAT,IIRRI,IRMANT,EFFIRR,ISWNIT,
     &         AIRAMT,NAPW,IDLAPL,AMT,TOTAPW,NIRR,IRRCOD,DSOIL,THETAC,
     &         EFFIRX)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL ERROR, CLEAR, VERIFY, SWIRR, AUTIRR, ENTIRR

      CHARACTER*1  RNMODE,IDETW,ISWWAT,ISWNIT,IIRRI,LINE(80)
      CHARACTER*5  IRRCOD(NAPPL)
      CHARACTER*6  ERRKEY
      CHARACTER*25 IRTYPE(25)
      CHARACTER*30 IRMANT

      INTEGER      MENU,NLOOP,NAPW,NIRR,IDLAPL(NAPPL),IRRTYP(NAPPL),I
      REAL         EFFIRR,FLAG,EFF,DSOIL,THETAC,EFFIR,EFFIRX
      REAL         AIRAM,AIRAMT,AMT(NAPPL),TOTAPW,DSOILT,THETACT

      PARAMETER (ERRKEY = 'SEIRR ')

C     IRTYPE ( 0) = 'No irrigation            '
      IRTYPE ( 1) = 'Furrow, mm               '
      IRTYPE ( 2) = 'Alternating furrows, mm  '
      IRTYPE ( 3) = 'Flood, mm                '
      IRTYPE ( 4) = 'Sprinkler, mm            '
      IRTYPE ( 5) = 'Drip or trickle, mm      '
      IRTYPE ( 6) = 'Flood depth, mm          '
      IRTYPE ( 7) = 'Water table depth, cm    '
      IRTYPE ( 8) = 'Percolation rate,mm day-1'
      IRTYPE ( 9) = 'Bund height, mm          '
      IRTYPE (10) = 'Puddling                 '
      IRTYPE (11) = 'Constant flood depth, mm '

      DO I = 1, NAPPL
	 READ (IRRCOD(I)(4:5),'(I2)') IRRTYP(I)
      END DO

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (INDEX('IE',RNMODE) .GT. 0)  THEN
	 CALL CLEAR
	 DSOILT  = -9.0
	 THETACT = -9.0
	 AIRAM   = -9.0
	 EFFIR   = EFFIRR
	 IF (ISWWAT .NE. 'Y') THEN
	    IRMANT  = 'NO WATER BALANCE SIMULATION   '
	    ISWNIT  = 'N'
	  ELSE IF (IIRRI .EQ. 'A') THEN
	    IRMANT  = 'AUTOMATIC IRRIGAT-REFILL PROF '
	    DSOILT  = DSOIL
	    THETACT = THETAC
	    EFFIR   = EFFIRR
	  ELSE IF (IIRRI .EQ. 'P') THEN
	    IRMANT  = 'FIELD SCHEDULE & AUT REFILL PR'
	    DSOILT  = DSOIL
	    THETACT = THETAC
	    EFFIR   = EFFIRR
	  ELSE IF (IIRRI .EQ. 'F') THEN
	    IRMANT  = 'AUTOMATIC IRRIGAT-FIXED AMOUNT'
	    DSOILT  = DSOIL
	    THETACT = THETAC
	    AIRAM   = AIRAMT
	    EFFIR   = EFFIRR
	  ELSE IF (IIRRI .EQ. 'W') THEN
	    IRMANT  = 'FIELD SCHEDULE & AUT. FIXED AM'
	    DSOILT  = DSOIL
	    THETACT = THETAC
	    AIRAM   = AIRAMT
	    EFFIR   = EFFIRR
	  ELSE IF (IIRRI .EQ. 'N') THEN
	    IRMANT  = 'RAINFED                       '
	  ELSE IF (IIRRI .EQ. 'R') THEN
	    IRMANT  = 'ACCORDING FIELD SCHEDULE-YRDOY'
	    EFFIR   = EFFIRX
	  ELSE IF (IIRRI .EQ. 'D') THEN
	    IRMANT  = 'ACCORDING FIELD SCHEDULE-DAP  '
	    EFFIR   = EFFIRX
	  ELSE
	    IRMANT  = '                              '
	 ENDIF

	 WRITE (*,200) ISWWAT,IRMANT,DSOILT,THETACT,AIRAM,EFFIR,IDETW
	 IF (ISWWAT .NE. 'Y' .AND. ISWWAT .NE. 'A') WRITE (*,280)
	 WRITE (*,290)
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
	  IF (ISWWAT .EQ. 'Y') THEN
	     ISWWAT = 'N'
	   ELSE
	     ISWWAT = 'Y'
	  ENDIF
       ELSE IF (MENU .EQ. 2) THEN
	  CALL SWIRR (RNMODE,IIRRI,ISWWAT,IRMANT)
       ELSE IF (MENU .EQ. 3) THEN
	  CALL AUTIRR (RNMODE,THETAC,DSOIL)
       ELSE IF (MENU .EQ. 4) THEN
	  IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,3500) AIRAMT
	  READ (5,2800) LINE
	  CALL VERIFY (LINE,EFF,FLAG)
	  IF (EFF .GT. 0.0 .AND. FLAG .LE. 0) THEN
	     AIRAMT = EFF
	  ENDIF
       ELSE IF (MENU .EQ. 5) THEN
	  IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,4500) EFFIR
	  READ (5,2800) LINE
	  CALL VERIFY (LINE,EFF,FLAG)
	  IF (EFF .GT. 0.0 .AND. EFF .LE. 1.0 .AND. FLAG .LE. 0) THEN
	     EFFIR = EFF
	  ENDIF
	  IF ((IIRRI .EQ. 'R') .OR. (IIRRI .EQ. 'D')) THEN
	    EFFIRX = EFFIR
	  ELSE
	    EFFIRR = EFFIR
	  ENDIF
       ELSE IF (MENU .EQ. 6) THEN
	  CALL ENTIRR (NIRR,IDLAPL,AMT,RNMODE,TOTAPW,NAPW,IRTYPE,
     &                   IRRTYP,IIRRI)
	  DO I = 1, NAPPL
	     WRITE (IRRCOD(I),'(A2,I3.3)') 'IR',IRRTYP(I)
	  END DO
       ELSE IF (MENU .EQ. 7) THEN
	  IF (IDETW .EQ. 'Y') THEN
	     IDETW = 'N'
	   ELSE
	     IDETW = 'Y'
	  ENDIF
      ENDIF

      GO TO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  200 FORMAT(//,
     &   9X,'WATER AND IRRIGATION MANAGEMENT AND MODIFICATION',
     & /,9X,'================================================',//,
     & 5X,' 0. Return to Main Menu ',//,
     & 5X,' 1. Water Balance Simulation ..........] ',A1,/,
     & 5X,' 2. Irrigation Management .............] ',A30,/,
     & 5X,' 3. Automatic Irrigation Control ......] ',F6.3,2X,F4.1,/,
     & 5X,' 4. Automatic Irrigation-Fixed Amount..] ',F6.3,' mm',/,
     & 5X,' 5. Irrigation Efficiency .............] ',F4.2,/,
     & 5X,' 6. Enter Irrigation Interactive ......| ',/,
     & 5X,' 7. Water Output File .................] ',A1,/)
  280 FORMAT (/, 9X,'To be able to simulate the nitrogen balance,',
     &        /, 9X,'Please turn water balance simulation ON !',/)
  290 FORMAT (//,9X,'SELECTION ? [ Default = 0 ] ===> ',$)
c1000 FORMAT (//,8X,'Please enter :',/,22X,'Y for YES',/,22X,'N for NO',
c    &                               /,22X,'A for ADAPT Hydrology',
c    &        //,8X,'OPTION SELECTED ===> ',1X,A1,
c    &         /,8X,'OVERVIEW FILE ? ---> ',1X,' ',$)
 2800 FORMAT (80A1)
 3500 FORMAT (//,9X,'IRRIGATION AMOUNT     ===>',1X,F8.3,' mm',/,
     &           9X,'NEW AMOUNT ?          --->  ',$)
 4500 FORMAT (//,9X,'IRRIGATION EFFICIENCY ===>',1X,F4.2,/,
     &           9X,'[VALUE BETWEEN 0 and 1]',/,
     &           9X,'NEW VALUE ?           --->  ',$)

      END SUBROUTINE SEIRR

C=======================================================================
C  SWIRR, Subroutine
C
C  Determines irrigation management strategy
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1991 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C-----------------------------------------------------------------------
C  INPUT  : RNMODE,IIRRI,ISWWAT,IRMANT
C
C  LOCAL  : ERRKEY,NLOOP,MENU
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SEIRR
C
C  Calls  : CLEAR ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SWIRR (RNMODE,IIRRI,ISWWAT,IRMANT)

      IMPLICIT     NONE
      EXTERNAL CLEAR, ERROR

      CHARACTER*1  RNMODE,IIRRI,ISWWAT
      CHARACTER*6  ERRKEY
      CHARACTER*30 IRMANT

      INTEGER      NLOOP,MENU

      PARAMETER (ERRKEY = 'SWIRR ')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (INDEX('IE',RNMODE) .GT. 0) THEN
	 CALL CLEAR
	 WRITE (*,3400)
	 WRITE (*, 290) IRMANT
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 100) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 1) THEN
	  IIRRI  = 'N'
	  ISWWAT = 'Y'
      ELSE IF (MENU .EQ. 2) THEN
	  IIRRI  = 'R'
	  ISWWAT = 'Y'
      ELSE IF (MENU .EQ. 3) THEN
	  IIRRI  = 'D'
	  ISWWAT = 'Y'
      ELSE IF (MENU .EQ. 4) THEN
	  IIRRI  = 'A'
	  ISWWAT = 'Y'
      ELSE IF (MENU .EQ. 5) THEN
	  IIRRI  = 'F'
	  ISWWAT = 'Y'
      ELSE IF (MENU .EQ. 6) THEN
	  IIRRI  = 'P'
	  ISWWAT = 'Y'
      ELSE IF (MENU .EQ. 7) THEN
	  IIRRI  = 'W'
	  ISWWAT = 'Y'
      ELSE IF (MENU .EQ. 8) THEN
	  ISWWAT = 'N'
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  290 FORMAT (//,9X,'CURRENT IRRIGATION MANAGEMENT   ===> ',A30,/
     &           9X,'SELECTION (#) ? [ Default = 0 ] ---> ',$)
 3400 FORMAT (9X,'IRRIGATION MANAGEMENT STRATEGY',/,9X,30('='),//,
     & 5X,' 0. Return to Previous Menu ',//
     & 5X,' 1. Rainfed / Not Irrigated ........................|',/,
     & 5X,' 2. Irrigate According To Field Schedule (YRDOY)....|',/,
     & 5X,' 3. Irrigate According To Field Schedule (DAP)......|',/,
     & 5X,' 4. Automatic Irrigation / Refill Profile ..........|',/,
     & 5X,' 5. Automatic Irrigation / Apply Fixed Amount ......|'/,
     & 5X,' 6. Field Schedule & Automatic Refill Profile ......|'/,
     & 5X,' 7. Field Schedule & Automatic Fixed Amount ........|'/,
     & 5X,' 8. No Water Stress / No Water Balance Simulation ..|')

      END SUBROUTINE SWIRR

C=======================================================================
C  AUTIRR, Subroutine
C
C  Determines automatic irrigation options
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1991 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C-----------------------------------------------------------------------
C  INPUT  : RNMODE,STHETA,SDSOIL
C
C  LOCAL  : ERRKEY,NLOOP,MENU,I,D,D1
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SEIRR
C
C  Calls  : CLEAR ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE AUTIRR (RNMODE,THETAC,DSOIL)

      IMPLICIT     NONE
      EXTERNAL CLEAR, ERROR, VERIFY

      CHARACTER*1  RNMODE,LINE(80)
      CHARACTER*6  ERRKEY
      INTEGER      NLOOP,MENU
      REAL         THETAC,DSOIL,EFF,FLAG

      PARAMETER (ERRKEY = 'AUTIRR')

      NLOOP = 0

  100 CONTINUE

      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)
      IF (INDEX('IE',RNMODE) .GT. 0) THEN
	 CALL CLEAR
	 WRITE (*,3900) DSOIL,THETAC
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
	  IF (INDEX('IE',RNMODE).GT. 0) WRITE (*,4200) DSOIL
	  READ (5,4300) LINE
	  CALL VERIFY (LINE,EFF,FLAG)
	  IF (EFF .GT. 0 .AND. FLAG .LE. 0) THEN
	     DSOIL  = EFF
	  ENDIF
      ELSE IF (MENU .EQ. 2) THEN
	  IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,4400) THETAC
	  READ (5,4300) LINE
	  CALL VERIFY (LINE,EFF,FLAG)
	  IF (EFF .GT. 0 .AND. FLAG .LE. 0) THEN
	     THETAC = EFF
	  ENDIF
      ENDIF

      GO TO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

 3900 FORMAT(//,
     & 9X,'AUTOMATIC IRRIGATION BASED ON DEPTH & AVAILABLE SOIL WATER',
     &  /,
     & 9X,'==========================================================',
     & //,
     & 5X,' 0. Return to Main Menu ',//,
     & 5X,' 1. Depth To Check Available Soil Water.] ',F6.2,' cm',/
     & 5X,' 2. Available Soil Water ...............] ',F6.2,'  %',/)
 4050 FORMAT (//,9X,'SELECTION (#) ? [ Default = 0 ] ---> ',$)
 4200 FORMAT (//,9X,'SOIL DEPTH             ===>',1X,F8.3,' cm',/,
     &           9X,'NEW DEPTH  ?           --->  ',$)
 4300 FORMAT (80A1)
 4400 FORMAT (//,9X,'EXTRACTABLE SOIL WATER ===>',1X,F8.3,' %',/,
     &           9X,'NEW PERCENTAGE         --->  ',$)

      END SUBROUTINE AUTIRR

C=======================================================================
C  ENTIRR, Subroutine
C
C  Determines interactive irrigation data entry
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1991 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C  05/07/2020 FO  Added new Y4K subroutine call to convert YRDOY
C-----------------------------------------------------------------------
C  INPUT  : NENTRY,EDAY,EMOUNT,RNMODE,TOTAPW,NAPW,IRTYPE,IRRTYP
C
C  LOCAL  : LINE,ERRKEY,I,MENU,YRTEMP,YRT,DOYT,NLOOP,ED,EFF,FLAG,TOTAPW,EM
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SEIRR
C
C  Calls  : CLEAR ERROR VERIFY YR_DOY SORT
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE ENTIRR (NENTRY,EDAY,EMOUNT,RNMODE,TOTAPW,NAPW,IRTYPE,
     &                   IRRTYP,IIRRI)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL YR_DOY, ERROR, CLEAR, UPCASE, VERIFY, Y4K_DOY, SORT

      CHARACTER*1  RNMODE,LINE(80),IMENU,UPCASE,ANS,IIRRI
      CHARACTER*6  ERRKEY
      CHARACTER*25 IRTYPE(25)

      INTEGER      NENTRY,EDAY(*),I,MENU,YRTEMP,YRT,DOYT,NLOOP
      INTEGER      NAPW,IRRTYP(*),COUNT,EMM(NAPPL)

      REAL         EMOUNT(*),EFF,FLAG,TOTAPW,EM(NAPPL)

      PARAMETER (ERRKEY='ENTIRR')


      DO I = 1, NAPPL
	 EM  = 0.0
	 EMM = 0
      END DO
      NLOOP = 0
  100 CONTINUE
      NLOOP = NLOOP + 1

      IF (INDEX('IE',RNMODE) .GT. 0)  THEN
C	 CALL HOME
	 IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)
	 WRITE (*,200)
	 WRITE (*,250)
	 COUNT = 0
	 IF (NENTRY .GT. 0) THEN
	    DO I = 1, NENTRY
		 CALL YR_DOY (EDAY(I),YRT,DOYT)
	       WRITE (*,285) I,YRT,DOYT,EMOUNT(I),IRTYPE(IRRTYP(I))
	       COUNT = COUNT + 1
	       IF (COUNT .EQ. 15) THEN
		  COUNT = 0
		  WRITE (*,275)
		  WRITE (*,1700)
		  READ  (5,'(A1)') ANS
C		  CALL HOME
		  WRITE (*,200)
		  WRITE (*,250)
	       ENDIF
	    END DO
	 ENDIF
	 WRITE (*,275)
	 WRITE (*,295)
      ENDIF
      IF (INDEX('DRPW',IIRRI) .EQ. 0) THEN
	WRITE(*,296)
      ENDIF
C
C     Get menu choice
C
      READ (5,'(A1)',ERR = 100) IMENU
      IF (INDEX('DRPW',IIRRI) .EQ. 0) THEN
	RETURN
      ENDIF
C
C     Branch to menu choice
C
      IMENU = UPCASE(IMENU)
      IF (IMENU .EQ. ('E')) THEN
	  IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,850)
	  READ (5,'(I5)',ERR = 100) MENU
	  IF (MENU .GT. NENTRY .OR. MENU .LE. 0) GOTO 100
      ELSE IF (IMENU .EQ. ('D')) THEN
	  IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,800)
	  READ (5,450) LINE
	  CALL VERIFY (LINE,EFF,FLAG)
	  IF (EFF .GT. 0. .AND. EFF .LE. NENTRY .AND. FLAG .LE. 0) THEN
	     MENU         = NINT(EFF)
	     EDAY(MENU)   = 99999999
	     EMOUNT(MENU) = 0.0
	     IRRTYP(MENU) = 0
	     GO TO 700
	  ENDIF
      ELSE IF (IMENU .EQ. ('A')) THEN
	  NENTRY = NENTRY + 1
	  MENU   = NENTRY
  380     IF (INDEX('IE',RNMODE) .GT. 0) THEN
	    IF (INDEX('RPW',IIRRI) .GT. 0) THEN
	      WRITE (*,400)
	    ELSE IF (IIRRI .EQ. 'D') THEN
	      WRITE (*,401)
	    ENDIF
	  ENDIF
	  READ (5,450) LINE
	  CALL VERIFY (LINE,EFF,FLAG)
	  IF (EFF .GT. -0.0001 .AND.EFF .LE. 9999999. .AND. FLAG .LE. 0)
     &  THEN
	     YRTEMP = NINT(EFF)
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
	     !CALL Y2K_DOY(YRTEMP)
       CALL Y4K_DOY(YRTEMP,'ENTIRR',1,ERRKEY,1)
	     IF (INDEX('RPW',IIRRI) .GT. 0) THEN
	       CALL YR_DOY (YRTEMP,YRT,DOYT)
	       IF (DOYT .GT. 0 .AND. DOYT .LE. 366 .AND. YRT .LT. 3000)
     &       THEN
	         EDAY(NENTRY) = YRTEMP
	       ELSE
	         WRITE (*,600)
	         GO TO 380
	       ENDIF
	     ELSE IF (IIRRI .EQ. 'D') THEN
	       EDAY(NENTRY) = YRTEMP
	     ENDIF
	   ELSE
	     WRITE (*,600)
	     GO TO 380
	  ENDIF
      ELSE IF (IMENU .EQ. ('Q')) THEN
	  RETURN
      ELSE
	  RETURN
      ENDIF
      !
      ! Allow user to change the date of irrigation application,
	!  if editing
      !
      IF (IMENU .EQ. 'E') THEN
 390     IF (INDEX('IE',RNMODE) .GT. 0) THEN
	    IF (INDEX('RPW',IIRRI) .GT. 0) THEN
	       WRITE (*,410) EDAY(MENU)
	    ELSE IF (IIRRI .EQ. 'D') THEN
	       WRITE (*,411) EDAY(MENU)
	    ENDIF
	 ENDIF
	 READ (5,450) LINE
	 CALL VERIFY (LINE,EFF,FLAG)
	 IF (EFF .GT. -0.0001 .AND. EFF .LE. 9999999. .AND. FLAG .LE. 0)
     &   THEN
	    YRTEMP = NINT(EFF)
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
      !CALL Y2K_DOY(YRTEMP)
	    CALL Y4K_DOY(YRTEMP,'ENTIRR',0,ERRKEY,1)
	   IF (INDEX('RPW',IIRRI) .GT. 0) THEN
	     CALL YR_DOY (YRTEMP,YRT,DOYT)
	     IF (DOYT .GT. 0 .AND. DOYT .LE. 366 .AND.YRT .LT. 3000) THEN
	       EDAY(MENU) = YRTEMP
	     ELSE
		   WRITE (*,600)
		   GO TO 390
	     ENDIF
	   ELSE IF (IIRRI .EQ. 'D') THEN
	     EDAY(MENU) = YRTEMP
	   ENDIF
	 ENDIF
      ENDIF

  550 IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,500) EMOUNT(MENU)
      READ (5,450) LINE
      CALL VERIFY (LINE,EFF,FLAG)
      IF (EFF .GE. 0.0 .AND. EFF .LE. 999.0 .AND. FLAG .LE. 0.0) THEN
	 EMOUNT(MENU) = EFF
       ELSE
	 WRITE (*,600)
	 GO TO 550
      ENDIF

      IF (INDEX('IE',RNMODE) .GT. 0) CALL CLEAR
 5000 IF (INDEX('IE',RNMODE) .GT. 0) THEN
	 WRITE (*,900) (I,IRTYPE(I),I=1,10),IRRTYP(MENU)
      ENDIF

      READ (5,450) LINE
      CALL VERIFY (LINE,EFF,FLAG)
      IF (EFF .GT. 0.0 .AND. EFF .LE. 17.0 .AND. FLAG .LE. 0) THEN
	 IRRTYP(MENU) = NINT(EFF)
       ELSE IF (FLAG .EQ. 1.0 .AND. IRRTYP(MENU) .GT. 0) THEN
	 IRRTYP(MENU) = IRRTYP(MENU)
       ELSE
	 WRITE (*,600)
	 GO TO 5000
      ENDIF

 700  CONTINUE

      CALL SORT (NENTRY,EDAY,EMOUNT,EM,IRRTYP,EMM)

      IF (EDAY(NENTRY) .EQ. 9999999) THEN
	 EDAY(NENTRY) = 0
	 NENTRY       = NENTRY - 1
      ENDIF

      TOTAPW = 0.0
      NAPW   = 0

      DO I = 1, NENTRY
	 IF (IRRTYP(I) .LE. 5) THEN
	    TOTAPW = TOTAPW + EMOUNT(I)
	    NAPW   = NAPW + 1
	 ENDIF
      END DO

      GO TO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  200 FORMAT(/,
     &    10X,'INTERACTIVE DATA ENTRY FOR IRRIGATION',/,
     &    10X,'=====================================')
  250 FORMAT (
     & 9X,'|---|---------|-------------|--------------------------|',/,
     & 9X,'| # | Date    | Amount (mm) |    Irrigation Type       |',/,
     & 9X,'|---|---------|-------------|--------------------------|')
  275 FORMAT (
     & 9X,'|---|---------|-------------|--------------------------|')
  285 FORMAT (
     & 9X,'│',I2,1X,'│',I4,1X,I3,1X,'│',F9.1,4X,'│ ',A25,'│')
  295 FORMAT (/9X,
     &     '(E)dit, (A)dd an event, (D)elete, (Q)uit (Enter = Done)',$)
  296 FORMAT (//,9X,
     &    'This option is not available for the current irrigation',/,
     & 9X,'management selection.  Please change selection first.', $)
  400 FORMAT (/, 9X,'Irrigation application date   [YRDOY]',/,
     &           9X,'New application date ?   --->',1X,' ',$)
  401 FORMAT (/, 9X,'Irrigation application day [DAP]',/,
     &           9X,'New application date ?   --->',1X,' ',$)
  410 FORMAT (/,9X,'Irrigation application date [YRDOY] ===>',2X,I7,
     &        /,9X,'New application date ?              --->',1X,' ',$)
  411 FORMAT (/,9X,'Irrigation application day [DAP]    ===>',2X,I5,
     &        /,9X,'New application date ?              --->',1X,' ',$)
  450 FORMAT (80A1)
  500 FORMAT (/, 9X,'Irrigation amount           ===>',1X,F10.3,' mm',
     &        /, 9X,'New irrigation amount ?     --->',3X,' ',$)
  600 FORMAT (/, 9X,'ERROR! Values are out of range',/)
  800 FORMAT (/, 9X,'Please enter entry number to delete ',$)
  850 FORMAT (/, 9X,'Please enter entry number to edit   ',$)
  900 FORMAT (//,9X,'|---|---------------------------|',/,
     &           9X,'| # |   Irrigation Type         |',/,
     &           9X,'|---|---------------------------|',/,
     &           10(9X,'|',I2,' |',1X,A25,' |',/),
     &           9X,'|---|---------------------------|',//,
     &           9X,'Irrigation type selected ===>',1X,I3,/,
     &           9X,'New type ?               --->  ',$)
 1700 FORMAT (/,15X,'There are more entries .. Press any key ..')

      END SUBROUTINE ENTIRR

C=======================================================================
C  SORT, Subroutine
C
C  Sorts variables
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : N,ARR,BRR,CRR,DRR
C
C  LOCAL  : I,J,A,D,B,C
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : ENTIRR ENTFERT
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SORT (N,ARR,BRR,CRR,DRR,ERR)

      IMPLICIT NONE

      INTEGER  ARR(*),I,J,N,AA,DRR(*),DD,ERR(*),EE
      REAL     BRR(*),BB,CRR(*),CC

      DO J = 2, N
	 AA = ARR(J)
	 BB = BRR(J)
	 CC = CRR(J)
	 DD = DRR(J)
	 EE = ERR(J)
	 DO I = J-1, 1, -1
	    IF (ARR(I) .LE. AA) GO TO 10
	    ARR(I+1) = ARR(I)
	    BRR(I+1) = BRR(I)
	    CRR(I+1) = CRR(I)
	    DRR(I+1) = DRR(I)
	    ERR(I+1) = ERR(I)
	 END DO
	 I = 0
   10    CONTINUE
	 ARR(I+1) = AA
	 BRR(I+1) = BB
	 CRR(I+1) = CC
	 DRR(I+1) = DD
	 ERR(I+1) = EE
      END DO

      RETURN
      END SUBROUTINE SORT
