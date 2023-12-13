C=======================================================================
C  SEHARV, Subroutine
C
C  Determines harvest timing and control
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1990 GH  Written
C  05/28/2000 PWW Header revision and minor changes      
C  06/09/2002 GH  Modified for Y2K   
C-----------------------------------------------------------------------
C  INPUT  : IHARI,HDLAY,HLATE,HPP,HRP,HSTG,HCOM,HSIZ,HDATE,HPC,CROP
C           YEAR,NHAR
C
C  LOCAL  : LINE,MFHRV,ERRKEY,STNAME,HARMAN,I,NLOOP,MENU,DFHRV,YFHRV,IFHRV,
C           JULIAN,HSTAGE,IDUMM,EFF,FLAG
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SENS
C
C  Calls  : ERROR NAILUJ CLEAR SWHARV JULIAN VERIFY STHARV SELHRV
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SEHARV (RNMODE,IHARI,HDLAY,HLATE,HPP,HRP,YEAR,
     &                   HSTG,HCOM,HSIZ,HDATE,HPC,CROP,NHAR,HBPC,
     &                   YRPLT)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL CLEAR, ERROR, INCYD, INSTGE, JULIAN, NAILUJ, SELHRV, 
     &  STHARV, SWHARV, VERIFY, YR_DOY

      CHARACTER*1  RNMODE,IHARI,LINE(80)
      CHARACTER*2  CROP
      CHARACTER*3  MFHRV
      CHARACTER*5  HSTG(NAPPL),HCOM(NAPPL),HSIZ(NAPPL)
      CHARACTER*6  ERRKEY
      CHARACTER*10 STNAME(20)
      CHARACTER*25 HARMAN

      INTEGER      NLOOP,MENU,DFHRV,YFHRV,IFHRV,YEAR,NHAR,YRPLT
      INTEGER      HDLAY,HLATE,HDATE(NAPPL),JULIAN,HSTAGE,HDATT
      INTEGER      INCYD,IDUMM

      REAL         HPP,HRP,HPC(NAPPL),EFF,FLAG,HBPC(NAPPL)

      PARAMETER (ERRKEY = 'SEHARV')

      NLOOP  = 0

      CALL INSTGE (STNAME,CROP)
      READ (HSTG(1)(4:5),'(I2)') HSTAGE
      IF (HSTAGE .LE. 0) THEN
        SELECT CASE (CROP)
!       IF (INDEX('BNSBPNTMPECHPPPRC3C4BHG1G2G3G4G5G6G7G8BRVBCPFBCOCT',
!    &             CROP) .GT. 0) THEN
          CASE('BN','SB','PN','TM','PE','CH','PP','PR','C3','C4','BH',
     &          'BR','VB','CP','FB','CO','CT','NP','GB','LT', 'SU')
           HSTAGE = 16
!       ELSE IF (INDEX('CS',CROP) .GT. 0) THEN
          CASE('CS')
           HSTAGE = 9
!       ELSE IF (INDEX('PI',CROP) .GT. 0) THEN
          CASE('PI')
           HSTAGE = 20
!       ELSE
          CASE DEFAULT
           HSTAGE = 6
!       ENDIF
        END SELECT
      ENDIF

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (INDEX('IE',RNMODE) .GT. 0) THEN
         IF (IHARI .EQ. 'D') THEN
            IF (HDATE(1) .GT. 9999 .OR. HDATE(1) .LE. 0) THEN
               HDATE(1) = 365
            ENDIF
            HDATT = HDATE(1)
         ELSE
            HDATT = -9
         ENDIF
         IF (IHARI .EQ. 'R' .OR. IHARI .EQ. 'W' .OR.
     &   IHARI .EQ. 'X' .OR. IHARI .EQ. 'Y' .OR. IHARI .EQ. 'Z') THEN
	     IF (HDATE(1) .LE. 1000) THEN
              HDATE(1) = INCYD(YRPLT,365)
           ENDIF
           CALL YR_DOY(HDATE(1),YFHRV,IFHRV)
           CALL NAILUJ (IFHRV,YFHRV,MFHRV,DFHRV)
C          YFHRV = 1900 + YFHRV
         ELSE
           YFHRV = -9
           DFHRV = -9
           MFHRV = ' '
         ENDIF

         IF (IHARI .EQ. 'A') THEN
            HARMAN = 'AUTOMATIC                '
          ELSE IF (IHARI .EQ. 'G') THEN
            HARMAN = 'AT REPORTED GROWTH STAGES'
          ELSE IF (IHARI .EQ. 'M') THEN
            HARMAN = 'AT HARVEST MATURITY      '
          ELSE IF (IHARI .EQ. 'R' .OR. IHARI .EQ. 'W' .OR.
     &   IHARI .EQ. 'X' .OR. IHARI .EQ. 'Y' .OR. IHARI .EQ. 'Z') THEN
            HARMAN = 'ON REPORTED DATE(S)      '
          ELSE IF (IHARI .EQ. 'D') THEN
            HARMAN = 'ON REPORTED DAP          '
         ENDIF

         IF (NHAR .EQ. 0) THEN
            NHAR = 1
         ENDIF
         CALL CLEAR
         WRITE (*,200) HARMAN
         WRITE (*,210) MFHRV,DFHRV,YFHRV
         WRITE (*,220) HDATT
         WRITE (*,230) STNAME(HSTAGE)
         WRITE (*,240) HCOM(1),HSIZ(1),HPC(1),HBPC(1)
         WRITE (*,250)
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
          CALL SWHARV (RNMODE,IHARI,HARMAN)
      ELSE IF (MENU .EQ. 2) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,4650) MFHRV,DFHRV
          READ (5,4550,ERR = 100) MFHRV,DFHRV
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,4700) YFHRV
          READ (5,4600) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0. .AND. EFF .LE. 2200. .AND. FLAG .LE. 0) THEN
             YFHRV = NINT(EFF)
          ENDIF
          IF (DFHRV .GT. 0 .AND. DFHRV .LE. 31) THEN
             IDUMM = JULIAN(DFHRV,MFHRV,YFHRV)
          ENDIF
          IF (IDUMM .GE. 1 .AND. IDUMM .LE. 366) THEN
             IFHRV  = IDUMM
          ENDIF
          HDATE(1) = YFHRV*1000 + IFHRV
      ELSE IF (MENU .EQ. 3) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,4800) HDATE(1)
          READ (5,4600) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0. .AND. EFF .LE. 366. .AND. FLAG .LE.0) THEN
             HDATE(1) = NINT(EFF)
          ENDIF
      ELSE IF (MENU .EQ. 4) THEN
          CALL STHARV (RNMODE,HSTG,STNAME,HSTAGE)
      ELSE IF (MENU .EQ. 5 .OR. MENU .EQ. 6) THEN
          WRITE (*,650)
          PAUSE
      ELSE IF (MENU .EQ. 7) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,4900) HPC(1)
          READ (5,4600) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 100.0 .AND. FLAG .LE. 0) THEN
             HPC(1) = EFF
          ENDIF
      ELSE IF (MENU .EQ. 8) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,5000) HBPC(1)
          READ (5,4600) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 100.0 .AND. FLAG .LE. 0) THEN
             HBPC(1) = EFF
          ENDIF
      ELSE IF (MENU .EQ. 9) THEN
          CALL SELHRV (HDLAY,HLATE,HPP,HRP,RNMODE,YEAR)
      ENDIF

      GO TO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  200 FORMAT (9X,'HARVEST TIMING AND CONTROL',/,
     &        9X,'==========================',//,
     &        5X,' 0. Return to Main Menu ',//
     &        5X,' 1. Harvest Management .................]',3X,A25)
  210 FORMAT (5X,' 2. Harvest Date .......................]',
     &        3X,A3,1X,I2,1X, I4)
  220 FORMAT (5X,' 3. Harvest Days after Planting ........]',I5)
  230 FORMAT (5X,' 4. Harvest Stage ......................]',3X,A10)
  240 FORMAT (5X,' 5. Harvest Component ..................]',3X,A5,/,
     &        5X,' 6. Harvest Size Group .................]',3X,A5,/,
     &        5X,' 7. Harvest Percentage .................]',2X,F5.0,/,
     &        5X,' 8. Byproduct Harvest Percentage........]',2X,F5.0)
  250 FORMAT (5X,' 9. Automatic Harvest Options ..........|',/)
  400 FORMAT (//,9X,'SELECTION (#) ? [ Default = 0 ] ---> ',$)
  650 FORMAT (5X,'Option Currently Not Available')
 4550 FORMAT (A3,1X,I3)
 4600 FORMAT (80A1)
 4650 FORMAT (  9X,'FIRST HARVEST DATE              ===> ',A3,' ',I2,
     &        /,9X,'NEW DATE ? (Ex. JUN 15)         ---> ',$)
 4700 FORMAT (/,9X,'CURRENT FIRST HARVEST YEAR      ===>',1X,I4,
     &        /,9X,'NEW HARVEST YEAR ?              ---> ',$)
 4800 FORMAT(//,9X,'HARVEST IN DAYS AFTER PLANTING  ===>',1X,I5,' dap',
     &        /,9X,'NEW HARVEST DATE ?              --->  ',$)
 4900 FORMAT(//,9X,'PERCENTAGE OF PRODUCT HARVESTED ===>',1X,F5.0,' %',
     &        /,9X,'NEW PERCENTAGE ?                --->  ',$)
 5000 FORMAT(//,9X,'PERCENTAGE OF BYPRODUCT HARVESTED ===>',1X,F5.0,
     &             ' %',
     &        /,9X,'NEW PERCENTAGE ?                --->  ',$)

      END SUBROUTINE SEHARV

C=======================================================================
C  SWHARV, Subroutine
C
C  Determines harvest management strategy
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1991 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C-----------------------------------------------------------------------
C  INPUT  : RNMODE,IHARI,HARMAN
C
C  LOCAL  : ERRKEY,NLOOP,MENU
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SEHARV
C
C  Calls  : ERROR CLEAR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SWHARV (RNMODE,IHARI,HARMAN)

      IMPLICIT     NONE
      EXTERNAL CLEAR, ERROR

      CHARACTER*1  RNMODE,IHARI
      CHARACTER*6  ERRKEY
      CHARACTER*25 HARMAN

      INTEGER      NLOOP,MENU

      PARAMETER (ERRKEY = 'SWPLT ')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (INDEX('IE',RNMODE) .GT. 0) THEN
         CALL CLEAR
         WRITE (*,3400)
         WRITE (*, 290) HARMAN
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 100) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 1) THEN
          IHARI  = 'M'
      ELSE IF (MENU .EQ. 2) THEN
          IHARI  = 'R'
      ELSE IF (MENU .EQ. 3) THEN
          IHARI  = 'D'
      ELSE IF (MENU .EQ. 4) THEN
          IHARI  = 'G'
      ELSE IF (MENU .EQ. 5) THEN
          IHARI  = 'A'
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  290 FORMAT (//,9X,'CURRENT HARVEST MANAGEMENT      ===> ',A25,/
     &           9X,'SELECTION (#) ? [ Default = 0 ] ---> ',$)
 3400 FORMAT (   9X,'HARVEST MANAGEMENT STRATEGY',/,9X,35('='),//,
     &   5X,' 0. Return to Previous Menu ',//
     &   5X,' 1. At Harvest Maturity ............................|',/,
     &   5X,' 2. On Reported Date(s) ............................|',/,
     &   5X,' 3. On Reported Days After Planting ................|',/,
     &   5X,' 4. At Reported Growth Stage(s) ....................|',/,
     &   5X,' 5. Automatic When Conditions are Satisfactory .....|')

      END SUBROUTINE SWHARV

C=======================================================================
C  SELHRV, Subroutine
C
C  Determines automatic harvest management options
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1991 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C-----------------------------------------------------------------------
C  INPUT  : HDLAY,HLATE,HPP,HRP,RNMODE,YEAR
C
C  LOCAL  : LINE,MLHRV,ERRKEY,NLOOP,MENU,ILYRH,ILHRV,IDUMM,JULIAN,DLHRV,
C           FLAG,EFF
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SEHARV
C
C  Calls  : ERROR NAILUJ YR_DOY CLEAR VERIFY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SELHRV (HDLAY,HLATE,HPP,HRP,RNMODE,YEAR)

      IMPLICIT    NONE
      EXTERNAL CLEAR, ERROR, JULIAN, NAILUJ, VERIFY, YDOY, YR_DOY

      CHARACTER*1 RNMODE,LINE(80)
      CHARACTER*3 MLHRV
      CHARACTER*6 ERRKEY

      INTEGER     NLOOP,MENU,ILYRH,ILHRV,IDUMM,JULIAN,DLHRV,HDLAY,HLATE
      INTEGER     YEAR,YDOY
      REAL        FLAG,HRP,HPP,EFF

      PARAMETER (ERRKEY = 'SELHRV')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)
      IF (HLATE .GT. 1000) THEN
	   CALL YR_DOY (HLATE,ILYRH,ILHRV)
         ILYRH = INT(HLATE/1000.0)
         ILHRV = HLATE - ILYRH * 1000
       ELSE
         ILHRV = HLATE
         ILYRH = YEAR
      ENDIF
      CALL NAILUJ (ILHRV,ILYRH,MLHRV,DLHRV)

      IF (INDEX('IE',RNMODE) .GT. 0) THEN
         CALL CLEAR
         WRITE (*,3900) HDLAY,MLHRV,DLHRV,ILYRH,HPP,HRP
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
          IF (INDEX('IE',RNMODE).GT. 0) WRITE (*,4500) HDLAY
          READ (5,4600) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 100.0 .AND. FLAG .LE. 0) THEN
             HDLAY = NINT(EFF)
           ELSE
             GO TO 100
          ENDIF
      ELSE IF (MENU .EQ. 2) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,4650) MLHRV,DLHRV
          READ (5,4550,ERR = 100) MLHRV,DLHRV
          IF (DLHRV .GT. 0 .AND. DLHRV .LE. 31) THEN
             IDUMM = JULIAN (DLHRV,MLHRV,ILHRV)
          ENDIF
          IF (IDUMM .GE. 1 .AND. IDUMM .LE. 366) ILHRV = IDUMM
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,4700) ILYRH

          READ (5,4600) LINE
          CALL VERIFY (LINE,EFF,FLAG)

          IF (EFF .GT. 0.0 .AND. EFF .LE. 3000.0 .AND. FLAG .LE. 0) THEN
             ILYRH = NINT(EFF)
          ENDIF
          HLATE = YDOY(ILYRH,ILHRV)
      ELSE IF (MENU .EQ. 3) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,4800) HPP
          READ (5,4600) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 100.0 .AND. FLAG .LE. 0) THEN
             HPP = EFF
          ENDIF
      ELSE IF (MENU .EQ. 4) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,4900) HRP
          READ (5,4600) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 100.0 .AND. FLAG .LE. 0) THEN
             HRP = EFF
          ENDIF
      ENDIF

      GO TO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

 3900 FORMAT (9X,'AUTOMATIC HARVEST MANAGEMENT',/,
     &        9X,'============================',//,
     & 6X,'0. Return to Previous Menu ',//
     & 6X,'1. Earliest Harvest Date (Days after Maturity) .] ',1X,I5,/,
     & 6X,'2. Latest Harvest Date   (Day of Year ) ........] ',
     &        A3,1X,I2,1X,I4,/,
     & 6X,'3. Product Harvested ........................(%)]',3X,F5.0,/,
     & 6X,'4. Residue Harvested ........................(%)]',3X,F5.0,/)
 4050 FORMAT (//,9X,'SELECTION (#) ? [ Default = 0 ] ---> ',$)
 4500 FORMAT (   9X,'EARLIEST HARVEST DATE           ===> ',I5,
     &      /,   9X,'NEW DATE ? (Ex. 4     )         ---> ',$)
 4550 FORMAT (A3,1X,I3)
 4600 FORMAT (80A1)
 4650 FORMAT (   9X,'LATEST HARVEST DATE             ===> ',A3,' ',I2,
     &      /,   9X,'NEW DATE ? (Ex. JUN 15)         ---> ',$)
 4700 FORMAT (/, 9X,'CURRENT LATEST HARVEST YEAR     ===>',1X,I4,
     &        /, 9X,'NEW HARVEST YEAR ?              ---> ',$)
 4800 FORMAT (//,9X,'PERCENTAGE OF PRODUCT HARVESTED ===>',1X,F5.0,
     &    ' %',/,9X,'NEW PERCENTAGE ?                --->  ',$)
 4900 FORMAT (//,9X,'PERCENTAGE OF RESIDUE HARVESTED ===>',1X,F5.0,
     &    ' %',/,9X,'NEW PERCENTAGE ?                --->  ',$)

      END SUBROUTINE SELHRV

C=======================================================================
C  STHARV, Subroutine
C
C  Determines harvest stage options
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1991 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C-----------------------------------------------------------------------
C  INPUT  : RNMODE,HSTG,STNAME
C
C  LOCAL  : ERRKEY,NLOOP,MENU,HSTAGE,I
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SEHARV
C
C  Calls  : ERROR CLEAR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE STHARV (RNMODE,HSTG,STNAME,HSTAGE)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL CLEAR, ERROR

      CHARACTER*1  RNMODE
      CHARACTER*5  HSTG(NAPPL)
      CHARACTER*6  ERRKEY
      CHARACTER*10 STNAME(20)

      INTEGER      NLOOP,MENU,HSTAGE,I

      PARAMETER (ERRKEY = 'SWPLT ')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (INDEX('IE',RNMODE) .GT. 0) THEN
         CALL CLEAR
c        READ (HSTG(1)(4:5),'(I2)') HSTAGE
c        IF (HSTAGE .EQ. 0) HSTAGE = 20
         WRITE (*,3400)
         WRITE (*,3500) (I,STNAME(I),I = 1,16)
         WRITE (*, 290) STNAME(HSTAGE)
      ENDIF

      READ  (5,'(I2)',ERR = 100) MENU
      IF (MENU .GT. 0) THEN
        WRITE (HSTG(1)(4:5),'(I2)') MENU
        HSTAGE = MENU
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  290 FORMAT (9X,'CURRENT HARVEST STAGE           ===> ',A10,/
     &           9X,'SELECTION (#) ? [ Default = 0 ] ---> ',$)
 3400 FORMAT (9X,'HARVEST STAGE OPTIONS',/,9X,21('='),//,
     &        5X,' 0. Return to Previous Menu ',/)
 3500 FORMAT (16(5X,I2,'. ',A10,' .............|',/))

      END SUBROUTINE STHARV
