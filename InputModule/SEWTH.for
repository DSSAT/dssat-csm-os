C=======================================================================
C  SEWTH, Subroutine
C
C  Determines weather sensitivity analysis options
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1993 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C  02/02/1998 GH  Fix WMODI
C  12/11/2000 GH  Fix output format
C  06/09/2002 GH  Modified for Y2K
C
C-----------------------------------------------------------------------
C  INPUT  : FILEW,WMODI,WTHADJ,PATHWT,MEWTH,YEAR
C
C  LOCAL  : BLANK,PROCOD,ERRKEY,FILEWT,WTHTEM,WTMANT,FILEWW,FILEXX,MENU,
C           NLOOP,PATHL,FEXIST
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SENS
C
C  Calls  : ERROR PATH CLEAR SWWTH IDWTH WTHMDI
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SEWTH (FILEW,RNMODE,WMODI,WTHADJ,PATHWT,MEWTH,YEAR,
     &           DSSATP,FILEWT,RSEED1)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL CLEAR, ERROR, IDWTH, PATH, SECLI, SWWTH, WTHMDI

      CHARACTER*1  RNMODE,MEWTH,BLANK,WMODI
      CHARACTER*3  PROCOD
      CHARACTER*6  ERRKEY
      CHARACTER*7  FILEWT
      CHARACTER*12 FILEW,WTHTEM,NAMEF
      CHARACTER*15 WTMANT
      CHARACTER*102 DSSATP
      CHARACTER*80 PATHWT
      CHARACTER*92 FILEWW,FILEXX

      INTEGER      MENU,NLOOP,PATHL,YEAR,YR,RSEED1
      LOGICAL      FEXIST
      REAL         WTHADJ(2,8)

      PARAMETER (ERRKEY = 'SEWTH ')
      PARAMETER (BLANK  = ' ')

      WTHTEM = FILEW
      NLOOP  = 0
      FEXIST = .TRUE.

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,BLANK,0)

      IF (MEWTH .EQ. 'M') THEN
         WTMANT = 'OBSERVED DATA  '
         PROCOD = 'WED'
       ELSE IF (MEWTH .EQ. 'G') THEN
         WTMANT = 'SIMULATED DATA '
         PROCOD = 'WGD'
       ELSE IF (MEWTH .EQ. 'S' .OR. MEWTH .EQ. 'W') THEN
         WTMANT = 'INTERNALLY GEN.'
         PROCOD = 'CLD'
      ENDIF

      INQUIRE (FILE = FILEW,EXIST = FEXIST)
      IF (.NOT. FEXIST .AND. PATHWT(1:1) .EQ. ' ') THEN
          CALL PATH (PROCOD,DSSATP,PATHWT,1,NAMEF)
      ENDIF

      IF (INDEX('IE',RNMODE) .GT. 0)  THEN
         CALL CLEAR
         WRITE (*,200) WTMANT,FILEW,PATHWT,WTHTEM,WMODI
         IF (MEWTH .EQ. 'S' .OR. MEWTH .EQ. 'W') THEN
           WRITE(*,210)
         ENDIF
         PATHL  = INDEX (PATHWT,BLANK)
         IF (PATHL .LE. 1) THEN
            FILEWW = FILEW
            FILEXX = WTHTEM
          ELSE
            FILEWW = PATHWT (1:(PATHL-1)) // FILEW
            FILEXX = PATHWT (1:(PATHL-1)) // WTHTEM
         ENDIF
         INQUIRE (FILE = FILEWW, EXIST = FEXIST)
         IF (.NOT. FEXIST) THEN
            WRITE (*,270) FILEW
          ELSE
            INQUIRE (FILE = FILEXX, EXIST = FEXIST)
            IF (.NOT. FEXIST) WRITE (*,270) WTHTEM
            FEXIST = .TRUE.
         ENDIF
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
          CALL SWWTH (RNMODE,MEWTH,PATHWT)
          IF (YEAR .LT. 2000) THEN
            YR = YEAR - 1900
          ELSE IF (YEAR .LT. 3000) THEN
            YR = YEAR - 2000
          ENDIF
          IF (MEWTH .EQ. 'M') THEN
             IF (FILEW(11:11) .EQ. BLANK) THEN
                WRITE (FILEW (5:12),170) YR,'01.WTH'
                WRITE (WTHTEM(5:12),170) YR,'01.WTH'
              ELSE
                WRITE (FILEW (12:12),'(A1)') 'H'
                WRITE (WTHTEM(12:12),'(A1)') 'H'
             ENDIF
           ELSE IF (MEWTH .EQ. 'G') THEN
             IF (FILEW(11:11) .EQ. BLANK) THEN
                WRITE (FILEW (5:12),170) YR,'01.WTG'
                WRITE (WTHTEM(5:12),170) YR,'01.WTG'
              ELSE
                WRITE (FILEW (12:12),'(A1)') 'G'
                WRITE (WTHTEM(12:12),'(A1)') 'G'
             ENDIF
           ELSE IF (MEWTH .EQ. 'S' .OR. MEWTH .EQ. 'W') THEN
             WRITE (FILEW (5:12),'(A8)') '.CLI    '
             WRITE (WTHTEM(5:12),'(A8)') '.CLI    '
          ENDIF
      ELSE IF (MENU .EQ. 2) THEN
          CALL IDWTH (FILEW,PATHWT,RNMODE,FILEWT)
          WTHTEM = FILEW
      ELSE IF (MENU .EQ. 3) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,800) PATHWT
          READ (5,'(A80)') PATHWT
          PATHL  = INDEX (PATHWT,BLANK)
          IF (PATHL .GT. 1) THEN
             IF (PATHWT((PATHL-1):(PATHL-1)) .NE. SLASH) THEN
                WRITE (PATHWT(PATHL:PATHL),'(A1)') SLASH
                PATHL = PATHL+1
             ENDIF
          ENDIF
      ELSE IF (MENU .EQ. 4) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,600) FILEW
          READ (5,'(A12)') WTHTEM
          IF (PATHL .LE. 1) THEN
             FILEXX = WTHTEM
           ELSE
             FILEXX = PATHWT (1:(PATHL-1)) // WTHTEM
          ENDIF
          INQUIRE (FILE = FILEXX, EXIST = FEXIST)
          IF (FEXIST) THEN
              FILEW = WTHTEM
          ENDIF
          IF (WTHTEM(1:1) .EQ. BLANK) WTHTEM = FILEW
      ELSE IF (MENU .EQ. 5) THEN
          CALL WTHMDI (RNMODE,WMODI,WTHADJ)
      ELSE IF (MENU .EQ. 6) THEN
          CALL SECLI (FILEW,RNMODE,WMODI,WTHADJ,PATHWT,MEWTH,YEAR,
     &            RSEED1,DSSATP,FILEWT)
      ENDIF

      GO TO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  170 FORMAT (I2.2,A6)
  200 FORMAT (//,
     &        15X,'WEATHER DATA SELECTION AND MODIFICATION',/,
     &        15X,'=======================================',//,
     &         5X,' 0. Return to Main Menu ',//,
     &         5X,' 1. Recorded/Simulated Data ............[ ',A15,/,
     &         5X,' 2. Weather Data Selection .............[ ',A12,/,
     &         5X,' 3. Weather Data Path ..................[ ',A30,/,
     &         5X,' 4. Enter Weather File Name Interactive.[ ',A12,/,
     &         5X,' 5. Weather Data Modification ..........[ ',A1)
  210 FORMAT  (5X,' 6. Weather Generator Options ..........| ')
  270 FORMAT ( //,
     &    9X,'File ',A12,' does not exist.',9X,/,
     &    9X,'Please select another file [Option 2 or 4]',/,
     &    9X,'or another weather data option [Option 1].')
  290 FORMAT (//,9X,'SELECTION ? [ Default = 0 ] ===> ',$)
  600 FORMAT (//,
     &    9X,'Please enter a weather file name using the following',/,
     &    9X,'format :       [ ',A12,' ] ',/,
     &    9X,'SELECTION ? ===> ',$)
  800 FORMAT (//,
     &    9X,'Please enter a weather path name using the following',/,
     &    9X,'format :       [ ',A50,' ] ',/,
     &    9X,'SELECTION ? ===> ',$)

      END SUBROUTINE SEWTH

C=======================================================================
C  IDWTH, Subroutine
C
C  Determines weather data selection
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1991 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C-----------------------------------------------------------------------
C  INPUT  : FILEW,PATHWT,RNMODE,FILEWT
C
C  LOCAL  : ANS,LINE,BLANK,YEARW,ERRKEY,FILEW,TITLEW,CHARTEST,FILETT,FILEWW,
C           I,NWTH,ERRNUM,NLOOP,PATHL,LUNLST,ISECT,LINF,FEXIST,FLAG,WTH,LATW,
C           LONGW,ELEVW
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SEWTH
C
C  Calls  : ERROR CLEAR IGNORE VERIFY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IDWTH (FILEW,PATHWT,RNMODE,FILEWT)

      IMPLICIT     NONE
      EXTERNAL CLEAR, ERROR, IGNORE, VERIFY

      CHARACTER*1  RNMODE,LINE(80),BLANK  !ANS,
      CHARACTER*4  YEARW
      CHARACTER*6  ERRKEY
      CHARACTER*7  FILEWT
      CHARACTER*12 FILEW,FILEWX
      CHARACTER*40 TITLEW
      CHARACTER*80 PATHWT,CHARTEST
      CHARACTER*92 FILETT,FILEWW

      INTEGER      I,NWTH,ERRNUM,NLOOP,PATHL,LUNLST,ISECT,LINF,YR
      LOGICAL*4    FEXIST
      REAL         FLAG,WTH,LATW,LONGW,ELEVW

      PARAMETER (ERRKEY = 'IDWTH ')
      PARAMETER (BLANK  = ' ')
      PARAMETER (LUNLST = 7)

      NLOOP  = 0
      FILEWX = FILEW
      PATHL  = INDEX(PATHWT,BLANK)

      IF (PATHL .LE. 1) THEN
         FILETT = FILEWT
         FILEWW = FILEW
       ELSE
         FILETT = PATHWT (1:(PATHL-1)) // FILEWT
         FILEWW = PATHWT (1:(PATHL-1)) // FILEW
      ENDIF

      OPEN (LUNLST,FILE = FILETT,STATUS = 'OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) RETURN

      INQUIRE (FILE = FILEWW, EXIST = FEXIST)
  50  CONTINUE
      REWIND (7)
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 50) CALL ERROR (ERRKEY,1,BLANK,0)
      I      = 0
      NWTH   = 0

      IF (INDEX('IE',RNMODE) .GT. 0) THEN
          CALL CLEAR
          WRITE (*,100)
      ENDIF

  200 I  = I + 1
  150 CONTINUE
      CALL IGNORE (LUNLST,LINF,ISECT,CHARTEST)
      IF (ISECT .EQ. 2) GO TO 150
      IF (ISECT .EQ. 1) THEN

!CHP  Replace with 5x format at start for revised WTH.LST file
!         IF (I .LT. 100) THEN
!            READ (CHARTEST,300,IOSTAT=ERRNUM) FILEWX,TITLEW,
!     &                                     LATW,LONGW,ELEVW
!         ELSE
!            READ (CHARTEST,350,IOSTAT=ERRNUM) FILEWX,TITLEW,
!     &                                     LATW,LONGW,ELEVW
!         ENDIF
         IF (MOD(I,16) .EQ. 0) THEN
           PAUSE "Enter to continue list of weather files"
         ENDIF
         READ (CHARTEST,355,IOSTAT=ERRNUM) FILEWX,TITLEW,
     &                                     LATW,LONGW,ELEVW
  355    FORMAT (5X,A12,1X,A26,2(1X,F8.3),1X,F5.0)
!CHP  End change

         FILEWX(9:9) = '.'
         IF (ELEVW .LE. -9000.) ELEVW = -0.1
         IF (LATW  .LT. - 180.) LATW  = -0.1
         IF (LONGW .LT. - 180.) LONGW = -0.1
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEWT,LINF)

         IF (FILEW(6:8) .EQ. 'CLI') THEN
            IF (FILEW(1:4) .EQ. FILEWX(1:4)) NWTH = I
          ELSE
            IF (FILEW(1:12) .EQ. FILEWX(1:12)) NWTH = I
         ENDIF

         IF (FILEWX(12:12) .EQ. BLANK) THEN
            YEARW = '    '
          ELSE
            READ(FILEWX(5:6),'(I2)') YR
          IF (YR .GE. 9) THEN
            WRITE (YEARW(1:4),'(2A2)') '19',FILEWX(5:6)
          ELSE
            WRITE (YEARW(1:4),'(2A2)') '20',FILEWX(5:6)
          ENDIF
         ENDIF

         IF (INDEX('IE',RNMODE) .GT. 0) THEN
            WRITE (*,400) I,TITLEW,LATW,LONGW,ELEVW,YEARW,FILEWX(1:12)
         ENDIF

!         IF (MOD(I,15) .EQ. 0 .AND. RNMODE .EQ. 'I') THEN
!            WRITE (*,500)
!            READ  (5,'(A1)') ANS
!         ENDIF
         GO TO 200
       ELSE
         REWIND LUNLST
      ENDIF

      IF (INDEX('IE',RNMODE) .GT. 0) THEN
         IF (NWTH .EQ. 0) WRITE  (*,950) FILEW,FILEWT
         IF (.NOT. FEXIST) WRITE (*,250) FILEW
         IF (NWTH .EQ. 0 .OR. .NOT. FEXIST) WRITE (*,275) I
      ENDIF

  800 CONTINUE
      LINE(1) = BLANK
      IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,900) NWTH
      READ (5,'(80A1)') LINE
      CALL VERIFY (LINE,WTH,FLAG)

      IF (WTH .LE. 0.0) THEN
         RETURN
       ELSE IF ((FLAG .GT. 0) .OR. (WTH .GT. (I))) THEN
         WRITE (*,1010) (I-1)
         GO TO 800
       ELSE IF (NINT(WTH) .EQ. I) THEN
         GO TO 50
       ELSE IF (WTH .NE. NINT(WTH)) THEN
         WRITE (*,1015)
         GO TO 800
       ELSE IF (WTH .GT. 0.0) THEN
         NWTH = NINT(WTH)
       ELSE
         GO TO 50
      ENDIF

      I = 0
 1200 CONTINUE
      I = I + 1
 1250 CONTINUE
      CALL IGNORE (LUNLST,LINF,ISECT,CHARTEST)
      IF (ISECT .EQ. 2) GO TO 1250

      IF (ISECT .EQ. 1) THEN

!CHP  Replace with 5x format at start for revised WTH.LST file
!         IF (I .LT. 100) THEN
!            READ (CHARTEST,300,IOSTAT=ERRNUM) FILEWX,TITLEW,
!     &                                     LATW,LONGW,ELEVW
!         ELSE
!            READ (CHARTEST,350,IOSTAT=ERRNUM) FILEWX,TITLEW,
!     &                                     LATW,LONGW,ELEVW
!         ENDIF
          READ (CHARTEST,355,IOSTAT=ERRNUM) FILEWX,TITLEW,
     &                                     LATW,LONGW,ELEVW
!CHP  End change

         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEW,LINF)
         FILEWX(9:9) = '.'
         IF (I .LT. NWTH) GO TO 1200
       ELSE
         CALL ERROR (ERRKEY,1,FILEWT,LINF)
      ENDIF

      IF (FILEW(6:8) .EQ. 'CLI') THEN
         FILEWX(1:12) = FILEWX(1:4) // '.CLI    '
      ENDIF

      IF (PATHL .LE. 1) THEN
         FILEWW = FILEWX
       ELSE
         FILEWW = PATHWT(1:(PATHL-1)) // FILEWX
      ENDIF

      INQUIRE (FILE = FILEWW, EXIST = FEXIST)
      IF (.NOT. FEXIST) GO TO 50
      FILEW = FILEWX
      CLOSE (7)

      RETURN

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  100 FORMAT (6X,'WEATHER LOCATIONS',
     &  13X,'LAT.',5X,'LONG',3X,'ELEV',2X,'YEAR',3X,'DATA SET',/,
     &  6X,25('-'),2(1X,8('-')),1X,6('-'),2X,'----',
     &  3X,12('-'))
  250 FORMAT (/,6X,'File ',A12,' does not exist !')
  275 FORMAT (/,6X,'Enter [  0] to Return to the main menu;',
     &        /,6X,'enter [',I3,'] to redisplay current menu;',
     &        /, 6X,'or select another entry')
  300 FORMAT (3X,A12,1X,A26,2(1X,F8.3),1X,F5.0)
  350 FORMAT (4X,A12,1X,A26,2(1X,F8.3),1X,F5.0)
  400 FORMAT (I5,')',1X,A25,2(1X,F8.3),1X,F6.0,2X,A4,3X,A12)
  500 FORMAT (/,6X,'More.... PRESS ENTER KEY')
  900 FORMAT (/,6X,'WEATHER DATA SELECTED ===>',1X,I3,
!chp     &        /,6X,'ALTERNATE SELECTION ? --->',2X,' ',$)
     &        /,6X,'ALTERNATE SELECTION ? --->','  ',$)
  950 FORMAT (/,6X,
     &      'FILE ',A12,' is not in the WEATHER DIRECTORY file ',A7,'!')
 1010 FORMAT (10X,'ERROR! Weather selection must be between 1 and',I3,/)
 1015 FORMAT (10X,'ERROR! Weather selection must be an INTEGER value',/)

      END SUBROUTINE IDWTH
