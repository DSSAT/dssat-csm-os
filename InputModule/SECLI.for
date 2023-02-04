C=======================================================================
C  SECLI, Subroutine
C
C  Determines weather sensitivity analysis options
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1993 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C  02/02/1998 GH  Fix WMODI
C  06/09/2002 GH  Modified for Y2K
C
C-----------------------------------------------------------------------
C  INPUT  : FILEW,WMODI,WTHADJ,PATHWT,MEWTH,YEAR,RSEED1
C
C  LOCAL  : LINE,BLANK,PROCOD,ERRKEY,FILEWT,WTHTEM,WTMANT,WGMANT,PATHWT
C           FILEWW,FILEXX,MENU,NLOOP,PATHL,RSEED1,FEXIST,FLAG,EFF
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SENS
C
C  Calls  : ERROR SWWTH SWWGEN WTHMDI VERIFY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SECLI (FILEW,RNMODE,WMODI,WTHADJ,PATHWT,MEWTH,YEAR,
     &                  RSEED1,DSSATP,FILEWT)

      USE ModuleData
      IMPLICIT     NONE
      EXTERNAL CLEAR, ERROR, IDWTH, PATH, SEWTH, SWWGEN, SWWTH, 
     &  VERIFY, WTHMDI

      CHARACTER*1  RNMODE,MEWTH,LINE(80),BLANK,WMODI
      CHARACTER*3  PROCOD
      CHARACTER*6  ERRKEY
      CHARACTER*8  WSTAT
      CHARACTER*7  FILEWT
      CHARACTER*12 FILEW,WTHTEM,NAMEF
      CHARACTER*15 WTMANT,WGMANT
      CHARACTER*102 DSSATP
      CHARACTER*80 PATHWT
      CHARACTER*92 FILEWW,FILEXX

      INTEGER      MENU,NLOOP,PATHL,YEAR,RSEED1,YR
      LOGICAL      FEXIST
      REAL         WTHADJ(2,8),FLAG,EFF

      PARAMETER (ERRKEY = 'SECLI ')
      PARAMETER (BLANK  = ' ')

      WTHTEM = FILEW
      NLOOP  = 0
      FEXIST = .TRUE.

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)
      IF (MEWTH .EQ. 'M') THEN
          WTMANT = 'OBSERVED DATA  '
          PROCOD = 'WED'
      ELSE IF (MEWTH .EQ. 'G') THEN
          WTMANT = 'SIMULATED DATA '
          PROCOD = 'WGD'
      ELSE IF (MEWTH .EQ. 'S') THEN
          WTMANT = 'INTERNALLY GEN.'
          PROCOD = 'CLD'
          WGMANT = 'SIMMETEO       '
      ELSE IF (MEWTH .EQ. 'W') THEN
          WTMANT = 'INTERNALLY GEN.'
          PROCOD = 'CLD'
          WGMANT = 'WGEN           '
      END IF

      INQUIRE (FILE = FILEW,EXIST = FEXIST)
      IF (.NOT. FEXIST .AND. PATHWT(1:1) .EQ. ' ') THEN
         CALL PATH (PROCOD,DSSATP,PATHWT,1,NAMEF)
      ENDIF

      IF (INDEX('IE',RNMODE) .GT. 0)  THEN
         CALL CLEAR
         WRITE (*,200) WTMANT,WGMANT,FILEW(1:4),PATHWT,WTHTEM,
     &                 RSEED1,WMODI
         IF (MEWTH .EQ. 'M' .OR. MEWTH .EQ. 'G') THEN
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
              IF (FILEW(11:11) .EQ. ' ') THEN
                 WRITE (FILEW (5:12),170) YR,'01.WTH'
                 WRITE (WTHTEM(5:12),170) YR,'01.WTH'
               ELSE
                 WRITE (FILEW (12:12),'(A1)') 'H'
                 WRITE (WTHTEM(12:12),'(A1)') 'H'
              ENDIF
          ELSE IF (MEWTH .EQ. 'G') THEN
              IF (FILEW(11:11) .EQ. ' ') THEN
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
          CALL SWWGEN (RNMODE,MEWTH)
      ELSE IF (MENU .EQ. 3) THEN
          CALL IDWTH (FILEW,PATHWT,RNMODE,FILEWT)
          WTHTEM = FILEW
      ELSE IF (MENU .EQ. 4) THEN
          IF (RNMODE .EQ. 'I') WRITE (*,800) PATHWT
          READ (5,'(A80)') PATHWT
          PATHL  = INDEX (PATHWT,BLANK)
          IF (PATHL .GT. 1) THEN
             IF (PATHWT((PATHL-1):(PATHL-1)) .NE. SLASH) THEN
                WRITE (PATHWT(PATHL:PATHL),'(A1)') SLASH
                PATHL = PATHL+1
             ENDIF
          ENDIF
      ELSE IF (MENU .EQ. 5) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,600) FILEW
          READ (5,'(A12)') WTHTEM
          IF (PATHL .LE. 1) THEN
             FILEXX = WTHTEM
           ELSE
             FILEXX = PATHWT (1:(PATHL-1)) // WTHTEM
          ENDIF
          INQUIRE (FILE = FILEXX, EXIST = FEXIST)
          IF (FEXIST) FILEW = WTHTEM
          IF (WTHTEM(1:1) .EQ. ' ') WTHTEM = FILEW
      ELSE IF (MENU .EQ. 6) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,1100) RSEED1
          READ (5,'(80A1)') LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. FLAG .LE. 0) THEN
             RSEED1 = NINT(EFF)
          ENDIF
      ELSE IF (MENU .EQ. 7) THEN
          CALL WTHMDI (RNMODE,WMODI,WTHADJ)
      ELSE IF (MENU .EQ. 8) THEN
          CALL SEWTH (FILEW,RNMODE,WMODI,WTHADJ,PATHWT,MEWTH,YEAR,
     &         DSSATP,FILEWT,RSEED1)

      ENDIF
      WSTAT = FILEW(1:8)
      CALL PUT('WEATHER','WSTA',WSTAT)

      GO TO 100

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  170 FORMAT (I2,A6)
  200 FORMAT (//,
     &      15X,'CLIMATE DATA SELECTION AND MODIFICATION',/,
     &      15X,'=======================================',//,
     &   5X,' 0. Return to Main Menu ',//,
     &   5X,' 1. Simulated/Recorded Data ............[ ',A15,/,
     &   5X,' 2. Weather Generator ..................[ ',A15,/,
     &   5X,' 3. Climate Data Site ..................[ ',A4,/,
     &   5X,' 4. Climate Data Path ..................[ ',A30,/,
     &   5X,' 5. Enter Climate File Name Interactive.[ ',A12,/,
     &   5X,' 6. Random Seed for Generator ..........[ ',I5,/,
     &   5X,' 7. Weather Data Modification ..........[ ',A1)
  210 FORMAT
     &  (5X,' 8. Weather Data Options ...............| ')
  270 FORMAT (/,
     &      9X,'File ',A12,' does not exist.',9X,/,
     &      9X,'Please select another file [Option 2 or 4]',/,
     &      9X,'or another weather data option [Option 1].')
  290 FORMAT (//,9X,'SELECTION ? [ Default = 0 ] ===> ',$)
  600 FORMAT (//,
     &      9X,'Please enter a weather file name using the following',/,
     &      9X,'format :       [ ',A12,' ] ',/,
     &      9X,'Selection ? ===> ',$)
  800 FORMAT (//,
     &      9X,'Please enter a weather path name using the following',/,
     &      9X,'format :       [ ',A50,' ] ',/,
     &      9X,'Selection ? ===> ',$)
 1100 FORMAT (//,
     &      9X,'RANDOM SEED FOR WEATHER GENERATOR  ===>',1X,I5,/,
     &      9X,'[VALUE BETWEEN > 0]',/,
     &      9X,'NEW RANDOM SEED ?                  --->  ',$)

      END SUBROUTINE SECLI

C=======================================================================
C  SWWGEN, Subroutine
C
C  Determines weather generators methods
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1991 GH  Written
C  05/28/1993 PWW Header revision and minor changes 
C-----------------------------------------------------------------------
C  INPUT  : RNMODE,MEWTH
C
C  LOCAL  : ERRKEY,WGMANT,NLOOP,MENU
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SECLI
C
C  Calls  : ERROR CLEAR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SWWGEN (RNMODE,MEWTH)

      IMPLICIT     NONE
      EXTERNAL CLEAR, ERROR

      CHARACTER*1  RNMODE,MEWTH
      CHARACTER*6  ERRKEY
      CHARACTER*15 WGMANT

      INTEGER      NLOOP,MENU

      PARAMETER (ERRKEY = 'SWWGEN')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (INDEX('IE',RNMODE) .GT. 0) THEN
         CALL CLEAR
         IF (MEWTH .EQ. 'S') THEN
            WGMANT = 'SIMMETEO       '
          ELSE IF (MEWTH .EQ. 'W') THEN
            WGMANT = 'WGEN           '
         ENDIF
         WRITE (*,3400)
         WRITE (*, 290) WGMANT
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 100) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 1) THEN
          MEWTH  = 'S'
      ELSE IF (MENU .EQ. 2) THEN
          MEWTH  = 'W'
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  290 FORMAT (//,
     &        9X,'CURRENT WEATHER GENERATOR        ===> ',A15,/
     &        9X,'SELECTION (#) ? [ Default = 0 ]  ---> ',$)
 3400 FORMAT (9X,'WEATHER GENERATOR METHODS',/,
     &        9X,'=========================',//,
     & 5X,' 0. Return to Previous Menu ',//
     & 5X,' 1. SIMMETEO - Shu Geng ..............................|',/,
     & 5X,' 2. WGEN - Richardson ................................|')

      END SUBROUTINE SWWGEN
