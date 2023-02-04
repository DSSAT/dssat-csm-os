C=======================================================================
C SETIME, Subroutine
C
C  Determines simulation and timing control
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1991 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C  06/06/2002 GH  Modified for Y2K
C-----------------------------------------------------------------------
C  INPUT  : ISIM,YEAR,ENDSIM,NYRS,FILEW,RNMODE,MESIC,YRSIM,YRPLT
C
C  LOCAL  : LINE,MSSIM,MESIM,ERRKEY,FILEWW,INMANT,NLOOP,MENU,YR,IDUMM,
C           DSSIM,DESIM,JULIAN,IPYRS,ESIM,EPYRS,FEXIST,FLAG,EFF
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SENS
C
C  Calls  : ERROR YR_DOY NAILUJ CLEAR SWINSC VERIFY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SETIME (ISIM,YEAR,NYRS,FILEW,RNMODE,
     &           MESIC,YRSIM,YRPLT,IHARI,HDATE,NHAR,PATHWT)

      IMPLICIT     NONE
      EXTERNAL CLEAR, ERROR, JULIAN, NAILUJ, SWINSC, VERIFY, YDOY,YR_DOY

      CHARACTER*1  RNMODE,IHARI,MESIC,LINE(80),BLANK
      CHARACTER*3  MSSIM,MESIM
      CHARACTER*6  ERRKEY
      CHARACTER*12 FILEW,WTHTEM
      CHARACTER*15 INMANT
      CHARACTER*80 PATHWT
      CHARACTER*92 FILEWW

      INTEGER      NLOOP,MENU,YEAR,ISIM,ENDSIM,NYRS,YR,IDUMM,PATHL
      INTEGER      DSSIM,DESIM,JULIAN,YRSIM,IPYRS,YRPLT,ESIM,EPYRS
      INTEGER      HDATE(3),NHAR,YDOY
      LOGICAL      FEXIST
      REAL         FLAG,EFF

      PARAMETER (ERRKEY = 'SETIME')
      PARAMETER (BLANK  = ' ')

      IF (MESIC .EQ. 'M') THEN
         INMANT = 'AS REPORTED    '
       ELSE IF (MESIC .EQ. 'S') THEN
         INMANT = 'PREVIOUS RUN   '
      ENDIF

      NLOOP  = 0
      YR     = YEAR
      WTHTEM = FILEW
      ENDSIM = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      CALL YR_DOY (YRSIM,IPYRS,ISIM)
      CALL NAILUJ (ISIM,IPYRS,MSSIM,DSSIM)

      IF (ENDSIM .GT. 0) THEN
         CALL YR_DOY (ENDSIM,EPYRS,ESIM)
         CALL NAILUJ (ESIM,EPYRS,MESIM,DESIM)
       ELSE
         MESIM = '   '
         DESIM = -9
      ENDIF

      IF (INDEX('IE',RNMODE) .GT. 0) THEN
         CALL CLEAR
         WRITE (*,200) MSSIM,DSSIM,YEAR,MESIM,DESIM,NYRS,INMANT
         IF (YR .NE. YEAR)     WRITE (*,300)
         IF (NYRS .GT. 1)      WRITE (*,400)
         IF (YRPLT .LT. YRSIM) WRITE (*,450)
         WRITE (*,500)
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
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,700) MSSIM,DSSIM
          READ (5,800,ERR = 100) MSSIM,DSSIM
          IF (DSSIM .GT. 0 .AND. DSSIM .LE. 31) THEN
             IDUMM = JULIAN (DSSIM,MSSIM,IPYRS)
          ENDIF
          IF (IDUMM .GE. 1 .AND. IDUMM .LE. 366) THEN
             ISIM  = IDUMM
             YRSIM = IPYRS*1000 + ISIM
          ENDIF
      ELSE IF (MENU .EQ. 2) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,1000) YEAR
          READ (5,1100) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 3000.0 .AND. FLAG .LE. 0) THEN
             YR = NINT(EFF)
	       IF (YR .LT. 100) THEN
	         IF (YR .LE. 10) THEN
                 YR = 2000 + YR
	         ELSE
	           YR = 1900 + YR
               ENDIF
	      ENDIF
          ENDIF
	    IF (YR .LT. 2000) THEN
	      WRITE (WTHTEM(5:6),'(I2.2)') (YR - 1900)
	    ELSE IF (YEAR .LT. 3000) THEN
	      WRITE (WTHTEM(5:6),'(I2.2)') (YR - 2000)
	    ENDIF
          INQUIRE (FILE = WTHTEM,EXIST = FEXIST)
          IF (.NOT. FEXIST .AND. PATHWT(1:1) .NE. ' ') THEN
             PATHL  = INDEX (PATHWT,BLANK)
             FILEWW = PATHWT (1:(PATHL-1)) // WTHTEM
             INQUIRE (FILE = FILEWW,EXIST = FEXIST)
          ENDIF
          IF (FEXIST) THEN
             YEAR   = YR
             FILEW  = WTHTEM
             YRSIM  = YEAR*1000+ISIM
          ENDIF
      ELSE IF (MENU .EQ. 3) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,900) MESIM,DESIM
          READ (5,800,ERR = 100) MESIM,DESIM
          IF (EPYRS .EQ. 0) EPYRS = YEAR
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,950) EPYRS
          READ (5,1100) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 3000.0 .AND. FLAG .LE. 0) THEN
             EPYRS = NINT(EFF)
          ENDIF
          IF (DESIM .GT. 0 .AND. DESIM .LE. 31) THEN
             IDUMM = JULIAN (DESIM,MESIM,EPYRS)
          ENDIF
          IF (IDUMM .GE. 1 .AND. IDUMM .LE. 366) THEN
             ENDSIM = YDOY(EPYRS, IDUMM)
          ENDIF
          IF (ENDSIM .GT. 0) THEN
             HDATE(1) = ENDSIM
             IHARI = 'R'
             IF (NHAR .EQ. 0) NHAR = 1
          ENDIF
      ELSE IF (MENU .EQ. 4) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,1300) NYRS
          READ (5,1100) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 999. .AND. FLAG .LE. 0) THEN
             NYRS = NINT(EFF)
          ENDIF
      ELSE IF (MENU .EQ. 5) THEN
          CALL SWINSC (RNMODE,MESIC)
      ENDIF

      GO TO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  200 FORMAT (9X,'SIMULATION TIMING AND CONTROL',/,
     &        9X,'=============================',//,
     &    5X,' 0. Return to Main Menu ',//
     &    5X,' 1. Start of Simulation Date ...........] ',A3,1X,I2,/,
     &    5X,' 2. Year of Simulation .................]',3X,I4,/,
     &    5X,' 3. End of Simulation Date .............] ',A3,1X,I2,/,
     &    5X,' 4. Number of Years to Be Simulated ....] ',I6,/,
     &    5X,' 5. Initial Conditions .................] ',A15,/)
  300 FORMAT (/,
     &    9X,'You selected a year for which no weather file exists !',
     &  /,9X,'Please reselect simulation year')
  400 FORMAT (/,
     &    9X,'You selected more than one year of simulation.',
     &  /,9X,'Verify that weather data exist for all these years.')
  450 FORMAT (/,
     &    9X,'Start of simulation date is after planting date.',
     &  /,9X,'Modify start of simulation or simulation will terminate!')
  500 FORMAT (//,9X, 'SELECTION (#) ? [ Default = 0 ] ---> ',$)
  700 FORMAT (9X,'SELECTED SIMULATION INITIATION DATE ===> ',A3,' ',I2,
     &      /,9X,'NEW DATE ? (Ex. JUN 15)             ---> ',$)
  800 FORMAT (A3,1X,I3)
  900 FORMAT (9X,'SELECTED END OF SIMULATION DATE     ===> ',A3,' ',I2,
     &      /,9X,'NEW DATE ? (Ex. JUN 15)             ---> ',$)
  950 FORMAT (//,9X,'CURRENT END OF SIMULATION YEAR   ===>',1X,I4,
     &         /,9X,'NEW END OF SIMULATION YEAR       ---> ',$)
 1000 FORMAT (//,9X,'CURRENT SIMULATION YEAR          ===>',1X,I4,
     &         /,9X,'NEW SIMULATION YEAR ?            ---> ',$)
 1100 FORMAT (80A1)
 1300 FORMAT (//,9X,'CURRENT NUMBER OF YEARS          ===>',1X,I4,/,
     &           9X,'NEW NUMBER OF YEARS ?            --->',3X,' ',$)

      END SUBROUTINE SETIME
