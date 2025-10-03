!=======================================================================
!  WTHFNAME, Subroutine
!
!  Program to check the Weather Filename and directory location
!-----------------------------------------------------------------------
!  04/01/2025 FO  Written
!  07/16/2025 FO Added YR logic to handle 2-4 digit dates
!=======================================================================

      SUBROUTINE WTHFDIR (CONTROL,MEWTH,FILEX,WSTA,WSTA1,YRDOY,         ! Input
     &                    DSSATP,PATHEX,                                ! Input
     &                    FILEW, FILEWG, FILEWC,                        ! Output
     &                    FILEWTH, PATHWT, PATHWTC, PATHWTG, PATHWTW)   ! Output
      USE ModuleDefs
      USE ModuleData    

      IMPLICIT NONE
      EXTERNAL ERROR, WARNING, YR_DOY, PATH

      CHARACTER*1  BLANK, RNMODE, MEWTH
      CHARACTER*3  PROCOD, PROCODG, PROCODC, PROCODW
      CHARACTER*4  WSTA, WSTA1
      CHARACTER*6  ERRKEY
      CHARACTER*8  FILEW4
      CHARACTER*12 NAMEF, FILEX, FILEW, FILEWC, FILEWG, FILEWTH
      CHARACTER*78 MSG(4)
      CHARACTER*80 PATHEX, PATHWT, PATHWTC, PATHWTG, PATHWTW
      CHARACTER*92 FILETMP
      CHARACTER*102 DSSATP

      INTEGER YEAR, YR, YRDOY, DOY, LINEXP, I

      LOGICAL FEXIST

      PARAMETER (ERRKEY = 'WTHFDI')
      PARAMETER (BLANK  = ' ')

      TYPE (ControlType) CONTROL

      RNMODE = CONTROL % RNMODE
      CALL YR_DOY (YRDOY,YEAR,DOY)
      IF (YEAR .LT. 100) THEN
        YR = YEAR
      ELSE
        YR = MOD(YEAR,100)
      ENDIF
!-----------------------------------------------------------------------
!     2020-10-11 CHP RNMODE = 'Y' indicates yield forecast mode. 
!      May need multiple weather files. 
!     If RNMODE = 'Y' and MEWTH = 'G','W','S', 
!      then also need a WTH file for forecast year weather data.
!     M = Measured daily, C = Measured CSV daily, H = Measured hourly 
!     G = generated data
!     S = interactively generated
!-----------------------------------------------------------------------
!     Generated weather data files
      IF (MEWTH .EQ. 'G') THEN
         IF (WSTA1(4:4) .EQ. BLANK) THEN
            WRITE (FILEWG(1:12),'(A4,I2.2,A6)') WSTA,YR,'01.WTG'
         ELSE
            WRITE (FILEWG(1:12),'(3A4)') WSTA,WSTA1,'.WTG'
         ENDIF
         PROCODG = 'WGD'
      ENDIF
!     Interactively generated weather 
      IF (MEWTH .EQ. 'S' .OR. MEWTH .EQ. 'W') THEN
         WRITE (FILEWC(1:12),'(A4,A8)') WSTA,'.CLI    '
         PROCODC = 'CLD'
      ENDIF
!     Measured weather data
      IF (MEWTH .EQ. 'M' .OR. MEWTH .EQ. 'H') THEN
         IF (WSTA1(4:4) .EQ. BLANK) THEN
           WRITE (FILEW(1:12),'(A4,I2.2,A6)') WSTA,YR,'01.WTH'
         ELSE
            WRITE(FILEW(1:12),'(3A4)') WSTA,WSTA1,'.WTH'
         ENDIF
         PROCODW = 'WED'
      ENDIF
      IF (MEWTH .EQ. 'C') THEN
         IF (WSTA1(4:4) .EQ. BLANK) THEN
           WRITE (FILEW(1:12),'(A4,I2.2,A6)') WSTA,YR,'01.CSV'
         ELSE
            WRITE(FILEW(1:12),'(3A4)') WSTA,WSTA1,'.CSV'
         ENDIF
         PROCODW = 'WED'
      ENDIF
      
      IF (INDEX('GSWM',RNMODE) .LT. 0) THEN
         CALL ERROR (ERRKEY,22,FILEX,LINEXP)
      ENDIF

!     Check for existing FILEW, FILEWC, and FILEWG
      DO I = 1, 3
        SELECT CASE (I)
          CASE (1)
            IF (INDEX('MCH', MEWTH) .GT. 0 .OR. RNMODE .EQ. 'Y') THEN
              FILEWTH = FILEW
              PROCOD = PROCODW
            ELSE
              CYCLE
            ENDIF
          CASE (2)
            IF (MEWTH .EQ. 'G') THEN
              FILEWTH = FILEWG
              PROCOD = PROCODG
            ELSE
              CYCLE
            ENDIF
          CASE (3)
            IF (MEWTH .EQ. 'S' .OR. MEWTH .EQ. 'W') THEN
              FILEWTH = FILEWC
              PROCOD = PROCODC
            ELSE
              CYCLE
            ENDIF
          CASE DEFAULT; CYCLE
        END SELECT

!       Check weather filename in current directory
        INQUIRE (FILE = FILEWTH,EXIST = FEXIST)
        IF (FEXIST) THEN
          PATHWT = BLANK
!       Check weather filename in data directory
        ELSE
          FILETMP = TRIM(PATHEX)//FILEWTH
          INQUIRE (FILE = FILETMP,EXIST = FEXIST)
          IF (FEXIST) THEN
            PATHWT = TRIM(PATHEX)
!         Check weather filename in default DSSAT directory
          ELSE
            CALL PATH(PROCOD,DSSATP,PATHWT,1,NAMEF)
            FILETMP = TRIM(PATHWT) // FILEWTH
            INQUIRE (FILE=FILETMP, EXIST = FEXIST)
            IF (FEXIST) THEN
              PATHWT = PATHWT
!           Check 4-character file name in data directory
            ELSE
              FILEW4 = FILEWTH(1:4) // FILEWTH(9:12)
              FILETMP = TRIM(PATHEX) // FILEW4
              INQUIRE (FILE=FILETMP, EXIST = FEXIST)
              IF (FEXIST) THEN
                PATHWT = TRIM(PATHEX)
                FILEWTH = FILEW4
!             Check 4-character filename in default DSSAT directory
              ELSE
                FILETMP = TRIM(PATHWT) // FILEWTH
                INQUIRE (FILE=FILETMP, EXIST = FEXIST)
                IF (FEXIST) THEN
                  PATHWT = PATHWT
                  FILEWTH = FILEW4
                ELSE
                  MSG(1) = "Weather file not found."
                  MSG(2) = "  Neither " // FILEWTH // " nor "//FILEW4
                  MSG(3) = 
     &              "  were found in weather or experiment directories."
                  MSG(4) = "Simulation will end."
                  CONTROL % ErrCode = 29
                  CALL PUT(CONTROL)
                  CALL WARNING(4,ERRKEY,MSG)
!                 CALL ERROR(ERRKEY,29,FILEW,0)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF

        SELECT CASE(I)
          CASE (1); FILEW  = FILEWTH; PATHWTW = PATHWT
          CASE (2); FILEWG = FILEWTH; PATHWTG = PATHWT
          CASE (3); FILEWC = FILEWTH; PATHWTC = PATHWT
        END SELECT
      ENDDO

      RETURN
      END SUBROUTINE WTHFDIR
      
!-----------------------------------------------------------------------
