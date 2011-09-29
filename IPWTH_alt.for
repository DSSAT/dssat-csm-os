C=======================================================================
C  IPWTH, Subroutine, N.B. Pickering, 08/30/91
C  Input weather data and check for errors.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  08/30/1991 NBP Written
C  07/01/1999 CHP Modified for modular format
C  09/02/1999 GH  Incorporated into CROPGRO
C  03/15/2000 GH  Modular model revisited
C  06/06/2002 GH  Modified for crop rotations
C  06/11/2002 GH  Modified for Y2K
C  08/12/2003 CHP Added warning messages for use of default TAMP and TAV
C  08/12/2003 CHP Added I/O error checking
C  01/14/2005 CHP Modified checks for TAMP, TAV
C  02/08/2005 CHP Added check for weather file name when first day of
C                   a sequence occurs on Jan 1.
!  08/02/2006 CHP Read and store weather data in arrays
!  08/25/2011 CHP Read vapor pressure (VAPR or VPRS) 
C-----------------------------------------------------------------------
C  Called by: WEATHR
C  Calls:     None
C=======================================================================

      SUBROUTINE IPWTH(CONTROL,
     &    CCO2, FILEW, FILEWW, MEWTH, PAR, PATHWT,        !Output
     &    RAIN, REFHT, RHUM, RSEED1, SRAD,                !Output
     &    TAMP, TAV, TDEW, TMAX, TMIN, VAPR, WINDHT,      !Output
     &    WINDSP, XELEV, XLAT, XLONG, YREND,              !Output
     &    DYNAMIC)

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
      IMPLICIT NONE
      SAVE

      CHARACTER*1  BLANK, MEWTH, RNMODE, UPCASE
      CHARACTER*4  INSI
      CHARACTER*6  SECTION, ERRKEY
      CHARACTER*8  WSTAT
      CHARACTER*10 TEXT
      CHARACTER*12 FILEW, LastFILEW
      CHARACTER*30 FILEIO
      CHARACTER*78 MSG(8)
      CHARACTER*80 PATHWT
      CHARACTER*92 FILEWW
      CHARACTER*120 LINE

      INTEGER DOY, DYNAMIC, ERR, ErrCode, FOUND, INCYD, ISIM
      INTEGER LINWTH, LNUM, LUNIO, LUNWTH, MULTI, NYEAR
      INTEGER PATHL, RSEED1, RUN, WYEAR
      INTEGER YEAR, YR, YRDOY, YRDOYW, YRDOYWY, YREND
      INTEGER YRSIM, YRSIMMY, YRDOY_WY

      INTEGER, PARAMETER :: MaxRecords = 11000   !5000

      REAL
     &  XELEV,PAR,RAIN,REFHT,SRAD,TAV,TAMP,TDEW,TMAX,TMIN,WINDHT,
     &  WINDSP,XLAT,XLONG,CCO2,RHUM, VAPR

      LOGICAL FEXIST, LongFile

      PARAMETER (ERRKEY = 'IPWTH ')
      PARAMETER (BLANK = ' ')

!     ============================================================
!     New stuff
!     Arrays of weather data stored.
      INTEGER, DIMENSION(MaxRecords) :: YRDOY_A, LineNumber
      REAL, DIMENSION(MaxRecords) :: SRAD_A, TMAX_A, TMIN_A, 
     &          RAIN_A, TDEW_A, WINDSP_A, PAR_A, RHUM_A, VAPR_A
      INTEGER CurrentWeatherYear, DOYW
      INTEGER I, J, LastRec, LastWeatherDay, NRecords
      INTEGER FirstWeatherDay, YEARW, RecNum

!     For free-format reads
!     Up to MAXCOL headers per line, up to 10 characters long each
      INTEGER, PARAMETER :: MAXCOL = 25
      CHARACTER*15  HEADER(MAXCOL), HTXT
!     COL keeps beginning and ending column for each header
      INTEGER COL(MAXCOL,2), ICOUNT, C1, C2, ISECT

      DATA LastFileW /" "/

C     The variable "CONTROL" is of constructed type "ControlType" as 
C     defined in ModuleDefs.for, and contains the following variables.
C     The components are copied into local variables for use here.
      TYPE (ControlType) CONTROL

      FILEIO = CONTROL % FILEIO  
      LUNIO  = CONTROL % LUNIO
      MULTI  = CONTROL % MULTI   
      RUN    = CONTROL % RUN    
      RNMODE = CONTROL % RNMODE  
      YRDOY  = CONTROL % YRDOY   
      YRSIM  = CONTROL % YRSIM   

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
!-----------------------------------------------------------------------
      IF (DYNAMIC == RUNINIT) THEN
!-----------------------------------------------------------------------
      IF (INDEX('FQ',RNMODE) <= 0 .OR. RUN. EQ. 1) THEN
        OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR /= 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
        READ (LUNIO,'(11(/),15X,A12,1X,A80)',IOSTAT=ERR) FILEW, PATHWT
        IF (ERR /= 0) CALL ERROR(ERRKEY,ERR,FILEIO,12)

        REWIND (LUNIO)
        SECTION = '*SIMUL'
        CALL FIND(LUNIO, SECTION, LNUM, FOUND)
        IF (FOUND == 0) CALL ERROR(SECTION, 42, FILEIO,LNUM)
        READ(LUNIO,'(41X,I5)',IOSTAT=ERR) RSEED1
        IF (ERR /= 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM+1)
        READ (LUNIO,'(/,19X,A1)',IOSTAT=ERR) MEWTH
        IF (ERR /= 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM+3)

        CLOSE (LUNIO)

        CALL GETLUN('FILEW', LUNWTH)

        IF (FILEW /= LastFileW) THEN
!          NRecords = 0
          LastWeatherDay  = 0
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC == SEASINIT) THEN
      
      ErrCode = 0

!-----------------------------------------------------------------------
!     Don't re-initialize for sequence and seasonal runs
      IF (INDEX('FQ',RNMODE) > 0 .AND. RUN > 1) RETURN

      OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR /= 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
      READ (LUNIO,'(11(/),15X,A12,1X,A80)',IOSTAT=ERR) FILEW, PATHWT
      IF (ERR /= 0) CALL ERROR(ERRKEY,ERR,FILEIO,12)
      CLOSE (LUNIO)

      WYEAR = (ICHAR(FILEW(5:5)) - 48)*10 + (ICHAR(FILEW(6:6)) - 48)
      NYEAR = (ICHAR(FILEW(7:7)) - 48)*10 + (ICHAR(FILEW(8:8)) - 48)
      IF (LastWeatherDay > FirstWeatherDay) THEN
        NYEAR = INT(LastWeatherDay)/1000 - INT(FirstWeatherDay)/1000 + 1
        NYEAR = MAX(1, NYEAR)
      ENDIF

      IF (NYEAR == 1 .AND. MULTI > 1) THEN
        PATHL  = INDEX(PATHWT,BLANK)
        WYEAR = MOD((WYEAR + MULTI - 1),100)
        WRITE(FILEW(5:6),'(I2.2)') WYEAR
        IF (PATHL <= 1) THEN
          FILEWW = FILEW
        ELSE
          FILEWW = PATHWT(1:(PATHL-1)) // FILEW
        ENDIF
        INQUIRE (FILE = FILEWW,EXIST = FEXIST)
        IF (.NOT. FEXIST) THEN  
          ErrCode = 29
          CALL WeatherError(CONTROL, ErrCode, FILEWW, 0, YRDOYWY, YREND)
          RETURN
        ENDIF
      ENDIF

!-----------------------------------------------------------------------
      CALL YR_DOY(YRSIM,YR,ISIM)

      PATHL  = INDEX(PATHWT,BLANK)
      IF (PATHL <= 1) THEN
        FILEWW = FILEW
      ELSE
        FILEWW = PATHWT(1:(PATHL-1)) // FILEW
      ENDIF

      IF (YRDOY == YRSIM) THEN
        YRDOY_WY = INCYD(YRSIM,-1)
      ENDIF

      IF (FILEW /= LastFileW) THEN
!       NRecords = 0
        YRDOY_WY = 0
        LastWeatherDay  = 0
        LINWTH = 0
        LongFile = .FALSE.

        INQUIRE(FILE=FILEWW,EXIST=FEXIST)
        IF (.NOT. FEXIST) THEN
          ErrCode = 30
          CALL WeatherError(CONTROL, ErrCode, FILEWW, 0, YRDOYWY, YREND)
          RETURN
        ENDIF
        OPEN (LUNWTH,FILE=FILEWW,STATUS='OLD',IOSTAT=ERR)
        IF (ERR /= 0) THEN
          ErrCode = 29
          CALL WeatherError(CONTROL, ErrCode, FILEWW, 0, YRDOYWY, YREND)
          RETURN
        ENDIF
        WSTAT = FILEW(1:8)
        CALL PUT('WEATHER','WSTA',WSTAT)

        INSI  = '-99 '
        XLAT  = -99.
        XLONG = -99.
        XELEV = -99.
        TAV   = -99.
        TAMP  = -99.
        REFHT = -99.
        WINDHT= -99.
        CCO2  = -99.

!       Look for 1st header line beginning with '@' in column 1 (ISECT = 3)
        DO WHILE (.TRUE.)   !.NOT. EOF(LUNWTH)
          CALL IGNORE2 (LUNWTH, LINWTH, ISECT, LINE)
          SELECT CASE(ISECT)
          CASE(0); CALL ERROR (ERRKEY,10,FILEW,LINWTH) !End of file 
          CASE(1); CALL ERROR (ERRKEY,10,FILEW,LINWTH) !Data record 
          CASE(2); CYCLE                               !End of section 
          CASE(3); EXIT                                !Header line 
          END SELECT
        ENDDO

!       Found header line for weather station data
        CALL PARSE_HEADERS(LINE, MAXCOL, HEADER, ICOUNT, COL)
        IF (ICOUNT .LT. 1) CALL ERROR (ERRKEY,10,FILEW,LINWTH)
        DO I = 1, ICOUNT
          HTXT = HEADER(I)
          DO J = 1, LEN(TRIM(HTXT))
            HTXT(J:J) = UPCASE(HTXT(J:J))
          END DO
          HEADER(I) = HTXT
        ENDDO

!       Read corresponding line of data 
        CALL IGNORE (LUNWTH, LINWTH, ISECT, LINE)
        IF (ISECT .NE. 1) CALL ERROR (ERRKEY,59,FILEW,LINWTH)

        DO I = 1, ICOUNT
          C1 = COL(I,1)
          C2 = COL(I,2)
          READ(LINE(C1:C2),'(A)',IOSTAT=ERR) TEXT
          IF (TEXT .NE. '          ') THEN
            SELECT CASE (TRIM(HEADER(I)))
              CASE('INSI')
                INSI = ADJUSTL(TEXT)

              CASE('LAT')
                READ(LINE(C1:C2),*,IOSTAT=ERR) XLAT
                IF (ERR .NE. 0) THEN
                  XLAT = 0.0
                  MSG(1) = 'Error reading latitude, value of zero'
     &              //  ' will be used.'
                  CALL WARNING(1, ERRKEY, MSG)
                ENDIF

              CASE('LONG')
                READ(LINE(C1:C2),*,IOSTAT=ERR) XLONG
                IF (ERR .NE. 0) XLONG = -99.0

              CASE('ELEV')
                READ(LINE(C1:C2),*,IOSTAT=ERR) XELEV
                IF (ERR .NE. 0) XELEV = -99.0

              CASE('TAV')
                READ(LINE(C1:C2),*,IOSTAT=ERR) TAV
                IF (ERR .NE. 0) TAV = -99.0

              CASE('AMP')
                READ(LINE(C1:C2),*,IOSTAT=ERR) TAMP
                IF (ERR .NE. 0) TAMP = -99.0

              CASE('REFHT')
                READ(LINE(C1:C2),*,IOSTAT=ERR) REFHT
                IF (ERR .NE. 0) REFHT = 1.5

              CASE('WNDHT')
                READ(LINE(C1:C2),*,IOSTAT=ERR) WINDHT
                IF (ERR .NE. 0) WINDHT = 2.0

              CASE('CCO2','CO2')
                READ(LINE(C1:C2),*,IOSTAT=ERR) CCO2
                IF (ERR .NE. 0) CCO2 = -99.0
            END SELECT
          ENDIF
        ENDDO

C       Substitute default values if REFHT or WINDHT are missing.
        IF (REFHT <= 0.) REFHT = 1.5
        IF (WINDHT <= 0.) WINDHT = 2.0

        LastFileW = FILEW
!       10/27/2005 CHP The checks for TAV and TAMP were being done in the 
!       STEMP routine, overriding this check. STEMP used .LE. instead 
!       of .LT. and the results were very different for some experiments 
!       which do not supply TAV and TAMP (UFMA8301.PNX, CLMO8501.SBX, 
!       NCCL8801.SBX, GALN0201.COX, others).
!       So -- leave LE for now.
        IF (TAV  .LE. 0.0) THEN       
!       IF (TAV  .LT. 0.0) THEN       
          TAV = 20.0
          WRITE(MSG(1), 100)
          WRITE(MSG(2), 120) TAV
          WRITE(MSG(3), 130)
  100     FORMAT
     &   ('Value of TAV, average annual soil temperature, is missing.')
  120     FORMAT('A default value of', F5.1, '�C is being used for ',
     &            'this simulation,')
  130     FORMAT('which may produce inaccurate results.')
          CALL WARNING (3, ERRKEY, MSG)
        ENDIF
        IF (TAMP .LE. 0.0) THEN
!       IF (TAMP .LT. 0.0) THEN
          TAMP = 5.0
          WRITE(MSG(1), 110)
  110     FORMAT('Value of TAMP, amplitude of soil temperature ',
     &            'function, is missing.')
          WRITE(MSG(2), 120) TAMP
          WRITE(MSG(3), 130)

          CALL WARNING (3, ERRKEY, MSG)
        ENDIF

!       -------------------------------------------------------------
!       Look for second header line beginning with '@' in column 1 (ISECT = 3)
        CALL IGNORE2 (LUNWTH, LINWTH, ISECT, LINE)
        IF (ISECT .EQ. 0) THEN        !End of file found
          CALL ERROR (ERRKEY,10,FILEW,LINWTH)
        ELSEIF (ISECT. EQ. 1) THEN    !Data record found
          CALL ERROR (ERRKEY,10,FILEW,LINWTH)
        ELSEIF (ISECT .EQ. 2) THEN    !End of section found
          CALL ERROR (ERRKEY,10,FILEW,LINWTH)
        ENDIF

!       Found header line for daily weather data
        CALL PARSE_HEADERS(LINE, MAXCOL, HEADER, ICOUNT, COL)
        IF (ICOUNT .LT. 1) CALL ERROR (ERRKEY,10,FILEW,LINWTH)
        CALL Check_Weather_Headers(
     &    COL, ICOUNT, FILEWW, HEADER, LINWTH)             !Input
!       ------------------------------------------------------------

        NRecords = 0

      ELSEIF (LongFile .AND. YRDOY < FirstWeatherDay) THEN
!       Starting over with long file -- same file, but need to read from top
        REWIND(LUNWTH)
  200   CONTINUE
        CALL IGNORE(LUNWTH,LINWTH,FOUND,LINE)
        IF (FOUND == 2) GO TO 200
        IF (FOUND == 0) CALL ERROR(ERRKEY,-1,FILEW,LINWTH)
        NRecords = 0

      ELSEIF (LongFile .AND. YRDOY > LastWeatherDay) THEN
!       Need to get next batch of records from long file
        NRecords = 0

!       Starting over, need to rewind 
        IF (MULTI == 1) THEN
          REWIND (LUNWTH)
  210     CONTINUE
          CALL IGNORE(LUNWTH,LINWTH,FOUND,LINE)
          IF (FOUND == 2) GO TO 210
          IF (FOUND == 0) CALL ERROR(ERRKEY,-1,FILEW,LINWTH)
        ENDIF
      ENDIF

      YRDOYWY = INCYD(YRSIM,-1)
      IF (MULTI > 1) THEN
        YRDOY_WY = YRDOYWY
      ELSE
        YRDOY_WY = 0
      ENDIF

      IF (NRecords == 0) THEN
        CALL IpWRec(CONTROL, MaxRecords,
     &    COL, ICOUNT, FILEWW, HEADER, LINWTH,             !Input
     &    LUNWTH, YRDOY_WY,                               !Input
     &    ErrCode, FirstWeatherDay, LastWeatherDay,       !Output
     &    LineNumber, LongFile, NRecords, PAR_A,          !Output
     &    RAIN_A, RHUM_A, SRAD_A, TDEW_A, TMAX_A,         !Output
     &    TMIN_A, VAPR_A, WINDSP_A, YRDOY_A, YREND)       !Output
        IF (ErrCode > 0) RETURN 
      ENDIF

!     Set weather values for initialization to the day before start of simulation
!     unless first weather day == start of simulation day
      IF (YRDOY_A(1) == YRDOY) THEN
        I = 1
        LastRec = 0
      ELSE
        DO I = 1, NRecords
          IF (YRDOY_A(I) == YRDOYWY) THEN
            LastRec = I
            EXIT
          ENDIF
        ENDDO
      ENDIF

      IF (I > NRecords) THEN
        I = NRecords
!        ErrCode = 10
!        CALL WeatherError(CONTROL,ErrCode,FILEWW,LINWTH, YRDOYWY, YREND)
!        RETURN
!       !CALL ERROR(ERRKEY,2,FILEW,LINWTH)
      ENDIF

      SRAD  = SRAD_A(I)
      TMAX  = TMAX_A(I)
      TMIN  = TMIN_A(I)
      RAIN  = RAIN_A(I)
      TDEW  = TDEW_A(I)
      WINDSP= WINDSP_A(I)
      PAR   = PAR_A(I)
      RHUM  = RHUM_A(I)
      VAPR  = VAPR_A(I)

!     Error checking
      CALL DailyWeatherCheck(CONTROL,
     &    "WTHINIT", FILEWW, RAIN, RecNum, RHUM,          !Input
     &    SRAD, TDEW, TMAX, TMIN, WINDSP, YRDOY,          !Input
     &    YREND)                                          !Output

      IF (YREND > 0) THEN
!       Try again with next weather day (only for initialization)
        SRAD  = SRAD_A(I+1)
        TMAX  = TMAX_A(I+1)
        TMIN  = TMIN_A(I+1)
        RAIN  = RAIN_A(I+1)
        TDEW  = TDEW_A(I+1)
        WINDSP= WINDSP_A(I+1)
        PAR   = PAR_A(I+1)
        RHUM  = RHUM_A(I+1)
        VAPR  = VAPR_A(I+1)
        YREND = -99
      
!       Error checking
        CALL DailyWeatherCheck(CONTROL,
     &    ERRKEY, FILEWW, RAIN, RecNum, RHUM,             !Input
     &    SRAD, TDEW, TMAX, TMIN, WINDSP, YRDOY,          !Input
     &    YREND)                                          !Output

      ENDIF

!***********************************************************************
!***********************************************************************
!     RATE (Daily input of weather data)
!***********************************************************************
      ELSEIF (DYNAMIC == RATE) THEN
!-----------------------------------------------------------------------
      IF (YRDOY > LastWeatherDay) THEN
!       StartReadDate = YRDOY
        YRSIMMY = INCYD(YRDOY,-1)
        CALL YR_DOY(YRDOY, YEAR, DOY)
        CurrentWeatherYear = YEAR

        IF (NYEAR <= 1 .AND. .NOT. LongFile) THEN
!         Open new weather file
          CLOSE(LUNWTH)
          WRITE(FILEW(5:6),'(I2.2)') MOD(CurrentWeatherYear,100)
          LINWTH = 0
          PATHL  = INDEX(PATHWT,BLANK)
          IF (PATHL <= 1) THEN
             FILEWW = FILEW
          ELSE
             FILEWW = PATHWT(1:(PATHL-1)) // FILEW
          ENDIF

          INQUIRE(FILE=FILEWW,EXIST=FEXIST)
          IF (.NOT. FEXIST) THEN
            ErrCode = 30
            CALL WeatherError(CONTROL, ErrCode, FILEWW, 0,YRDOYWY,YREND)
            RETURN
          ENDIF
      
          OPEN (LUNWTH,FILE=FILEWW,STATUS='OLD',IOSTAT=ERR)
          IF (ERR /= 0) THEN
            ErrCode = 29
            CALL WeatherError(CONTROL, ErrCode, FILEWW, 0,YRDOYWY,YREND)
            RETURN
          ENDIF
          WSTAT = FILEW(1:8)
          CALL PUT('WEATHER','WSTA',WSTAT)

C         Read in weather file header.
  500     CONTINUE
          CALL IGNORE(LUNWTH,LINWTH,FOUND,LINE)
          IF (FOUND == 2) GO TO 500
          IF (FOUND == 0) CALL ERROR(ERRKEY,-1,FILEW,LINWTH)

          IF (FILEW == LastFILEW) THEN
            IF(YRDOY > YRDOY_A(NRecords)) THEN
              ErrCode = 10
              CALL WeatherError(CONTROL, ErrCode, FILEWW,
     &                          LINWTH, YRDOY, YREND)
              RETURN
            ENDIF
          ENDIF
          LastFileW = FILEW
        ELSEIF (NYEAR > 1 .AND. .NOT. LongFile) THEN
!         Simulation goes beyond weather file data
          ErrCode = 10
          CALL WeatherError(CONTROL, ErrCode, FILEWW, LINWTH, 
     &      YRDOY, YREND)
          RETURN
        ELSEIF (LongFile) THEN
          YRDOYWY = LastWeatherDay
        ENDIF

        LastRec = 0

!     ---------------------------------------------------------
        IF (YRDOY == YRSIM) THEN
          YRDOYWY = INCYD(YRSIM,-1)
        ENDIF

!       Read in another batch of data
        CALL IpWRec(CONTROL, MaxRecords, 
     &    COL, ICOUNT, FILEWW, HEADER, LINWTH,             !Input
     &    LUNWTH, YRDOYWY,                                !Input
     &    ErrCode, FirstWeatherDay, LastWeatherDay,       !Output
     &    LineNumber, LongFile, NRecords, PAR_A,          !Output
     &    RAIN_A, RHUM_A, SRAD_A, TDEW_A, TMAX_A,         !Output
     &    TMIN_A, VAPR_A, WINDSP_A, YRDOY_A, YREND)       !Output
        IF (ErrCode > 0) RETURN 
      ENDIF

      YRDOYWY = INCYD(YRDOY,-1)
!     ---------------------------------------------------------
!     Retreive daily weather data from stored arrays
      DO I = LastRec+1, NRecords
        RecNum = LineNumber(I)    !Line number in weather file
        IF (YRDOY_A(I) /= YRDOY) THEN
          IF (YRDOY_A(I) < YRSIM) CYCLE

!         Check for sequential dates
          YRDOYW = YRDOY_A(I)
          CALL YR_DOY(YRDOYW, YEARW, DOYW)

          IF (YRDOYW <= YRDOYWY) THEN
!           Repeated date - ignore with warning
            WRITE(MSG(1),601) 
            WRITE(MSG(2),602) YEARW, DOYW
            WRITE(MSG(3),603) Trim(FILEWW)
            WRITE(MSG(4),604) 
  601       FORMAT("Duplicate record found in weather file.")
  602       FORMAT("Year ",I4," DOY ",I3)
  603       FORMAT("File: ",A)
  604       FORMAT("Record will be ignored.")
            CALL WARNING(4, ERRKEY, MSG)
            LastRec = I
            CYCLE

          ELSEIF (YRDOYW > INCYD(YRDOYWY,1)) THEN
            ErrCode = 10
            CALL WeatherError(CONTROL, ErrCode, FILEWW, RecNum, 
     &              INCYD(YRDOYWY,1), YREND)
            RETURN
           !CALL ERROR(ERRKEY,2,FILEW,RecNum)
          ENDIF
        ENDIF

!       Date OK, continue
        YRDOYWY = YRDOY

        SRAD   = SRAD_A(I)  
        TMAX   = TMAX_A(I)  
        TMIN   = TMIN_A(I)  
        RAIN   = RAIN_A(I)  
        TDEW   = TDEW_A(I)  
        WINDSP = WINDSP_A(I)
        PAR    = PAR_A(I)   
        RHUM   = RHUM_A(I) 
        VAPR   = VAPR_A(I)

        LastRec = I
        EXIT
      ENDDO

!     Error checking
      CALL DailyWeatherCheck(CONTROL,
     &    ERRKEY, FILEWW, RAIN, RecNum, RHUM,             !Input
     &    SRAD, TDEW, TMAX, TMIN, WINDSP, YRDOY,          !Input
     &    YREND)                                          !Output

!      ERR = 0
!      IF (SRAD < 0.) ErrCode = 2
!      IF (RAIN < 0.) ErrCode = 3
!      IF (NINT(TMAX * 100.) == 0 .AND. NINT(TMIN * 100.) == 0)
!     &  ErrCode = 4
!      IF (ABS(TMAX - TMIN) < 0.005) ErrCode = 5
!      IF (TMAX < TMIN) ErrCode = 6
!
!      IF (ErrCode > 0) THEN
!        MSG(1) = "Error in weather data:"
!        WRITE(MSG(2),'(A)') FILEWW(1:78)
!        WRITE(MSG(3),'(A,I4)') "Line ",RecNum
!        WRITE(MSG(4),'("SRAD = ",F6.2)') SRAD 
!        WRITE(MSG(5),'("TMAX = ",F6.2)') TMAX  
!        WRITE(MSG(6),'("TMIN = ",F6.2)') TMIN  
!        WRITE(MSG(7),'("RAIN = ",F6.2)') RAIN
!        MSG(8) = "This run will stop."
!        CALL WARNING(8,ERRKEY,MSG)
!!       CALL ERROR(ERRKEY,ERR,FILEW,RecNum)
!        CALL WeatherError(CONTROL, ErrCode, FILEWW, 
!     &                  LINWTH, YRDOYWY,YREND)
!        RETURN
!      ENDIF
!
!!     Substitute default values if TDEW or WINDSP are missing.
!      IF (TDEW <= -90.)  THEN 
!c               MJ, 2007-04-05: set TDEW to TMIN if TDEW not otherwise available.  This is not
!c               appropriate to South African (and presumably other) conditions
!c               --> suggest replacing with a better calculation based on relative humidity, if
!c                   available.
!          IF (RHUM .GT. 0.01) THEN
!              TDEW = CALC_TDEW(TMIN, RHUM)
!          ELSE
!             TDEW = TMIN
!          ENDIF
!      ENDIF
!      IF (WINDSP <= 0.) WINDSP = 86.4
!!      IF (WINDSP <= -1.E-6) THEN
!!        WINDSP = 86.4
!!      ELSEIF (WINDSP < 1.0) THEN
!!        MSG(1) = "Unlikely value for WINDSP in weather file."
!!        WRITE(MSG(2),'("WINDSP = ",F8.2," km/d")') WINDSP
!!        CALL WARNING(2,ERRKEY,MSG)
!!        CALL ERROR(ERRKEY,9,FILEW,RecNum)
!!      ENDIF

      IF (I > NRecords) THEN
        ErrCode = 64
        CALL WeatherError(CONTROL, ErrCode, FILEWW, 
     &                  LINWTH, YRDOY, YREND)
        RETURN
      ENDIF

!***********************************************************************
!***********************************************************************
!     SEASEND
!***********************************************************************
      ELSEIF (DYNAMIC == SEASEND) THEN
!-----------------------------------------------------------------------
!     CHP 8/2/2006 Removed close stmt.  
!     New weather routine can pick up where it left off if using
!     Long format weather files (i.e., more than one year in a file).

    !  CLOSE (LUNWTH)
      INQUIRE(UNIT=LUNWTH,OPENED=FEXIST)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE IPWTH
!=======================================================================


!=======================================================================
!  IpWRec, Subroutine, C.H. Porter, 08/10/2006
!  Read weather records into arrays
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  08/10/2006 CHP Written
!-----------------------------------------------------------------------
!  Called by: IPWTH_alt
!  Calls:     None
!=======================================================================

      SUBROUTINE IpWRec(CONTROL, MaxRecords,
     &    COL, ICOUNT, FILEWW, HEADER, LINWTH,             !Input
     &    LUNWTH, YRDOYWY,                                !Input
     &    ErrCode, FirstWeatherDay, LastWeatherDay,       !Output
     &    LineNumber, LongFile, NRecords, PAR_A,          !Output
     &    RAIN_A, RHUM_A, SRAD_A, TDEW_A, TMAX_A,         !Output
     &    TMIN_A, VAPR_A, WINDSP_A, YRDOY_A, YREND)       !Output

!-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      SAVE

      INTEGER MaxRecords

      TYPE (ControlType) CONTROL
      CHARACTER*1  RNMODE
      CHARACTER*6, PARAMETER :: ERRKEY = "IPWTH "
      CHARACTER*78 MSG(2)
      CHARACTER*120 LINE  
      CHARACTER*92 FILEWW

      INTEGER CENTURY, ERR, ErrCode, FOUND, LINWTH, LUNWTH, MULTI, RUN  
      INTEGER YRDOY, YRDOYW, YRDOYWY, YRDOY_start, YREND, YRSIM
      INTEGER YRDOYW_SAVE

      REAL PAR, RAIN, SRAD, TDEW, TMAX, TMIN, WINDSP, RHUM, VAPR

      LOGICAL LongFile

!     Arrays of weather data -- up to one year stored.
      INTEGER, DIMENSION(MaxRecords) :: YRDOY_A, LineNumber
      REAL, DIMENSION(MaxRecords) :: SRAD_A, TMAX_A, TMIN_A, 
     &          RAIN_A, TDEW_A, WINDSP_A, PAR_A, RHUM_A, VAPR_A
      INTEGER LastRec, LastWeatherDay, NRecords
      INTEGER FirstWeatherDay

!     For free-format reads
!     Up to MAXCOL headers per line, up to 10 characters long each
      INTEGER, PARAMETER :: MAXCOL = 25
      CHARACTER*15  HEADER(MAXCOL)
!     COL keeps beginning and ending column for each header
      INTEGER COL(MAXCOL,2), ICOUNT, C1, C2, I

      MULTI  = CONTROL % MULTI
      RNMODE = CONTROL % RNMODE
      RUN    = CONTROL % RUN
      YRDOY  = CONTROL % YRDOY
      YRSIM  = CONTROL % YRSIM

!-----------------------------------------------------------------------
!     LongFile = .FALSE.
      YRDOY_start = CONTROL % YRDOY
!-----------------------------------------------------------------------
!     Begin reading daily weather data
 100  NRecords = 0

!     zero out arrays
      YRDOY_A  = 0
      SRAD_A   = 0.0
      TMAX_A   = 0.0
      TMIN_A   = 0.0
      RAIN_A   = 0.0
      TDEW_A   = 0.0
      WINDSP_A = 0.0
      PAR_A    = 0.0
      RHUM_A   = 0.0
      VAPR_A   = 0.0

      CENTURY = INT(YRSIM / 100000.)

      DO WHILE (.TRUE.)   !.NOT. EOF(LUNWTH)
!       Read array of weather records for this calendar year 
!       starting with simulation start date and ending at end 
!       of file or at MaxRecords # of records
        CALL IGNORE(LUNWTH,LINWTH,FOUND,LINE)
        IF (FOUND == 1) THEN

          SRAD  = -99.
          TMAX  = -99.
          TMIN  = -99. 
          RAIN  = -99. 
          TDEW  = -99. 
          WINDSP= -99. 
          PAR   = -99. 
          RHUM  = -99. 
          VAPR  = -99.

!         Use free format reads
!          READ (LINE,RECFMT,IOSTAT=ERR) YRDOYW,SRAD,TMAX,TMIN,
!     &           RAIN,TDEW,WINDSP,PAR,RHUM
!          IF (ERR /= 0) CALL ERROR(ERRKEY,ERR,FILEW,LINWTH)

!         CHP 1/2/2008
!         Require date in col 1-5 regardless of header
          READ (LINE,'(I5)',IOSTAT=ERR) YRDOYW
          IF (ERR /= 0) THEN
            ErrCode = 59
            CALL WeatherError(CONTROL, ErrCode, FILEWW, 
     &                  LINWTH, YRDOYW, YREND)
            RETURN
          ENDIF

          DO I = 2, ICOUNT
            C1 = COL(I,1)
            C2 = COL(I,2)
            SELECT CASE (TRIM(HEADER(I)))
              !CASE('DATE')
              !  READ(LINE(C1:C2),*,IOSTAT=ERR) YRDOYW
              !  IF (ERR .NE. 0) EXIT

              CASE('SRAD')  !Solar radiation MJ/m2/d
                READ(LINE(C1:C2),*,IOSTAT=ERR) SRAD
                IF (ERR .NE. 0) SRAD = -99.

              CASE('TMAX')  !Max daily temperature (C)
                READ(LINE(C1:C2),*,IOSTAT=ERR) TMAX
                IF (ERR .NE. 0) TMAX = -99.

              CASE('TMIN')  !Min daily temperature (C)
                READ(LINE(C1:C2),*,IOSTAT=ERR) TMIN
                IF (ERR .NE. 0) TMIN = -99.

              CASE('RAIN')  !Daily precip (mm)
                READ(LINE(C1:C2),*,IOSTAT=ERR) RAIN
                IF (ERR .NE. 0) RAIN = -99.

              CASE('DEWP', 'TDEW')  !Dewpoint temp (C)
                READ(LINE(C1:C2),*,IOSTAT=ERR) TDEW
                IF (ERR .NE. 0) TDEW = -99.0

              CASE('WIND')  !Daily wind run (km/d)
                READ(LINE(C1:C2),*,IOSTAT=ERR) WINDSP
                IF (ERR .NE. 0) WINDSP = -99.0

              CASE('PAR')   
!               Photosynthetically active radiation (Einstein/m2/day)
                READ(LINE(C1:C2),*,IOSTAT=ERR) PAR
                IF (ERR .NE. 0) PAR = -99.0

              CASE('RHUM')  
!               Relative humidity at TMIN (or max rel. hum) (%)
                READ(LINE(C1:C2),*,IOSTAT=ERR) RHUM
                IF (ERR .NE. 0) RHUM = -99.0

              CASE('VAPR','VPRS')   !Vapor pressure (kPa)
                READ(LINE(C1:C2),*,IOSTAT=ERR) VAPR
                IF (ERR .NE. 0) VAPR = -99.0
            END SELECT
          ENDDO

          YRDOYW_SAVE = YRDOYW
          CALL Y2K_DOYW(MULTI, YRDOYWY, YRDOYW, CENTURY)
          IF (NRecords == 0 .AND. YRDOY == YRSIM .AND.  !First record
     &        YRDOYW > YRSIM .AND.                      ! > YRSIM
     &        YRDOYW_SAVE < 99366) THEN       ! & century set by program
              CENTURY = CENTURY - 1
            YRDOYW = YRDOYW - 100000
          ENDIF

          YRDOYWY = YRDOYW
          IF (NRecords == 0) FirstWeatherDay = YRDOYW
!         Store record in arrays
!         Data will be checked for errors when used.  May have errors
!         in records which are after harvest dates.
          NRecords = NRecords + 1
          YRDOY_A(NRecords) = YRDOYW
          SRAD_A(NRecords)  = SRAD
          TMAX_A(NRecords)  = TMAX
          TMIN_A(NRecords)  = TMIN
          RAIN_A(NRecords)  = RAIN
          TDEW_A(NRecords)  = TDEW
          WINDSP_A(NRecords)= WINDSP
          PAR_A(NRecords)   = PAR
          RHUM_A(NRecords)  = RHUM
          VAPR_A(NRecords)  = VAPR
          LineNumber(NRecords) = LINWTH

          IF (NRecords > 1) THEN
            IF (YRDOY_A(NRecords) < YRDOY_A(NRecords-1)) THEN
              ErrCode = 8
              WRITE(MSG(1),'("Record",I7,I8)') NRecords-1,
     &          YRDOY_A(NRecords-1)
              WRITE(MSG(2),'("Record",I7,I8)')NRecords,YRDOY_A(NRecords)
              CALL WARNING(2,ERRKEY,MSG)
              CALL WeatherError(CONTROL, ErrCode, FILEWW, 
     &                  LINWTH, YRDOYW, YREND)
              RETURN
             !CALL ERROR(ERRKEY,8,FILEWW,LINWTH)
            ENDIF 
          ENDIF

          IF (NRecords == MaxRecords) THEN
            LastWeatherDay = YRDOYW
            LongFile = .TRUE.
            IF (YRDOYW < YRDOY_start) THEN
!             Beginning of simulation is after (MaxRecords) number
!               of records.  Start over.
              GOTO 100
            ELSE
              EXIT
            ENDIF
          ENDIF
        ELSE
          LastWeatherDay = YRDOYW
          EXIT  
        ENDIF
      ENDDO
      !IF (EOF(LUNWTH)) LastWeatherDay = YRDOYW
      LastRec = 0

      RETURN
      End Subroutine IpWRec
!=======================================================================

!=======================================================================
      SUBROUTINE Check_Weather_Headers(
     &    COL, ICOUNT, FILEWW, HEADER, LINWTH)             !Input

!     Checks that required headers are found in weather file.  Reports
!     to INFO.OUT the headers that are found.
!-----------------------------------------------------------------------
      CHARACTER*1 UPCASE
      CHARACTER*92 FILEWW
      INTEGER IM, LINWTH
      CHARACTER*6, PARAMETER :: ERRKEY = 'IPWTH '

!     For free-format reads
!     Up to MAXCOL headers per line, up to 10 characters long each
      INTEGER, PARAMETER :: MAXCOL = 25
      CHARACTER*15  HEADER(MAXCOL), HTXT
!     COL keeps beginning and ending column for each header
      INTEGER COL(MAXCOL,2), ICOUNT, I, J
      CHARACTER*78 MSG(MAXCOL+1)

      INTEGER, PARAMETER :: NumRqdHeaders = 4
      LOGICAL FOUND_Headers(NumRqdHeaders) !verification of minimum data
      CHARACTER*15 Rqd_Headers(NumRqdHeaders)

      DATA Rqd_Headers /'SRAD', 'TMAX', 'TMIN', 'RAIN'/

      FOUND_Headers = .FALSE.

!     Report acceptable headers to INFO.OUT file.  This list should match that
!       in the read section below.
      WRITE(MSG(1),'(A)') FILEWW(1:78)
      WRITE(MSG(2),'("Found daily weather data headers for:")') 
      IM = 2
      DO I = 1, ICOUNT

!       Change headers to all caps
        HTXT = HEADER(I)
        DO J = 1, LEN(TRIM(HTXT))
          HTXT(J:J) = UPCASE(HTXT(J:J))
        END DO
        HEADER(I) = HTXT

!       Print confirmation that header was found to INFO.OUT
        SELECT CASE (TRIM(HEADER(I)))
          CASE('SRAD','TMAX','TMIN','RAIN','DEWP','TDEW','WIND',
     &        'PAR','RHUM','VAPR')
            IM = IM + 1
            WRITE(MSG(IM),'(2X,A15,"Col ",I3," - ",I3)') 
     &          HEADER(I), COL(I,1), COL(I,2)
        END SELECT

!       Check that headers are found for minimum required data
        DO J = 1, NumRqdHeaders
          IF (TRIM(HEADER(I)) == TRIM(Rqd_Headers(J))) THEN
            FOUND_Headers(J) = .TRUE.
            EXIT
          ENDIF
        ENDDO

      ENDDO

      CALL INFO(IM,ERRKEY,MSG)

      DO I = 1, NumRqdHeaders
        IF (.NOT. Found_Headers(I)) THEN
          MSG(1) = TRIM(Rqd_Headers(I)) // 
     &                " data not found in weather file:"
          MSG(2) = TRIM(FILEWW)
          CALL WARNING(2,ERRKEY,MSG)
          CALL ERROR(ERRKEY,10,FILEWW,LINWTH)
        ENDIF
      ENDDO

      RETURN
      END SUBROUTINE Check_Weather_Headers

!=======================================================================


!=======================================================================
! DailyWeatherCheck, Subroutine, C.H. Porter, 09/01/2009
! Checks daily weather data, observed, generated or modified.

!-----------------------------------------------------------------------
! REVISION HISTORY
! 09/01/2009 CHP Written
!-----------------------------------------------------------------------
      Subroutine DailyWeatherCheck(CONTROL,
     &    ERRKEY, FILEWW, RAIN, RecNum, RHUM,             !Input
     &    SRAD, TDEW, TMAX, TMIN, WINDSP, YRDOYW,         !Input
     &    YREND)                                          !Output

!     Checks validity of daily weather for observed or generated values.
!     ------------------------------------------------------------------
      Use ModuleDefs
      Use ModuleData
      Implicit None

      CHARACTER*(*) ERRKEY, FILEWW
      CHARACTER*78 MSG(10)
      Integer ErrCode, NChar, RecNum, YRDOYW, YREND
      REAL RAIN, RHUM, SRAD, TDEW, TMAX, TMIN, WINDSP
      REAL CALC_TDEW
      TYPE (ControlType) CONTROL

!     Error checking
      ErrCode = 0
      IF (SRAD < 1.E-2) ErrCode = 2
      IF (RAIN < 0.) ErrCode = 3
      IF (NINT(TMAX * 100.) == 0 .AND. NINT(TMIN * 100.) == 0)
     &  ErrCode = 4
      IF (TMAX < TMIN) THEN 
        ErrCode = 6
      ELSEIF (TMAX - TMIN < 0.05) THEN
        ErrCode = 5
      ENDIF

      IF (ErrCode > 0) THEN
!       For seasonal initialization, try next weather day
!       ERRKEY="INIT" indicates using weather for day before start 
!         of simulation.  YREND = ErrCode indicates that error was
!         found in the data.
        IF (INDEX(ERRKEY,"INIT") > 0) THEN
          YREND = ErrCode  !
          MSG(1) = "Error in weather data on day before " // 
     &      "start of simulation date."
          MSG(2) = "Will attempt to intitialize with data " // 
     &      "for start of simulation date."
          CALL WARNING(2,ERRKEY,MSG)
          RETURN
        ENDIF
        MSG(1) = "Error in weather data:"
        NChar = MIN(78,LEN_Trim(FILEWW))
        WRITE(MSG(2),'(A)') FILEWW(1:NChar)
        WRITE(MSG(3),'(A,I4)') "Line ", RecNum
        WRITE(MSG(4),'("SRAD = ",F6.2)') SRAD 
        WRITE(MSG(5),'("TMAX = ",F6.2)') TMAX  
        WRITE(MSG(6),'("TMIN = ",F6.2)') TMIN  
        WRITE(MSG(7),'("RAIN = ",F6.2)') RAIN
        MSG(8) = "This run will stop."
        CALL WARNING(8,ERRKEY,MSG)

        IF (ERRKEY == 'WTHMOD') ErrCode = ErrCode + 70
        IF (ERRKEY == 'WGEN  ') ErrCode = ErrCode + 80  
      
        CALL WeatherError(CONTROL, ErrCode, FILEWW, 
     &                  RecNum, YRDOYW, YREND)
        RETURN
      ENDIF

!     Warnings: issue message, but do not end simulation
      IF (SRAD < 1.0) THEN
        MSG(1) = "Warning: SRAD < 1"
        NChar = MIN(78,LEN_Trim(FILEWW))
        WRITE(MSG(2),'(A)') FILEWW(1:NChar)
        WRITE(MSG(3),'(A,I8)') "Line ", RecNum
        WRITE(MSG(4),'("SRAD = ",F6.2)') SRAD
        CALL WARNING(4,ERRKEY,MSG) 
      ENDIF

!     Substitute default values if TDEW or WINDSP are missing.
      IF (TDEW <= -90.)  THEN 
c               MJ, 2007-04-05: set TDEW to TMIN if TDEW not otherwise available.  This is not
c               appropriate to South African (and presumably other) conditions
c               --> suggest replacing with a better calculation based on relative humidity, if
c                   available.
          IF (RHUM .GT. 0.01) THEN
              TDEW = CALC_TDEW(TMIN, RHUM)
          ELSE
             TDEW = TMIN
          ENDIF
      ENDIF

!      IF (WINDSP <= 0.) WINDSP = 86.4
!      IF (WINDSP <= -1.E-6) THEN
!        WINDSP = 86.4
!      ELSEIF (WINDSP < 1.0) THEN
!        MSG(1) = "Unlikely value for WINDSP in weather file."
!        WRITE(MSG(2),'("WINDSP = ",F8.2," km/d")') WINDSP
!        CALL WARNING(2,ERRKEY,MSG)
!        CALL ERROR(ERRKEY,9,FILEW,RecNum)
!      ENDIF

      Return
      End Subroutine DailyWeatherCheck

!=======================================================================


!=======================================================================
! WeatherError, Subroutine, C.H. Porter, 09/01/2009
! Ends a batch run for errors in weather data, but continue with next
!     simulation in batch.  Stops sequence simulation.

!-----------------------------------------------------------------------
! REVISION HISTORY
! 09/01/2009 CHP Written
!-----------------------------------------------------------------------
      SUBROUTINE WeatherError(CONTROL, ErrCode, FILEWW, LNUM, 
     &      YRDOYW, YREND)

      USE ModuleDefs
      USE ModuleData
      IMPLICIT NONE

      CHARACTER*6, PARAMETER :: ERRKEY = 'IPWTH '
      CHARACTER*78 MSG(4)
      CHARACTER*92 FILEWW

      INTEGER DOYY, ErrCode, I, LNUM, YRDOYW, YREND, YRY
      INTEGER LenString, NCHAR, NMSG
      TYPE (ControlType) CONTROL

!-----------------------------------------------------------------------
      CALL YR_DOY(YRDOYW, YRY, DOYY)
      SELECT CASE(ErrCode)
!       Weather data read from file
        CASE (1); MSG(1)="Header section not found in weather file."
        CASE (2); WRITE(MSG(1),'(A,I5,I4)') 
     &      "Solar radiation data error on YR DOY: ", YRY, DOYY
        CASE (3); WRITE(MSG(1),'(A,I5,I4)') 
     &      "Precipitation data error on YR DOY: ", YRY, DOYY
        CASE (4); WRITE(MSG(1),'(A,I5,I4)') 
     &      "Tmax and Tmin are both set to 0 on YR DOY: ", YRY, DOYY
        CASE (5); WRITE(MSG(1),'(A,I5,I4)') 
     &      "Tmax and Tmin have identical values on YR DOY: ", YRY, DOYY
        CASE (6); WRITE(MSG(1),'(A,I5,I4)') 
     &      "Tmax is less than Tmin on YR DOY: ", YRY, DOYY
        CASE (8); WRITE(MSG(1),'(A,I5,I4)') 
     &      "Non-sequential data in file: ", YRY, DOYY
        CASE (10); WRITE(MSG(1),'(A,I5,I4)')
     &      "Weather record not found for YR DOY:",YRY, DOYY
        CASE (29); MSG(1) = "Weather file not found."
        CASE (30); MSG(1) = "Error opening weather file."
        CASE (59); MSG(1) = "Invalid format in weather file."
        CASE (64); MSG(1) = "Syntax error."

!       Weather modification
        CASE (72); WRITE(MSG(1),'(A,I5,I4)') 
     &      "Solar radiation data error on YR DOY: ", YRY, DOYY
        CASE (73); WRITE(MSG(1),'(A,I5,I4)') 
     &      "Precipitation data error on YR DOY: ", YRY, DOYY
        CASE (74); WRITE(MSG(1),'(A,I5,I4)') 
     &      "Tmax and Tmin are both set to 0 on YR DOY: ", YRY, DOYY
        CASE (75); WRITE(MSG(1),'(A,I5,I4)') 
     &      "Tmax and Tmin have identical values on YR DOY: ", YRY, DOYY
        CASE (76); WRITE(MSG(1),'(A,I5,I4)') 
     &      "Tmax is less than Tmin on YR DOY: ", YRY, DOYY

!       Generated weather data
        CASE (82); WRITE(MSG(1),'(A,I5,I4)') 
     &      "Solar radiation data error on YR DOY: ", YRY, DOYY
        CASE (83); WRITE(MSG(1),'(A,I5,I4)') 
     &      "Precipitation data error on YR DOY: ", YRY, DOYY
        CASE (84); WRITE(MSG(1),'(A,I5,I4)') 
     &      "Tmax and Tmin are both set to 0 on YR DOY: ", YRY, DOYY
        CASE (85); WRITE(MSG(1),'(A,I5,I4)') 
     &      "Tmax and Tmin have identical values on YR DOY: ", YRY, DOYY
        CASE (86); WRITE(MSG(1),'(A,I5,I4)') 
     &      "Tmax is less than Tmin on YR DOY: ", YRY, DOYY

      END SELECT

      NMSG = 3
      IF (ErrCode > 80) THEN
        MSG(2) = "Generated weather data"
        NMSG = 4
      ELSEIF (ErrCode > 70) THEN
        MSG(2) = "Modified weather data"
        NMSG = 4
      ENDIF

      NCHAR = LenString(FILEWW)
      NCHAR = MIN(76,NCHAR)
      WRITE(MSG(NMSG-1),'(2X,A)') FILEWW(1:NCHAR)

      MSG(NMSG) = "Simulation will end."
      CALL WARNING(NMSG,ERRKEY,MSG)
      YREND = CONTROL%YRDOY
      CONTROL % ErrCode = ErrCode
      CALL PUT(CONTROL)

!!     Stop the run for these errors:
!      SELECT CASE(ErrCode)
!        CASE (29,30)
!        CALL ERROR(ERRKEY,ErrCode,FILEWW(I-11:I),0)
!      END SELECT

      IF (INDEX('FQ',CONTROL%RNMODE) > 0) THEN
        I = LEN_TRIM(FILEWW)
        CALL ERROR(ERRKEY,ErrCode,FILEWW(I-11:I),LNUM)
      ENDIF

      RETURN
      END SUBROUTINE WeatherError
!=======================================================================

!=======================================================================
! IPWTH Variables

!-----------------------------------------------------------------------
! BLANK   blank character 
! CALC_TDEW Function that calculates dew point temperature from min and
!           max temperatures and relative humidity.
! CCO2    Atmospheric CO2 concentration read from input file (ppm)
! ERRKEY  Subroutine name for error file 
! ERR  Error number for input 
! FILEIO  Filename for input file (e.g., IBSNAT35.INP) 
! FILEW   Weather data file 
! FILEWW  Pathname plus filename for weather file (e.g. UFGA7801.WTH) 
! FIRST   Indicates first call to subroutine (true or false) 
! FOUND   Indicator that good data was read from file by subroutine FIND  
!           (0- End-of-file encountered, 1 - NAME was found) 
! INCYD   Function subroutine increases/decreases date (YRDOY) 
!           based on variable INC. 
! INSI    Location code for weather data 
! ISIM    Day portion of Julian date 
! LINE    Record of data read from file 
! LINWTH  Current line read from weather file 
! LNUM    Current line number of input file 
! LUNIO   Logical unit number for FILEIO 
! LUNWTH  Unit number for weather file 
! MEWTH   Switch for method of obtaining weather data-- 'G' or 'M'- read 
!           weather data file 'S'- read SIMMETEO inputs and generate 
!           weather data 'W'- read WGEN inputs and generate weather data 
! MULTI   Current simulation year (=1 for first or single simulation, =NYRS 
!           for last seasonal simulation) 
! NYEAR   Numeric value of 7th and 8th characters in FILEW, i.e.,  
! PAR     Daily photosynthetically active radiation or photon flux density
!           (moles[quanta]/m2-d)
! PATHL   Number of characters in path name (path plus filename for FILEC) 
! PATHWT  Directory path for weather file 
! RAIN    Precipitation depth for current day (mm)
! REFHT   Reference height for wind speed (m)
! RHUM    Relative humidity (%)
! RSEED1  Random number generator seed- user input 
! SECTION Section name in input file 
! SRAD    Solar radiation (MJ/m2-d)
! TAMP    Amplitude of temperature function used to calculate soil 
!           temperatures (�C)
! TAV     Average annual soil temperature, used with TAMP to calculate soil 
!           temperature. (�C)
! TDEW    Dewpoint temperature (�C)
! TIMDIF  Integer function which calculates the number of days between two 
!           Julian dates (da)
! TMAX    Maximum daily temperature (�C)
! TMIN    Minimum daily temperature (�C)
! WINDHT  Reference height for wind speed (m)
! WINDSP  Wind speed (km/d)
! WYEAR   Weather year; current year for weather data 
! XELEV   Field elevation (not used) (m)
! XLAT    Latitude (deg.)
! XLONG   Longitude (deg.)
! YR      Year portion of date 
! YR_DOY  Function subroutoine converts date in YYDDD format to integer 
!           year (YY) and day (DDD). 
! YRDOY   Current day of simulation (YYDDD)
! YRDOYW  Julian date read from weather file (YYDDD)
! YRDOYY  Date read from weather file yesterday (YYDDD)
! YRSIM   Start of simulation date (YYDDD)
! YRSIMM  Beginning date of simulation (YYDDD)
! YRSIMMY Day before simulation date (YYDDD format) 
! YRSIMY  Day before simulation date (YYDDD format) 
!=======================================================================

