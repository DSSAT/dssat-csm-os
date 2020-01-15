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
!  07/25/2014 CHP Added daily CO2 read from weather file (DCO2)
!  05/11/2017 WP  FlexibleIO for Weather Input Reader in CPP (Author: Felipe Vargas)
!  12/11/2017 WP/FO Added variable to return error code in readweather subroutine.
!  12/04/2017 WP/FO Added an argument in FlexibleIO function calls to the new data structure
!  08/10/2018 WP/FO Updated IPWTH subroutine for weather input
C-----------------------------------------------------------------------
C  Called by: WEATHR
C  Calls:     None
C=======================================================================

      SUBROUTINE IPWTH(CONTROL,
     &    CCO2, DCO2, FILEW, FILEWW, MEWTH, PAR, PATHWT,  !Output
     &    RAIN, REFHT, RHUM, RSEED1, SRAD,                !Output
     &    TAMP, TAV, TDEW, TMAX, TMIN, VAPR, WINDHT,      !Output
     &    WINDSP, XELEV, XLAT, XLONG, YREND,              !Output
     &    DYNAMIC)

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
!-----------------------------------------------------------------------
! FlexibleIO
!-----------------------------------------------------------------------
      USE flexibleio
!-----------------------------------------------------------------------

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
      CHARACTER*15 ERRYRDOY

      INTEGER EOF
      INTEGER DOY, DYNAMIC, ERR, ErrCode, FOUND, INCYD, ISIM
      INTEGER LINWTH, LNUM, LUNIO, LUNWTH, MULTI, NYEAR
      INTEGER PATHL, RSEED1, RUN, WYEAR
      INTEGER YEAR, YR, YRDOY, YRDOYW, YRDOYWY, YREND
      INTEGER YRSIM, YRSIMMY, YRDOY_WY

!      INTEGER, PARAMETER :: MaxRecords = 11000   !5000

      REAL
     &  XELEV,PAR,RAIN,REFHT,SRAD,TAV,TAMP,TDEW,TMAX,TMIN,WINDHT,
     &  WINDSP,XLAT,XLONG,CCO2,RHUM, VAPR, DCO2

      LOGICAL FEXIST, LongFile

      PARAMETER (ERRKEY = 'IPWTH ')
      PARAMETER (BLANK = ' ')

!     ============================================================
!     New stuff
!     Arrays of weather data stored.
!      INTEGER, DIMENSION(MaxRecords) :: YRDOY_A, LineNumber
!      REAL, DIMENSION(MaxRecords) :: SRAD_A, TMAX_A, TMIN_A, 
!     &          RAIN_A, TDEW_A, WINDSP_A, PAR_A, RHUM_A, VAPR_A
!     &        , DCO2_A
      INTEGER CurrentWeatherYear, DOYW
      INTEGER I, J, LastRec, LastWeatherDay, NRECORDS
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
!     Do not re-initialize for sequence and seasonal runs
      IF (INDEX('FQ',RNMODE) > 0 .AND. RUN > 1) RETURN

!-----------------------------------------------------------------------
! Open FILEIO and read FILEW and PATHWT
!-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR /= 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
      READ (LUNIO,'(11(/),15X,A12,1X,A80)',IOSTAT=ERR) FILEW, PATHWT
      IF (ERR /= 0) CALL ERROR(ERRKEY,ERR,FILEIO,12)
      CLOSE (LUNIO)
!-----------------------------------------------------------------------      
!-----------------------------------------------------------------------
! Run MULTI Season with same treatment
!-----------------------------------------------------------------------
      WYEAR = (ICHAR(FILEW(5:5)) - 48)*10 + (ICHAR(FILEW(6:6)) - 48)
      NYEAR = (ICHAR(FILEW(7:7)) - 48)*10 + (ICHAR(FILEW(8:8)) - 48)

      IF (NYEAR == 1 .AND. MULTI > 1) THEN
        WYEAR = MOD((WYEAR + MULTI - 1),100)
        WRITE(FILEW(5:6),'(I2.2)') WYEAR
      ENDIF

      PATHL  = INDEX(PATHWT,BLANK)
      IF (PATHL <= 1) THEN
        FILEWW = FILEW
      ELSE
        FILEWW = PATHWT(1:(PATHL-1)) // FILEW
      ENDIF
      INQUIRE(FILE=FILEWW,EXIST=FEXIST)
      IF (.NOT. FEXIST) THEN
        ErrCode = 29
        CALL WeatherError(CONTROL, ErrCode, FILEWW, 0, YRDOYWY, YREND)
        RETURN
      ENDIF
      
      WSTAT = FILEW(1:8)
      CALL PUT('WEATHER','WSTA',WSTAT)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! If file exist start read data implementation
!-----------------------------------------------------------------------    
      IF (FILEW /= LastFileW .OR. FirstWeatherDay .GT. YRDOY) THEN
        
        EOF = 0
        LNUM = 0
        NRECORDS = 370
        FirstWeatherDay = 0
        LastWeatherDay = 0
        

        INSI  = '-99 '
        XLAT  = -99.
        XLONG = -99.
        XELEV = -99.
        TAV   = -99.
        TAMP  = -99.
        REFHT = -99.
        WINDHT= -99.
        CCO2  = -99.
        
        
        CALL READWEATHER(FILEWW, YRDOY, FirstWeatherDay, LastWeatherDay,
     &                         EOF, LNUM, NRECORDS, ERRYRDOY, ErrCode)
      
        IF(ErrCode .NE. 0) THEN
          CALL ErrorWeatherInput(CONTROL, ErrCode, ERRYRDOY, YREND, 
     &                           FILEWW, LNUM)
          RETURN
        ENDIF

        LastFileW = FILEW
        
        CALL fio%get('WTH', "INSI",INSI)
        CALL fio%get('WTH', "LAT",XLAT)
        IF(XLAT .EQ. -99) CALL fio%get('WTH', "LATITUDE",XLAT) 
        CALL fio%get('WTH', "LONG",XLONG)
        IF(XLONG .EQ. -99) CALL fio%get('WTH', "LONGITUD",XLONG) 
        CALL fio%get('WTH', "ELEV",XELEV)
        CALL fio%get('WTH', "TAV",TAV)
        CALL fio%get('WTH', "AMP",TAMP)
        IF(TAMP .EQ. -99) CALL fio%get('WTH', "TAMP",TAMP)
!       Substitute default values if REFHT or WINDHT are missing.
        CALL fio%get('WTH', "REFHT",REFHT)
        IF (REFHT <= 0.) REFHT = 1.5
        CALL fio%get('WTH', "WNDHT",WINDHT)
        IF (WINDHT <= 0.) WINDHT = 2.0
        CALL fio%get('WTH', "CCO2",CCO2)
        if(CCO2 .EQ. -99) CALL fio%get('WTH', "CO2",CCO2)
        

!       10/27/2005 CHP The checks for TAV and TAMP were being done in the 
!       STEMP routine, overriding this check. STEMP used .LE. instead 
!       of .LT. and the results were very different for some experiments 
!       which do not supply TAV and TAMP (UFMA8301.PNX, CLMO8501.SBX, 
!       NCCL8801.SBX, GALN0201.COX, others).
!       So -- leave LE for now.
!       IF (TAV  .LT. 0.0) THEN       
        IF (TAV  .LE. 0.0) THEN       
          TAV = 20.0
          WRITE(MSG(1),'(A)') 
     &    'Value of TAV, average annual soil temperature, is missing.'
          WRITE(MSG(2),'(A,F5.1,A)') 
     &    'A default value of', TAV,
     &    'ºC is being used for this simulation,' 
          WRITE(MSG(3),'(A)') 'which may produce inaccurate results.'
          CALL WARNING (3, ERRKEY, MSG)
        ENDIF
!       IF (TAMP .LT. 0.0) THEN
        IF (TAMP .LE. 0.0) THEN
          TAMP = 5.0
          WRITE(MSG(1),'(A)') 
     &    'Value of TAMP, amplitude of soil temperature ' //
     &    'function, is missing.'
          WRITE(MSG(2),'(A,F5.1,A)') 
     &    'A default value of', TAMP,
     &    'ºC is being used for this simulation,' 
          WRITE(MSG(3),'(A)') 'which may produce inaccurate results.'
          CALL WARNING (3, ERRKEY, MSG)
        ENDIF
        
      ENDIF
!-----------------------------------------------------------------------

!     Set weather values for initialization to the day before start of simulation
!     unless first weather day == start of simulation day
      YRDOYWY = INCYD(YRSIM, -1)
      CALL fio%get('WTH', YRDOYWY,"DATE",YRDOYW)
      
      IF(YRDOYW > 0) THEN
        CALL fio%get('WTH', YRDOYW,"SRAD",SRAD)
        CALL fio%get('WTH', YRDOYW,"TMAX",TMAX)
        CALL fio%get('WTH', YRDOYW,"TMIN",TMIN)
        CALL fio%get('WTH', YRDOYW,"RAIN",RAIN)
        CALL fio%get('WTH', YRDOYW,"DEWP",TDEW)
        IF(TDEW .EQ. -99) CALL fio%get('WTH', YRDOYW,"TDEW",TDEW)
        CALL fio%get('WTH', YRDOYW,"WIND",WINDSP)
        CALL fio%get('WTH', YRDOYW,"PAR",PAR)
        CALL fio%get('WTH', YRDOYW,"RHUM",RHUM)
        CALL fio%get('WTH', YRDOYW,"VAPR",VAPR)
        IF(VAPR .EQ. -99) CALL fio%get('WTH', YRDOYW,"VPRS",VAPR)
        CALL fio%get('WTH', YRDOYW,"DCO2",DCO2)
        IF(DCO2 .EQ. -99) CALL fio%get('WTH', YRDOYW,"CO2",DCO2)
        CALL fio%get('WTH', YRDOYW,"LNUM",LNUM)
        
        CALL DailyWeatherCheck(CONTROL,
     &    ERRKEY, FILEWW, RAIN, LNUM, RHUM,               !Input
     &    SRAD, TDEW, TMAX, TMIN, WINDSP, YRDOYW,         !Input
     &    YREND)                                          !Output
     
      ELSE
        MSG(1) = "Error in weather data on day before " // 
     &      "start of simulation date."
        MSG(2) = "Will attempt to intitialize with data " // 
     &      "for start of simulation date."
        CALL WARNING(2,ERRKEY,MSG)
     
        CALL fio%get('WTH', YRSIM,"SRAD",SRAD)
        CALL fio%get('WTH', YRSIM,"TMAX",TMAX)
        CALL fio%get('WTH', YRSIM,"TMIN",TMIN)
        CALL fio%get('WTH', YRSIM,"RAIN",RAIN)
        CALL fio%get('WTH', YRSIM,"DEWP",TDEW)
        IF(TDEW .EQ. -99) CALL fio%get('WTH', YRSIM,"TDEW",TDEW)
        CALL fio%get('WTH', YRSIM,"WIND",WINDSP)
        CALL fio%get('WTH', YRSIM,"PAR",PAR)
        CALL fio%get('WTH', YRSIM,"RHUM",RHUM)
        CALL fio%get('WTH', YRSIM,"VAPR",VAPR)
        IF(VAPR .EQ. -99) CALL fio%get('WTH', YRSIM,"VPRS",VAPR)
        CALL fio%get('WTH', YRSIM,"DCO2",DCO2)
        IF(DCO2 .EQ. -99) CALL fio%get('WTH', YRSIM,"CO2",DCO2)
        CALL fio%get('WTH', YRSIM,"LNUM",LNUM)
        
        CALL DailyWeatherCheck(CONTROL,
     &    ERRKEY, FILEWW, RAIN, LNUM, RHUM,               !Input
     &    SRAD, TDEW, TMAX, TMIN, WINDSP, YRSIM,         !Input
     &    YREND)                                          !Output
             
      ENDIF

!***********************************************************************
!***********************************************************************
!     RATE (Daily input of weather data)
!***********************************************************************
      ELSEIF (DYNAMIC == RATE) THEN
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! FlexibleIO
!-----------------------------------------------------------------------
      CALL fio%get('WTH', YRDOY,"DATE",YRDOYW)
      

      IF(YRDOYW > 0)THEN
        
        CALL fio%get('WTH', YRDOYW,"SRAD",SRAD)
        CALL fio%get('WTH', YRDOYW,"TMAX",TMAX)
        CALL fio%get('WTH', YRDOYW,"TMIN",TMIN)
        CALL fio%get('WTH', YRDOYW,"RAIN",RAIN)
        CALL fio%get('WTH', YRDOYW,"DEWP",TDEW)
        IF(TDEW .EQ. -99) CALL fio%get('WTH', YRDOYW,"TDEW",TDEW)
        CALL fio%get('WTH', YRDOYW,"WIND",WINDSP)
        CALL fio%get('WTH', YRDOYW,"PAR",PAR)
        CALL fio%get('WTH', YRDOYW,"RHUM",RHUM)
        CALL fio%get('WTH', YRDOYW,"VAPR",VAPR)
        IF(VAPR .EQ. -99) CALL fio%get('WTH', YRDOYW,"VPRS",VAPR)
        CALL fio%get('WTH', YRDOYW,"DCO2",DCO2)
        IF(DCO2 .EQ. -99) CALL fio%get('WTH', YRDOYW,"CO2",DCO2)
        CALL fio%get('WTH', YRDOYW,"LNUM",LNUM)        
        
        CALL DailyWeatherCheck(CONTROL,
     &    ERRKEY, FILEWW, RAIN, LNUM, RHUM,               !Input
     &    SRAD, TDEW, TMAX, TMIN, WINDSP, YRDOYW,         !Input
     &    YREND)                                          !Output
                
      ELSE IF(EOF .EQ. 1)THEN
        CALL YR_DOY(YRDOY, YEAR, DOY)
        WRITE(FILEW(5:6),'(I2.2)') MOD(YEAR,100)

        PATHL  = INDEX(PATHWT,BLANK)
        IF (PATHL <= 1) THEN
           FILEWW = FILEW
        ELSE
           FILEWW = PATHWT(1:(PATHL-1)) // FILEW
        ENDIF
        INQUIRE(FILE=FILEWW,EXIST=FEXIST)
        IF (.NOT. FEXIST) THEN
          ErrCode = 29
          CALL WeatherError(CONTROL,ErrCode,FILEWW, 0,YRDOYWY,YREND)
          RETURN
        ENDIF

        WSTAT = FILEW(1:8)
        CALL PUT('WEATHER','WSTA',WSTAT)

        CALL READWEATHER(FILEWW, YRDOY, FirstWeatherDay, 
     &  LastWeatherDay, EOF, LNUM, NRECORDS, ERRYRDOY, ErrCode)
            
        IF(ErrCode .NE. 0) THEN
          CALL ErrorWeatherInput(CONTROL, ErrCode, ERRYRDOY, YREND, 
     &                           FILEWW, LNUM)
          RETURN
        ENDIF
          
        LastFileW = FILEW
        
        YRDOYW = YRDOY
              
        CALL fio%get('WTH', YRDOYW,"SRAD",SRAD)
        CALL fio%get('WTH', YRDOYW,"TMAX",TMAX)
        CALL fio%get('WTH', YRDOYW,"TMIN",TMIN)
        CALL fio%get('WTH', YRDOYW,"RAIN",RAIN)
        CALL fio%get('WTH', YRDOYW,"DEWP",TDEW)
        IF(TDEW .EQ. -99) CALL fio%get('WTH', YRDOYW,"TDEW",TDEW)
        CALL fio%get('WTH', YRDOYW,"WIND",WINDSP)
        CALL fio%get('WTH', YRDOYW,"PAR",PAR)
        CALL fio%get('WTH', YRDOYW,"RHUM",RHUM)
        CALL fio%get('WTH', YRDOYW,"VAPR",VAPR)
        IF(VAPR .EQ. -99) CALL fio%get('WTH', YRDOYW,"VPRS",VAPR)
        CALL fio%get('WTH', YRDOYW,"DCO2",DCO2)
        IF(DCO2 .EQ. -99) CALL fio%get('WTH', YRDOYW,"CO2",DCO2)
        CALL fio%get('WTH', YRDOYW,"LNUM",LNUM)        
        
        CALL DailyWeatherCheck(CONTROL,
     &    ERRKEY, FILEWW, RAIN, LNUM, RHUM,               !Input
     &    SRAD, TDEW, TMAX, TMIN, WINDSP, YRDOYW,         !Input
     &    YREND)                                          !Output
              
              
      ELSE
          
        CALL READWEATHER(FILEWW, YRDOY, FirstWeatherDay,
     &  LastWeatherDay, EOF, LNUM, NRECORDS, ERRYRDOY, ErrCode)
     
        IF(ErrCode .NE. 0) THEN
          CALL ErrorWeatherInput(CONTROL, ErrCode, ERRYRDOY, YREND, 
     &                           FILEWW, LNUM)
          RETURN
        ENDIF
        
        YRDOYW = YRDOY
          
        CALL fio%get('WTH', YRDOYW,"SRAD",SRAD)
        CALL fio%get('WTH', YRDOYW,"TMAX",TMAX)
        CALL fio%get('WTH', YRDOYW,"TMIN",TMIN)
        CALL fio%get('WTH', YRDOYW,"RAIN",RAIN)
        CALL fio%get('WTH', YRDOYW,"DEWP",TDEW)
        IF(TDEW .EQ. -99) CALL fio%get('WTH', YRDOYW,"TDEW",TDEW)
        CALL fio%get('WTH', YRDOYW,"WIND",WINDSP)
        CALL fio%get('WTH', YRDOYW,"PAR",PAR)
        CALL fio%get('WTH', YRDOYW,"RHUM",RHUM)
        CALL fio%get('WTH', YRDOYW,"VAPR",VAPR)
        IF(VAPR .EQ. -99) CALL fio%get('WTH', YRDOYW,"VPRS",VAPR)
        CALL fio%get('WTH', YRDOYW,"DCO2",DCO2)
        IF(DCO2 .EQ. -99) CALL fio%get('WTH', YRDOYW,"CO2",DCO2)
        CALL fio%get('WTH', YRDOYW,"LNUM",LNUM)        
        
        CALL DailyWeatherCheck(CONTROL,
     &    ERRKEY, FILEWW, RAIN, LNUM, RHUM,               !Input
     &    SRAD, TDEW, TMAX, TMIN, WINDSP, YRDOYW,         !Input
     &    YREND)                                          !Output
          
      ENDIF
      
!-----------------------------------------------------------------------

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
    !  INQUIRE(UNIT=LUNWTH,OPENED=FEXIST)

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
! CheckDataWeather, Subroutine, C.H. Porter, 09/01/2009
! Checks daily weather data.

!-----------------------------------------------------------------------
! REVISION HISTORY
! 09/01/2009 CHP Written
! 08/07/2018 WP/FO Updated check weather data for FlexibleIO
!-----------------------------------------------------------------------
      SUBROUTINE CheckDataWeather(CONTROL, ERRKEY, FILEWW, LNUM,   !Input
     &            YREND,
     &            SRAD, TMAX, TMIN, RAIN, TDEW, WINDSP, PAR, RHUM, !Input
     &            VAPR, DCO2)                                      !Input

      USE ModuleDefs
      USE ModuleData
      IMPLICIT None

      CHARACTER*78 MSG(10)
      CHARACTER*(*) ERRKEY, FILEWW
      INTEGER ErrCode, LNUM, YRDOY, YREND, NChar, RecNum, YRDOYWY
      REAL RAIN, RHUM, SRAD, TDEW, TMAX, TMIN, WINDSP
      REAL PAR, VAPR, DCO2
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
     &                  RecNum, YRDOYWY, YREND)
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
!     MJ, 2007-04-05: set TDEW to TMIN if TDEW not otherwise available.  This is not
!     appropriate to South African (and presumably other) conditions
!     --> suggest replacing with a better calculation based on relative humidity, if
!         available.
      IF (TDEW <= -90.)  THEN 
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

      RETURN
      END SUBROUTINE CheckDataWeather

!=======================================================================

!=======================================================================
! ErrorWeatherInput, Subroutine, C.H. Porter, 09/01/2009
! Subroutine for errors found in weather data using FlexibleIO.

!-----------------------------------------------------------------------
! REVISION HISTORY
! 09/01/2009 CHP Written
! 08/07/2018 WP/FO Updated error subroutine for FlexibleIO
!-----------------------------------------------------------------------
      SUBROUTINE ErrorWeatherInput(CONTROL, ErrCode, ERRYRDOY, YREND, 
     &                             FILEWW, LNUM)

      USE ModuleDefs
      USE ModuleData
      IMPLICIT NONE

      CHARACTER*6, PARAMETER :: ERRKEY = 'IPWTH '
      CHARACTER*78 MSG(4)
      CHARACTER*92 FILEWW
      CHARACTER*15 ERRYRDOY
      
      INTEGER ErrCode
      INTEGER YREND
      INTEGER LNUM
      
      TYPE (ControlType) CONTROL

!-----------------------------------------------------------------------
!     Select Weather error message
      SELECT CASE(ErrCode)
        CASE (2); WRITE(MSG(1),'(2A)') 
     &      "Missing day in weather data file: ", ERRYRDOY
        CASE (8); WRITE(MSG(1),'(2A)') 
     &      "Non-sequential data in file: ", ERRYRDOY
        CASE (10); WRITE(MSG(1),'(2A)')
     &      "Weather record not found for YR DOY: ", ERRYRDOY
        CASE (11); MSG(1)="Header section not found in weather file."
        CASE (29); MSG(1) = "Weather file not found."
        CASE (30); MSG(1) = "Error opening Weather file."
        CASE (59); WRITE(MSG(1),'(2A)')
     &      "Invalid format in Weather file: ",ERRYRDOY
        CASE (64); MSG(1) = "Syntax error in Weather file."

      END SELECT

!     Weather file location
      WRITE(MSG(2),'(2X,A)') FILEWW(:76)
      WRITE(MSG(3),'(2X,A,I4)') "Line: ", LNUM
      
      MSG(4) = "Simulation will end."
      
      YREND = CONTROL % YRDOY
      CONTROL % ErrCode = ErrCode
      CALL PUT(CONTROL)
      
!     Warning for these errors:
      CALL WARNING(4,ERRKEY,MSG)
!     Stop the run for these errors:
!      CALL ERROR(ERRKEY,ErrCode,FILEWW,LNUM)
      
      RETURN
      END SUBROUTINE ErrorWeatherInput
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
     &        'PAR','RHUM','VAPR','VPRS','DCO2','CO2')
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
      YREND = CONTROL%YRDOY
      CONTROL % ErrCode = ErrCode
      CALL PUT(CONTROL)
      CALL WARNING(NMSG,ERRKEY,MSG)

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

      SUBROUTINE WeatherErrorC(ErrCode, FILEWW, LNUM,
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
      CALL Get_CONTROL(CONTROL)
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
      END SUBROUTINE WeatherErrorC
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
!           temperatures (°C)
! TAV     Average annual soil temperature, used with TAMP to calculate soil 
!           temperature. (°C)
! TDEW    Dewpoint temperature (°C)
! TIMDIF  Integer function which calculates the number of days between two 
!           Julian dates (da)
! TMAX    Maximum daily temperature (°C)
! TMIN    Minimum daily temperature (°C)
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
