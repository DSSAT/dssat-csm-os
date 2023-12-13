Module Forecast
  USE ModuleDefs

  Type DailyWeatherType
    INTEGER YRDOY
    REAL RAIN, SRAD, TMAX, TMIN, PAR
    REAL DCO2, OZON7, RHUM, TDEW, VAPR, WINDSP
  End Type
  TYPE (DailyWeatherType), Allocatable :: Obs_data(:)

  INTEGER FCOUNT, FODAT, FSTART, ForecastDate

! Track years of ensemble weather data
  INTEGER EnsYearFirst, EnsYearLast, EnsYearCurrent, ForecastYear
! Weather data from historical file

! Indicates that start of sim date = FODAT > last weather date
! Entire simulation is forecast, we have no in-season weather data.
  LOGICAL Preseason
  

CONTAINS

!========================================================================
! Subroutine FCAST_STORE

! Determine start and end of forecast season (i.e., prior to ensemble)
!   and the first and last years for the historical ensemble weather years
! Store the forecast year weather data plus data from short term forecast file. 
!   These will be used in every year of the forecast ensemble. 

SUBROUTINE FCAST_STORE(                                 &  
     CCO2, DCO2, FILEW, FILEWC, FILEWG, FILEWW,         &  !Output
     FYRDOY, FYRSIM, MEWTH, OZON7, PATHWTC, PATHWTG,    &  !Output
     PATHWTW,REFHT, RHUM, RSEED1, TAMP, TAV, TDEW,      &  !Output
     VAPR, WINDHT, WINDSP, XELEV, XLAT, XLONG, YREND)      !Output

  USE ModuleData
  IMPLICIT NONE

  CHARACTER*1  MEWTH
  CHARACTER*6, PARAMETER :: ERRKEY = "FORCST"
  CHARACTER*12 FILEW, FILEWC, FILEWG
  CHARACTER*78 MSG(10)
  CHARACTER*80 PATHWTC, PATHWTG, PATHWTW
  CHARACTER*92 FILEWW

  INTEGER DOY, I, Obs_YRDOY, RSEED1, YREND, YR, YRSIM
  INTEGER INCDAT, TIMDIF, FYRDOY, FYRSIM
  REAL CCO2, DCO2, OZON7, PAR, RAIN, REFHT, RHUM
  REAL SRAD, TAMP, TAV, TDEW, TMAX, TMIN, VAPR, WINDHT
  REAL WINDSP, XELEV, XLAT, XLONG
  TYPE (ControlType) CONTROL, CONTROL2

  EXTERNAL IPWTH, YR_DOY, INCDAT, WARNING, TIMDIF, ERROR

  CALL GET(CONTROL)
  YRSIM = CONTROL % YRSIM
  CALL YR_DOY(YRSIM,YR,DOY)

! ----------------------------------------------------------------------
  SELECT CASE (CONTROL % ENDYRS)
! ----------------------------------------------------------------------
  CASE (1)
!   ENDYRS = 1: Initialize and store in-season (i.e., pre-forecast) data
    FODAT = CONTROL % FODAT
    CONTROL2 = CONTROL
    CONTROL2 % DYNAMIC = SEASINIT
    Obs_YRDOY = INCDAT(YRSIM, -1)
    CONTROL2 % YRDOY = Obs_YRDOY
! ----------------------------------------------------------------------
!   Initialize weather data for YRSIM, forecast year
    CALL IPWTH(CONTROL2, ERRKEY,                        &
        CCO2, DCO2, FILEW, FILEWC, FILEWG, FILEWW,    &    !Output
        MEWTH, OZON7, PAR,                            &    !Output
        PATHWTC, PATHWTG, PATHWTW,                    &    !Output
        RAIN, REFHT, RHUM, RSEED1, SRAD,              &    !Output
        TAMP, TAV, TDEW, TMAX, TMIN, VAPR, WINDHT,    &    !Output
        WINDSP, XELEV, XLAT, XLONG, YREND,            &    !Output
        SEASINIT)

!   FCAST_ScanWeathData was called from IPWTH so now we know
!   the starting and ending dates for weather in a long weather file.
                                           
    IF (YREND .EQ. Obs_YRDOY) THEN
      MSG(1) = "In-season observed weather data not found."
!     MSG(2) = "Program will stop."
      MSG(2) = "No in-season weather data will be used in the forecast."
      CALL WARNING(2, ERRKEY, MSG)
!     CALL ERROR(ERRKEY,CONTROL%ERRCODE,"",0)
    ENDIF
    
!   ENDYRS = 1: Initialize and store forecast data
!   FODAT = date of forecast. Weather prior to this date come from observations. 
!     On this date and after, they come from ensemble.
!   FCOUNT = number of days of "observed weather" to be used in forecast. 
    FSTART = YRSIM
    FCOUNT = TIMDIF(FSTART, ForecastDate)

!   Check for FODAT < FSTART
    IF (FCOUNT < 0) THEN
      MSG(1) = "Forecast date is less than start of simulation date."
      MSG(2) = "Please check your input data. Program will stop."
      CALL WARNING(2, ERRKEY, MSG)
      CALL ERROR(ERRKEY,1,"",0)
    ENDIF

!   Determine the first and last years of the weather ensemble using simulation start date as the reference.
!   These physical years are needed for historical ensemble, but not for generated weather
!   First year of historical ensemble is NYRS before start of simulation date
    EnsYearFirst = YR - CONTROL % NYRS
!   Last year of historical ensemble is one year before start of simulation date
    EnsYearLast = YR - 1
!   Start with first year of ensemble
    EnsYearCurrent = EnsYearFirst
    FYRDOY = EnsYearFirst *1000 + DOY
    FYRSIM = FYRDOY
    ForecastYear = YR

!   IF FODAT is before or equal to YRSIM, then all weather data are from ensembles.
    IF (FCOUNT .LT. 1) RETURN

!   Deallocate the array space for stored weather data if necessary
    IF (ALLOCATED(Obs_data)) THEN
      DEALLOCATE(Obs_data)
    ENDIF

!   Allocate the array size for observed weather data
    Allocate (Obs_data(0:FCOUNT))

!   Initialize to -99s
    Obs_data % YRDOY  = -99
    Obs_data % RAIN   = -99.
    Obs_data % SRAD   = -99.
    Obs_data % TMAX   = -99.
    Obs_data % TMIN   = -99.
    Obs_data % PAR    = -99.
    Obs_data % DCO2   = -99.
    Obs_data % OZON7  = -99.
    Obs_data % RHUM   = -99.
    Obs_data % TDEW   = -99.
    Obs_data % VAPR   = -99.
    Obs_data % WINDSP = -99.

!   Store the initial weather data here.
    Obs_data(0) % YRDOY  = Obs_YRDOY
    Obs_data(0) % SRAD   = SRAD
    Obs_data(0) % TMAX   = TMAX
    Obs_data(0) % TMIN   = TMIN
    Obs_data(0) % RAIN   = RAIN
    Obs_data(0) % PAR    = PAR
    Obs_data(0) % TDEW   = TDEW
    Obs_data(0) % WINDSP = WINDSP
    Obs_data(0) % RHUM   = RHUM
    Obs_data(0) % VAPR   = VAPR
    Obs_data(0) % DCO2   = DCO2
    Obs_data(0) % OZON7  = OZON7

! ----------------------------------------------------------------------
!   Get and store weather data between YRSIM and FODAT-1
    CONTROL2 = CONTROL
    CONTROL2 % DYNAMIC = RATE
    
!   IPWTH was already initialized for the forecast year. Go ahead and retrieve 
!     data for this year and store for retrieval at the beginning of each 
!     ensemble.
    DO I = 1, FCOUNT
      Obs_YRDOY = INCDAT(FSTART, I-1)
      CONTROL2 % YRDOY = Obs_YRDOY
    
      CALL IPWTH(CONTROL2, ERRKEY,                      &
          CCO2, DCO2, FILEW, FILEWC, FILEWG, FILEWW,    &    !Output
          MEWTH, OZON7, PAR,                            &    !Output
          PATHWTC, PATHWTG, PATHWTW,                    &    !Output
          RAIN, REFHT, RHUM, RSEED1, SRAD,              &    !Output
          TAMP, TAV, TDEW, TMAX, TMIN, VAPR, WINDHT,    &    !Output
          WINDSP, XELEV, XLAT, XLONG, YREND,            &    !Output
          RATE)                                           
    
      Obs_data(I) % YRDOY  = Obs_YRDOY
      Obs_data(I) % SRAD   = SRAD
      Obs_data(I) % TMAX   = TMAX
      Obs_data(I) % TMIN   = TMIN
      Obs_data(I) % RAIN   = RAIN
      Obs_data(I) % PAR    = PAR
      Obs_data(I) % TDEW   = TDEW
      Obs_data(I) % WINDSP = WINDSP
      Obs_data(I) % RHUM   = RHUM
      Obs_data(I) % VAPR   = VAPR
      Obs_data(I) % DCO2   = DCO2
      Obs_data(I) % OZON7  = OZON7
    ENDDO

! ----------------------------------------------------------------------
  CASE DEFAULT  ! CONTROL % ENDYRS > 1
!   Data has already been stored. Just need to update YRSIM for this ensemble year
    EnsYearCurrent = EnsYearCurrent + 1
    FYRDOY = EnsYearCurrent * 1000 + DOY
    FYRSIM = FYRDOY
    RETURN
  END SELECT

  RETURN
END SUBROUTINE FCAST_STORE

!========================================================================
SUBROUTINE FCAST_RETRIEVE(WDATE,        &   !Input
        DCO2, FYRDOY, OZON7, PAR, RAIN, &   !Output
        RHUM, TDEW, TMAX, TMIN, VAPR,   &   !Output
        SRAD, WINDSP)                       !Output

  USE ModuleData
  EXTERNAL :: YR_DOY, TIMDIF, INCDAT, WARNING, ERROR
  SAVE
  CHARACTER*6, PARAMETER :: ERRKEY = "FORCST"
  CHARACTER*78 MSG(4)
  INTEGER DOY, YR, FYRDOY, FYRDOY_Y
  INTEGER WDATE, I, TIMDIF, INCDAT
  REAL RAIN, TMAX, TMIN, SRAD, PAR
  REAL DCO2, OZON7, RHUM, TDEW, VAPR, WINDSP
  TYPE (ControlType) CONTROL

! FYRDOY = 0 - Use observed data (retrieve from array below).
! FYRDOY > 0 - This is the date to use for the historical ensemble.

! Current date is after forecast date, use ensemble weather data
  IF (WDATE .GE. ForecastDate .OR. FCOUNT .LT. 1) THEN
    CALL GET(CONTROL)
    CALL YR_DOY(WDATE, YR, DOY)
    IF (WDATE == ForecastDate) THEN
      EnsYearCurrent = EnsYearFirst + CONTROL % ENDYRS - 1
      FYRDOY = EnsYearCurrent * 1000 + DOY
    ELSE
      FYRDOY = INCDAT(FYRDOY_Y, 1)
    ENDIF
    FYRDOY_Y = FYRDOY

  ELSE
!   Use observed weather data. 
    I = TIMDIF(FSTART, WDATE) + 1
    IF (WDATE .NE. Obs_data(I) % YRDOY) THEN
      WRITE(MSG(1),'(A)') "Error in forecast weather data, possible non-consecutive dates."
      WRITE(MSG(2),'(A,I10)') "Simulation date:", WDATE 
      WRITE(MSG(3),'(A,I10)') "Observed weather date:", Obs_data(I) % YRDOY
      WRITE(MSG(4),'(A)') "Program will stop."
      CALL WARNING(4, ERRKEY, MSG)
      CALL ERROR(ERRKEY,CONTROL%ERRCODE,"",0)
    ENDIF
    
    RAIN   = Obs_data(I) % RAIN
    SRAD   = Obs_data(I) % SRAD
    TMAX   = Obs_data(I) % TMAX
    TMIN   = Obs_data(I) % TMIN
    PAR    = Obs_data(I) % PAR
    TDEW   = Obs_data(I) % TDEW
    WINDSP = Obs_data(I) % WINDSP
    RHUM   = Obs_data(I) % RHUM
    VAPR   = Obs_data(I) % VAPR
    DCO2   = Obs_data(I) % DCO2
    OZON7  = Obs_data(I) % OZON7
    FYRDOY = 0
  ENDIF

  RETURN
END SUBROUTINE FCAST_RETRIEVE

!========================================================================
SUBROUTINE FCAST_FINISH()
  IF (ALLOCATED(Obs_data)) THEN
    DEALLOCATE(Obs_data)
  ENDIF
  RETURN
END SUBROUTINE FCAST_FINISH

!========================================================================
SUBROUTINE FCAST_ScanWeathData(CONTROL, FileW, LunWth, CenturyFirst) 

!Scan the historic weather data to get starting and ending dates.
!Set the Century for the first weather data - important for 2-digit years.

  USE ModuleDefs
  USE ModuleData
  EXTERNAL IGNORE2, INCDAT, TIMDIF, WARNING, WeatherError, YR_DOY

  CHARACTER*6, PARAMETER :: ERRKEY = "FORCST"
  CHARACTER*78,DIMENSION(10) :: MSG
  CHARACTER*92 FileW
  CHARACTER*120 LINE
  Integer ERR, ErrCode, ISECT, LINWTH, LUNWTH, NMSG
  integer CenturyForecast, CenturyFirst, I, NYears, NDays, NRecords
  integer WRecDate, WeathRecFirst, WeathRecLast, WFirstDate, WFirstYear, WLastYear, WLastDOY
  integer SCENTURY
  REAL NWeathYears
  Type (ControlType) CONTROL
  INTEGER :: INCDAT , TIMDIF !FUNCTION

  ErrCode = 0

!--------------------------------------------------------------
  OPEN (LUNWTH,FILE=FILEW,STATUS='OLD',IOSTAT=ERR)
  REWIND(LUNWTH)
  IF (ERR /= 0) THEN
    ErrCode = 29
    CALL WeatherError(CONTROL, ErrCode, FILEW, 0, 0, 0)
    RETURN
  ENDIF

! Find 2nd header line
  DO I = 1, 2
! Look for header line beginning with '@' in column 1 (ISECT = 3)
    DO WHILE (.TRUE.)   !.NOT. EOF(LUNWTH)
      CALL IGNORE2 (LUNWTH, LINWTH, ISECT, LINE)
      SELECT CASE(ISECT)
        CASE(0);            !End of file
          ErrCode = 59
          CALL WeatherError(CONTROL, ErrCode, FILEW, 0, 0, 0)
          RETURN          
        CASE(1); CYCLE      !Data line
        CASE(2); CYCLE      !New section ("*" found) 
        CASE(3); EXIT       !Header line ("@" found)
      END SELECT
    ENDDO  !Loop thru data file one record at a time
  ENDDO    !Loop thru 1st and 2nd headers

  I = 0
! Read weather dates
  DO WHILE (.TRUE.)   !.NOT. EOF(LUNWTH)
    CALL IGNORE2 (LUNWTH, LINWTH, ISECT, LINE)
    SELECT CASE(ISECT)
      CASE(0); EXIT       !End of file
      CASE(2); EXIT       !New section ("*" found) 
      CASE(3); EXIT       !Header line ("@" found)
      CASE(1)             !Data line found
        I = I + 1
        READ (LINE,'(I7)') WRecDate
        IF (I .EQ. 1) WeathRecFirst = WRecDate
    END SELECT
  ENDDO  !Loop thru data file one record at a time
  CLOSE (LUNWTH)

  WeathRecLast = WRecDate
  NRecords = I
  NWeathYears = NRecords / 365.25

!--------------------------------------------------------------
! Check for missing FODAT. Set it equal to last weather record. 
  IF (FODAT .EQ. -99) THEN
    FODAT = INCDAT(WeathRecLast,1)
!   Check for 2-digit year, use century of YRSIM.
    IF (FODAT .LE. 99365) THEN
      SCENTURY = AINT(FLOAT(CONTROL%YRSIM)/100000.)
      FODAT = FODAT + SCENTURY * 100000
      !IF (TIMDIF(CONTROL%YRSIM,FODAT) < 0) THEN
      !  SCENTURY = SCENTURY + 1
      !  FODAT = FODAT + SCENTURY * 100000
      !ENDIF
      !NMSG = NMSG + 1
      !WRITE(MSG(NMSG),'("Forecast date = ",I8)') FODAT
    ENDIF

    MSG(1) = "Forecast date is missing."
    MSG(2) = "Set forecast date to last weather record."
    WRITE(MSG(3),'("Forecast date = ",I8)') FODAT
    NMSG = 3

!   Check for FODAT >= YRSIM
    IF (TIMDIF(CONTROL%YRSIM,FODAT) < 0) THEN
      FODAT = CONTROL % YRSIM
      NMSG = NMSG + 1
      MSG(NMSG) = "Pre-season forecast."
      Preseason = .TRUE.
      MSG(NMSG+1)="Start of simulation date is after last weather date."
      MSG(NMSG+2)="Set forecast date to start of simulation date."
      NMSG = NMSG + 3
      WRITE(MSG(NMSG),'("Forecast date = ",I8)') FODAT
      CALL WARNING(NMSG, ERRKEY, MSG)
    ENDIF
  ENDIF
  ForecastDate = FODAT
   
!--------------------------------------------------------------
  IF (WeathRecLast .LE. 99365) THEN
!   2-digit years, calculate first weather date based on FODAT and # records 
!   First assume last weather record has the same century as the FODAT
    CenturyForecast = AINT(FLOAT(FODAT) / 100000.)
    WeathRecLast = CenturyForecast * 100000 + WeathRecLast
!!   If the last weather date is before FODAT, then need to incrment the century
!    DO WHILE (WeathRecLast .LT. FODAT)
!      WeathRecLast = WeathRecLast + 100000
!    ENDDO
    
!   Now we have the correct century for the last weather date,
!     calculate first weather date based on number of records
    CALL YR_DOY(WeathRecLast, WLastYear, WLastDOY)
    NYears = AINT(NWeathYears)
    NDays = (NWeathYears - NYears)*365.25
    WFirstYear = WLastYear - NYears
    
    WFirstDate = WFirstYear * 1000 + WLastDOY
    WFirstDate = INCDAT(WFirstDate, -Ndays+1)
    
!   One possible failure mode is when the first weather date is near 
!   the beginning or end of a century. Because the number of leap days
!   is uncertain, there may be a mismatch on first date and therefore
!   first century. Probably need to check the DOY. If it matches, then
!   we're OK. If not, then maybe adjust WFirstDate to match WeathRecFirst
!   Below is the beginning of something to handle that.
!    CALL YR_DOY(WeathRecFirst, YR0, DOY0)
!    CALL YR_DOY(WFirstDate, YR1, DOY1)
!    IF (DOY1 - DOY0 .GT. 300) THEN

!CONCLUSION:  !!!!!
! MUST use 4-digit weather data to guarantee that weather forecasting
! works when weather data crosses a century boundary or has more than 
! 100 years.

  ELSE
    WFirstDate = WeathRecFirst
  ENDIF
  
!--------------------------------------------------------------
  CenturyFirst = AINT(FLOAT(WFirstDate) / 100000.)

  RETURN
END SUBROUTINE FCAST_ScanWeathData

!========================================================================
End Module Forecast
!========================================================================
