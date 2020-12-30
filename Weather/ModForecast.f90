Module Forecast
  USE ModuleDefs

  Type DailyWeatherType
    INTEGER YRDOY
    REAL RAIN, SRAD, TMAX, TMIN, PAR
    REAL DCO2, OZON7, RHUM, TDEW, VAPR, WINDSP
  End Type
  TYPE (DailyWeatherType), Allocatable :: Obs_data(:)

  INTEGER FCOUNT, FODAT, FSTART

! Track years of ensemble weather data
  INTEGER EnsYearFirst, EnsYearLast, EnsYearCurrent

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
  CHARACTER*80 PATHWTC, PATHWTG, PATHWTW
  CHARACTER*92 FILEWW

  INTEGER DOY, I, Obs_YRDOY, RSEED1, YREND, YR, YRSIM
  INTEGER INCDAT, TIMDIF, FYRDOY, FYRSIM, FCOUNT
  REAL CCO2, DCO2, OZON7, PAR, RAIN, REFHT, RHUM
  REAL SRAD, TAMP, TAV, TDEW, TMAX, TMIN, VAPR, WINDHT
  REAL WINDSP, XELEV, XLAT, XLONG
  TYPE (ControlType) CONTROL, CONTROL2

  EXTERNAL IPWTH, YR_DOY

  CALL GET(CONTROL)
  FODAT = CONTROL % FODAT
  YRSIM = CONTROL % YRSIM
  CALL YR_DOY(YRSIM,YR,DOY)

! =======================================================================
  SELECT CASE (CONTROL % ENDYRS)
! =======================================================================
  CASE (1)
! ENDYRS = 1: Initialize and store forecast data

! FODAT = date of forecast. Weather prior to this date come from observations. 
!   On this date and after, they come from ensemble.
! FCOUNT = number of days of "observed weather" to be used in forecast. 
  FSTART = YRSIM
  FCOUNT = TIMDIF(FSTART, FODAT)

! Initialize weather file for this year's data (i.e., pre-forecast date)
  CONTROL2 = CONTROL
  CONTROL2 % DYNAMIC = SEASINIT
  Obs_YRDOY = INCDAT(FSTART, -1)
  CONTROL2 % YRDOY = Obs_YRDOY

! Determine the first and last years of the weather ensemble using simulation start date as the reference.
! These physical years are needed for historical ensemble, but not for generated weather
! First year of historical ensemble is NYRS before start of simulation date
  EnsYearFirst = YR - CONTROL % NYRS
! Last year of historical ensemble is one year before start of simulation date
  EnsYearLast = YR - 1
! Start with first year of ensemble
  EnsYearCurrent = EnsYearFirst
  FYRDOY = EnsYearFirst *1000 + DOY
  FYRSIM = FYRDOY

! IF FODAT is before or equal to YRSIM, then all weather data are from ensembles.
  IF (FCOUNT .LT. 1) RETURN

  IF (ALLOCATED(Obs_data)) THEN
    DEALLOCATE(Obs_data)
  ENDIF

! Allocate the array size for observed weather data
  Allocate (Obs_data(0:FCOUNT))

! =======================================================================
! Initialize IPWTH for forecast year
  CALL IPWTH(CONTROL2, ERRKEY,                        &
        CCO2, DCO2, FILEW, FILEWC, FILEWG, FILEWW,    &    !Output
        MEWTH, OZON7, PAR,                            &    !Output
        PATHWTC, PATHWTG, PATHWTW,                    &    !Output
        RAIN, REFHT, RHUM, RSEED1, SRAD,              &    !Output
        TAMP, TAV, TDEW, TMAX, TMIN, VAPR, WINDHT,    &    !Output
        WINDSP, XELEV, XLAT, XLONG, YREND,            &    !Output
        SEASINIT)                                           

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

! =======================================================================
! Get and store weather data between YRSIM and FODAT-1
  CONTROL2 = CONTROL
  CONTROL2 % DYNAMIC = RATE

! IPWTH was already initialized for the forecast year. Go ahead and retrieve 
!   data for this year and store for retrieval at the beginning of each 
!   ensemble.
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
    Obs_data(0) % TDEW   = TDEW
    Obs_data(0) % WINDSP = WINDSP
    Obs_data(0) % RHUM   = RHUM
    Obs_data(0) % VAPR   = VAPR
    Obs_data(0) % DCO2   = DCO2
    Obs_data(0) % OZON7  = OZON7
  ENDDO

! =======================================================================
  CASE DEFAULT  ! CONTROL % ENDYRS > 1
! Data has already been stored. Just need to update YRSIM for this ensemble year
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
  INTEGER DOY, YR, FYRDOY
  INTEGER WDATE, I, TIMDIF
  REAL RAIN, TMAX, TMIN, SRAD, PAR
  REAL DCO2, OZON7, RHUM, TDEW, VAPR, WINDSP
  TYPE (ControlType) CONTROL
  EXTERNAL :: YR_DOY, TIMDIF

! FYRDOY = 0 - Use observed data (retrieve from array below).
! FYRDOY > 0 - This is the date to use for the historical ensemble.

! Current date is after forecast date, use ensemble weather data
  IF (WDATE .GE. FODAT) THEN
    CALL GET(CONTROL)
    CALL YR_DOY(WDATE, YR, DOY)
    EnsYearCurrent = EnsYearFirst + CONTROL % ENDYRS - 1
    FYRDOY = EnsYearCurrent * 1000 + DOY

  ELSE
!   Use observed weather data. 
    I = TIMDIF(FSTART, WDATE) + 1
    IF (WDATE .NE. Obs_data(I) % YRDOY) THEN
      PRINT *, WDATE, Obs_data(I) % YRDOY, "Error in forecast weather data"
    ENDIF
    
    RAIN   = Obs_data(I) % RAIN
    SRAD   = Obs_data(I) % SRAD
    TMAX   = Obs_data(I) % TMAX
    TMIN   = Obs_data(I) % TMIN
    PAR    = Obs_data(I) % PAR
    TDEW   = Obs_data(0) % TDEW
    WINDSP = Obs_data(0) % WINDSP
    RHUM   = Obs_data(0) % RHUM
    VAPR   = Obs_data(0) % VAPR
    DCO2   = Obs_data(0) % DCO2
    OZON7  = Obs_data(0) % OZON7
    FYRDOY = 0
  ENDIF

  RETURN
END SUBROUTINE FCAST_RETRIEVE

!========================================================================
SUBROUTINE FCAST_FINISH()
  DEALLOCATE (Obs_data)
  RETURN
END SUBROUTINE FCAST_FINISH

!========================================================================
End Module Forecast
!========================================================================
