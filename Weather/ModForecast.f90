Module Forecast
  USE ModuleDefs

! 2020-10-30 chp 
! change gears - use weather generator for historical ensemble as a first cut.
!   later we will introduce weather ensemble read from files.

!  Type DailyWeatherType
!    INTEGER YRDOY
!    REAL RAIN, SRAD, TMAX, TMIN
!  End Type
!  TYPE (DailyWeatherType), Allocatable :: Obs_data(:)

  INTEGER COUNT, FODAT, FSTART

! Track years of ensemble weather data
  INTEGER EnsYearFirst, EnsYearLast, EnsYearCurrent


CONTAINS

!========================================================================
SUBROUTINE FCAST_COUNT(NYRS, YRSIM, FODAT)
! Determine start and end of forecast season (i.e., prior to ensemble)
!   and the first and last years for the historical ensemble weather years

  INTEGER, INTENT(IN)  :: NYRS
  INTEGER, INTENT(IN)  :: YRSIM
  INTEGER, INTENT(OUT) :: FODAT

  INTEGER COUNT, DOY, TIMDIF, YR

! FODAT = date of forecast. Weather prior to this date come from observations. After, they come from ensemble.
! COUNT = number of days of "observed weather" to be used in forecast. 
  FSTART = YRSIM
  COUNT = TIMDIF(FODAT,FSTART)

! IF FODAT is before or equal to YRSIM, then all weather data are from ensembles.
  IF (COUNT .LT. 1) RETURN

!! Allocate the array size for observed weather data
!  Allocate (Obs_data(COUNT))

! Determine the first and last years of the weather ensemble using simulation start date as the reference.
  CALL YR_DOY(YRSIM,YR,DOY)
! First year of historical ensemble is NYRS before start of simulation date
  EnsYearFirst = YR - NYRS
! Last year of historical ensemble is one year before start of simulation date
  EnsYearLast = YR - 1
! Start with first year of ensemble
  EnsYearCurrent = EnsYearFirst

  RETURN
END SUBROUTINE FCAST_COUNT

!!========================================================================
!SUBROUTINE FCAST_STORE(SUCCESS)
!  USE flexibleio
!  INTEGER COUNT, I, INCDAT, Obs_YRDOY
!  LOGICAL SUCCESS
!
!  SUCCESS = .TRUE.
!! get weather data 
!  DO I = 1, COUNT
!    Obs_YRDOY = INCDAT(FSTART, I-1)
!    CALL fio%get('WTH', Obs_YRDOY, "SRAD", Obs_data(I) % SRAD)
!    CALL fio%get('WTH', Obs_YRDOY, "TMAX", Obs_data(I) % TMAX)
!    CALL fio%get('WTH', Obs_YRDOY, "TMIN", Obs_data(I) % TMIN)
!    CALL fio%get('WTH', Obs_YRDOY, "RAIN", Obs_data(I) % RAIN)
!!   CALL fio%get('WTH', Obs_YRDOY, "LNUM", LNUM)
!  ENDDO
!
!  RETURN
!END SUBROUTINE FCAST_STORE
!
!!========================================================================
!SUBROUTINE FCAST_RETRIEVE(WDATE, RAIN, TMAX, TMIN, SRAD, FCODE)
!  INTEGER WDATE, FCODE, I, TIMDIF
!  REAL RAIN, TMAX, TMIN, SRAD
!
!! FCODE = 0 - No observed data for this date, use ensemble data
!! FCODE = 1 - Use observed data, return values.
!
!  FCODE = 0
!  RAIN = -99
!  SRAD = -99
!  TMAX = -99
!  TMIN = -99
!
!! No forecast weather data
!  IF (COUNT .LE. 0) RETURN
!
!! Current date is after forecast date
!  IF (WDATE .GE. FODAT) RETURN
!
!! Find today's forecast weather data
!  I = TIMDIF(WDATE, FSTART) + 1
!  RAIN = Obs_data(I) % RAIN
!  SRAD = Obs_data(I) % SRAD
!  TMAX = Obs_data(I) % TMAX
!  TMIN = Obs_data(I) % TMIN
!  FCODE = 1
!
!  RETURN
!END SUBROUTINE FCAST_RETRIEVE
!
!========================================================================
End Module Forecast
!========================================================================
